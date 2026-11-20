package org.intellij.erlang.parser.macro;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.intellij.erlang.ErlangParserDefinition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static org.intellij.erlang.ErlangTypes.*;

/**
 * Spike 3: the macro substitution engine of spikes 1 and 2, turned into a pure function from a file text to an expanded
 * text with a source map. It is never used by the lexer or the parser of a file.
 * <p>
 * Every same-file macro use with exactly one visible definition for its name and arity is replaced by its expansion
 * (the selection rule of the engine, without the "fragments" shape test). A form whose expanded size is over the cap
 * is not expanded at all. Every token of an expansion knows its source: a token of a macro argument at the use, or a
 * token of a {@code -define} body, both as absolute offsets in the same text; tokens made by {@code ??Arg} have none.
 * Text outside the replaced uses is copied as it is (verbatim segments).
 */
public final class ErlangMacroExpander {
  public static final int MAX_DEPTH = 32;
  /** A hard limit of the engine, independent of the form cap. */
  public static final int MAX_TOKENS_PER_FORM = 50_000;

  public static final int SRC_NONE = 0;
  public static final int SRC_ARGUMENT = 1;
  public static final int SRC_BODY = 2;
  public static final int SRC_VERBATIM = 3;

  public static final Set<String> PREDEFINED = Set.of(
    "FILE", "FUNCTION_NAME", "FUNCTION_ARITY", "LINE", "MODULE", "MODULE_STRING", "BASE_MODULE", "BASE_MODULE_STRING",
    "MACHINE", "BEAM", "OTP_RELEASE", "FEATURE_AVAILABLE", "FEATURE_ENABLED");

  /** What happened to a macro use in the text (outside {@code -define} bodies). */
  public enum Outcome {
    SUBSTITUTED, PREDEFINED, NOT_IN_FILE, AMBIGUOUS, ARITY_MISMATCH,
    LIMIT_DEPTH, LIMIT_TOKENS, LIMIT_RECURSION, BAD_ARGUMENTS, FORM_OVER_CAP
  }

  public enum FormStatus {
    /** No same-file macro use was substituted and none was refused. */
    NO_USES,
    EXPANDED,
    /** Same-file uses exist, none substituted, at least one because of more than one visible definition. */
    NOT_EXPANDED_AMBIGUOUS,
    /** Substitutions were possible, but the expanded form is larger than the cap. */
    OVER_CAP,
    /** Same-file uses exist, none substituted, for another reason (limits, arity, bad arguments). */
    NOT_EXPANDED_OTHER
  }

  public static final class Form {
    public final int srcStart;
    public final int srcEnd;
    public int expStart;
    public int expEnd;
    public FormStatus status = FormStatus.NO_USES;
    public int substitutedUses;
    public int expandedTokens;
    /** All substituted uses have a body that is not one complete expression (the "fragments" class of spikes 1, 2). */
    public boolean allFragments = true;

    Form(int srcStart, int srcEnd) {
      this.srcStart = srcStart;
      this.srcEnd = srcEnd;
    }

    public boolean containsSource(int offset) {
      return srcStart <= offset && offset <= srcEnd;
    }

    @Override
    public String toString() {
      return "Form[" + srcStart + ".." + srcEnd + " -> " + expStart + ".." + expEnd + " " + status + "]";
    }
  }

  /** The result: the expanded text, its source map and its forms. */
  public static final class Expansion {
    public final CharSequence sourceText;
    public final String text;
    public final List<Form> forms;
    public final EnumMap<Outcome, Integer> outcomes;
    private final int[] myExpStart, myExpEnd, mySrcStart, mySrcEnd, myKind;
    private final int mySegments;
    private final int[] myVerbatim; // indices of the verbatim segments, sorted by source (and expanded) offset
    private Map<Integer, int[]> myArgumentCopies;

    Expansion(CharSequence sourceText, String text, List<Form> forms, EnumMap<Outcome, Integer> outcomes, Out out) {
      this.sourceText = sourceText;
      this.text = text;
      this.forms = forms;
      this.outcomes = outcomes;
      myExpStart = out.expStart;
      myExpEnd = out.expEnd;
      mySrcStart = out.srcStart;
      mySrcEnd = out.srcEnd;
      myKind = out.kind;
      mySegments = out.n;
      int v = 0;
      for (int i = 0; i < mySegments; i++) if (myKind[i] == SRC_VERBATIM) v++;
      myVerbatim = new int[v];
      v = 0;
      for (int i = 0; i < mySegments; i++) if (myKind[i] == SRC_VERBATIM) myVerbatim[v++] = i;
    }

    public boolean hasExpansions() {
      for (Form f : forms) if (f.status == FormStatus.EXPANDED) return true;
      return false;
    }

    public int expandedFormCount() {
      int n = 0;
      for (Form f : forms) if (f.status == FormStatus.EXPANDED) n++;
      return n;
    }

    /** The expanded offset of a source offset that lies in copied (verbatim) text, else -1. */
    public int toExpanded(int srcOffset) {
      int lo = 0, hi = myVerbatim.length - 1;
      while (lo <= hi) {
        int mid = (lo + hi) >>> 1;
        int s = myVerbatim[mid];
        if (srcOffset < mySrcStart[s]) hi = mid - 1;
        else if (srcOffset >= mySrcEnd[s]) lo = mid + 1;
        else return myExpStart[s] + (srcOffset - mySrcStart[s]);
      }
      return -1;
    }

    /** The expanded offsets of every copy of the argument token that starts at srcOffset (empty if none). */
    public int @NotNull [] argumentCopies(int srcOffset) {
      if (myArgumentCopies == null) {
        Map<Integer, int[]> map = new HashMap<>();
        for (int i = 0; i < mySegments; i++) {
          if (myKind[i] != SRC_ARGUMENT) continue;
          int[] old = map.get(mySrcStart[i]);
          int[] now = old == null ? new int[1] : Arrays.copyOf(old, old.length + 1);
          now[now.length - 1] = myExpStart[i];
          map.put(mySrcStart[i], now);
        }
        myArgumentCopies = map;
      }
      int[] r = myArgumentCopies.get(srcOffset);
      return r == null ? new int[0] : r;
    }

    private int segmentAt(int expOffset) {
      int lo = 0, hi = mySegments - 1;
      while (lo <= hi) {
        int mid = (lo + hi) >>> 1;
        if (expOffset < myExpStart[mid]) hi = mid - 1;
        else if (expOffset >= myExpEnd[mid]) lo = mid + 1;
        else return mid;
      }
      return -1;
    }

    /** The source kind of the expanded offset: one of the SRC_ constants, or -1 for a space between tokens. */
    public int kindAt(int expOffset) {
      int s = segmentAt(expOffset);
      return s < 0 ? -1 : myKind[s];
    }

    /**
     * The single source range of an expanded range, or null. The range has a single source when all its tokens come
     * from one kind of source (verbatim text, argument tokens, or {@code -define} body tokens), in source order, with
     * only whitespace between them in the source.
     */
    public @Nullable TextRange toSource(int expStart, int expEnd) {
      if (expEnd <= expStart) {
        int s = segmentAt(expStart);
        if (s < 0 || myKind[s] == SRC_NONE) return null;
        int at = mySrcStart[s] + (expStart - myExpStart[s]);
        return TextRange.create(at, at);
      }
      int first = segmentAt(expStart);
      int last = segmentAt(expEnd - 1);
      if (first < 0 || last < 0) return null;
      int kind = myKind[first];
      if (kind == SRC_NONE) return null;
      for (int i = first + 1; i <= last; i++) {
        if (myKind[i] != kind) return null;
        if (mySrcStart[i] < mySrcEnd[i - 1]) return null;
        for (int c = mySrcEnd[i - 1]; c < mySrcStart[i]; c++) {
          if (!Character.isWhitespace(sourceText.charAt(c))) return null;
        }
      }
      int start = mySrcStart[first] + (expStart - myExpStart[first]);
      int end = mySrcStart[last] + (expEnd - myExpStart[last]);
      if (end > mySrcEnd[last] || start > mySrcEnd[first]) return null;
      return TextRange.create(start, end);
    }

    public @Nullable Form formAtSource(int srcOffset) {
      int lo = 0, hi = forms.size() - 1;
      while (lo <= hi) {
        int mid = (lo + hi) >>> 1;
        Form f = forms.get(mid);
        if (srcOffset < f.srcStart) hi = mid - 1;
        else if (srcOffset > f.srcEnd) lo = mid + 1;
        else return f;
      }
      return null;
    }

    public @Nullable Form formAtExpanded(int expOffset) {
      int lo = 0, hi = forms.size() - 1;
      while (lo <= hi) {
        int mid = (lo + hi) >>> 1;
        Form f = forms.get(mid);
        if (expOffset < f.expStart) hi = mid - 1;
        else if (expOffset > f.expEnd) lo = mid + 1;
        else return f;
      }
      return null;
    }
  }

  /** Segments of the expanded text. */
  static final class Out {
    final StringBuilder text = new StringBuilder();
    int[] expStart = new int[64], expEnd = new int[64], srcStart = new int[64], srcEnd = new int[64], kind = new int[64];
    int n;

    void segment(int es, int ee, int ss, int se, int k) {
      if (k == SRC_VERBATIM && n > 0 && kind[n - 1] == SRC_VERBATIM && expEnd[n - 1] == es && srcEnd[n - 1] == ss) {
        expEnd[n - 1] = ee;
        srcEnd[n - 1] = se;
        return;
      }
      if (n == kind.length) {
        int size = n * 2;
        expStart = Arrays.copyOf(expStart, size);
        expEnd = Arrays.copyOf(expEnd, size);
        srcStart = Arrays.copyOf(srcStart, size);
        srcEnd = Arrays.copyOf(srcEnd, size);
        kind = Arrays.copyOf(kind, size);
      }
      expStart[n] = es;
      expEnd[n] = ee;
      srcStart[n] = ss;
      srcEnd[n] = se;
      kind[n] = k;
      n++;
    }
  }

  private static final TokenSet SKIPPED = TokenSet.orSet(ErlangParserDefinition.COMMENTS, TokenSet.create(TokenType.WHITE_SPACE));
  private static final TokenSet BLOCK_OPENERS = TokenSet.create(ERL_BEGIN, ERL_CASE, ERL_IF, ERL_RECEIVE, ERL_TRY, ERL_MAYBE);
  private static final TokenSet BINARY_ONLY_OPERATORS = TokenSet.create(
    ERL_OP_AR_MUL, ERL_OP_AR_DIV, ERL_DIV, ERL_REM, ERL_BAND, ERL_BOR, ERL_BXOR, ERL_BSL, ERL_BSR, ERL_AND, ERL_OR,
    ERL_XOR, ERL_ANDALSO, ERL_ORELSE, ERL_OP_EQ_EQ, ERL_OP_DIV_EQ, ERL_OP_EQ_LT, ERL_OP_LT, ERL_OP_GT_EQ, ERL_OP_GT,
    ERL_OP_EQ_COL_EQ, ERL_OP_EQ_DIV_EQ, ERL_OP_PLUS_PLUS, ERL_OP_MINUS_MINUS, ERL_OP_EQ, ERL_OP_EXL, ERL_OP_MAYBE_EQ);
  private static final TokenSet ANY_OPERATOR_OR_DANGLING = TokenSet.orSet(BINARY_ONLY_OPERATORS, TokenSet.create(
    ERL_OP_PLUS, ERL_OP_MINUS, ERL_NOT, ERL_BNOT, ERL_CATCH, ERL_RADIX, ERL_COLON, ERL_QMARK));
  private static final TokenSet NOT_IN_EXPRESSION_TOP = TokenSet.create(
    ERL_COMMA, ERL_SEMI, ERL_ARROW, ERL_WHEN, ERL_OP_OR, ERL_OR_OR, ERL_ASSOC, ERL_MATCH, ERL_OP_LT_MINUS,
    ERL_OP_LT_EQ, ERL_OP_LT_COLON_MINUS, ERL_OP_LT_COLON_EQ, ERL_OP_AND_AND, ERL_DOT_DOT, ERL_DOT_DOT_DOT, ERL_OF,
    ERL_AFTER, ERL_END, ERL_ELSE);

  private ErlangMacroExpander() {
  }

  /**
   * Expands the text.
   *
   * @param base    the plain Erlang lexer
   * @param formCap the largest number of significant tokens an expanded form may have; a larger form is not expanded
   */
  public static @NotNull Expansion expand(@NotNull CharSequence text, @NotNull Lexer base, int formCap) {
    Base b = Base.lex(base, text, 0, text.length(), 0);
    Run run = new Run(b, formCap);
    run.run();
    return new Expansion(text, run.out.text.toString(), run.forms, run.outcomes, run.out);
  }

  /** The tokens of the base lexer. */
  private static final class Base {
    IElementType[] type;
    int[] start;
    int[] end;
    int n;
    CharSequence text;

    static Base lex(Lexer lexer, CharSequence buffer, int startOffset, int endOffset, int state) {
      Base b = new Base();
      b.text = buffer;
      int cap = Math.max(16, (endOffset - startOffset) / 3);
      b.type = new IElementType[cap];
      b.start = new int[cap];
      b.end = new int[cap];
      lexer.start(buffer, startOffset, endOffset, state);
      IElementType t;
      while ((t = lexer.getTokenType()) != null) {
        if (b.n == b.type.length) {
          b.type = Arrays.copyOf(b.type, b.n * 2);
          b.start = Arrays.copyOf(b.start, b.n * 2);
          b.end = Arrays.copyOf(b.end, b.n * 2);
        }
        b.type[b.n] = t;
        b.start[b.n] = lexer.getTokenStart();
        b.end[b.n] = lexer.getTokenEnd();
        b.n++;
        lexer.advance();
      }
      return b;
    }

    String text(int i) {
      return text.subSequence(start[i], end[i]).toString();
    }
  }

  /** A token of a macro body, an argument or a result, with its source kind and absolute source range. */
  record Tok(@NotNull IElementType type, @NotNull String text, int kind, int start, int end) {
    Tok(@NotNull IElementType type, @NotNull String text) {
      this(type, text, SRC_NONE, -1, -1);
    }
  }

  static final class Def {
    final String name;
    final int arity; // -1: no parentheses
    final List<String> params;
    final List<Tok> body;
    final String key;
    final boolean fragment;

    Def(String name, int arity, List<String> params, List<Tok> body, boolean fragment) {
      this.name = name;
      this.arity = arity;
      this.params = params;
      this.body = body;
      this.fragment = fragment;
      StringBuilder sb = new StringBuilder().append(arity).append(params);
      for (Tok t : body) sb.append(' ').append(t.text);
      this.key = sb.toString();
    }
  }

  private static final class LimitException extends Exception {
    final Outcome outcome;

    LimitException(Outcome outcome) {
      super(null, null, false, false);
      this.outcome = outcome;
    }
  }

  private record Selection(@Nullable Def def, @NotNull Outcome outcome) {
  }

  private record Use(int q, int last, Def def, List<Tok> result) {
  }

  private static final class Run {
    final Base b;
    final int[] sig;
    final int sigCount;
    final int formCap;
    final Map<String, List<Def>> defs = new HashMap<>();
    final Out out = new Out();
    final List<Form> forms = new ArrayList<>();
    final EnumMap<Outcome, Integer> outcomes = new EnumMap<>(Outcome.class);
    int cursor; // next source offset to copy
    int formSubstitutedTokens;
    int conditionalDepth;

    Run(Base b, int formCap) {
      this.b = b;
      this.formCap = formCap;
      int[] s = new int[b.n];
      int c = 0;
      for (int i = 0; i < b.n; i++) {
        if (!SKIPPED.contains(b.type[i])) s[c++] = i;
      }
      sig = s;
      sigCount = c;
    }

    IElementType st(int p) {
      return p < sigCount ? b.type[sig[p]] : null;
    }

    String stext(int p) {
      return b.text(sig[p]);
    }

    @Nullable Name nameAt(int p, int limit) {
      IElementType t = st(p);
      if (p > limit) return null;
      if (t == ERL_VAR || t == ERL_ATOM_NAME) return new Name(stext(p), p);
      if (t == ERL_SINGLE_QUOTE && p + 2 <= limit && st(p + 1) == ERL_ATOM_NAME && st(p + 2) == ERL_SINGLE_QUOTE) {
        return new Name(stext(p + 1), p + 2);
      }
      return null;
    }

    boolean isFormEnd(int p) {
      if (st(p) != ERL_DOT) return false;
      int raw = sig[p];
      return raw + 1 >= b.n || SKIPPED.contains(b.type[raw + 1]);
    }

    void count(Outcome o) {
      outcomes.merge(o, 1, Integer::sum);
    }

    void flushTo(int srcOffset) {
      if (srcOffset <= cursor) return;
      int es = out.text.length();
      out.text.append(b.text, cursor, srcOffset);
      out.segment(es, out.text.length(), cursor, srcOffset, SRC_VERBATIM);
      cursor = srcOffset;
    }

    void emitTokens(List<Tok> tokens) {
      out.text.append(' ');
      Tok prev = null;
      for (Tok t : tokens) {
        boolean glued = prev != null && prev.kind == t.kind && t.kind != SRC_NONE && prev.end == t.start;
        if (prev != null && !glued) out.text.append(' ');
        int es = out.text.length();
        out.text.append(t.text);
        out.segment(es, out.text.length(), t.start, t.end, t.kind);
        prev = t;
      }
      out.text.append(' ');
    }

    void run() {
      int p = 0;
      while (p < sigCount) {
        int formStart = p;
        int formEnd = p;
        while (formEnd < sigCount - 1 && !isFormEnd(formEnd)) formEnd++;
        formSubstitutedTokens = 0;
        Form form = new Form(b.start[sig[formStart]], b.end[sig[formEnd]]);
        String attribute = attributeName(formStart, formEnd);
        boolean directive = "define".equals(attribute) || "undef".equals(attribute) ||
                            "ifdef".equals(attribute) || "ifndef".equals(attribute);
        List<Use> uses = new ArrayList<>();
        boolean ambiguous = false, refused = false;
        if (!directive) {
          for (int q = formStart; q <= formEnd; q++) {
            if (st(q) != ERL_QMARK) continue;
            Object r = handleUse(q, formEnd);
            if (r instanceof Use u) {
              uses.add(u);
              q = u.last;
            }
            else if (r == Outcome.AMBIGUOUS) ambiguous = true;
            else if (r instanceof Outcome o && o != Outcome.PREDEFINED && o != Outcome.NOT_IN_FILE) refused = true;
          }
        }
        if (!uses.isEmpty()) {
          int size = formEnd - formStart + 1;
          for (Use u : uses) size += u.result.size() - (u.last - u.q + 1);
          form.expandedTokens = size;
          if (size > formCap) {
            form.status = FormStatus.OVER_CAP;
            for (int i = 0; i < uses.size(); i++) {
              count(Outcome.FORM_OVER_CAP);
              outcomes.merge(Outcome.SUBSTITUTED, -1, Integer::sum);
            }
            uses.clear();
          }
          else {
            form.status = FormStatus.EXPANDED;
            form.substitutedUses = uses.size();
            for (Use u : uses) if (!u.def.fragment) form.allFragments = false;
          }
        }
        else if (ambiguous) form.status = FormStatus.NOT_EXPANDED_AMBIGUOUS;
        else if (refused) form.status = FormStatus.NOT_EXPANDED_OTHER;
        flushTo(form.srcStart);
        form.expStart = out.text.length();
        for (Use u : uses) {
          flushTo(b.start[sig[u.q]]);
          emitTokens(u.result);
          cursor = b.end[sig[u.last]];
        }
        flushTo(form.srcEnd);
        form.expEnd = out.text.length();
        forms.add(form);
        if ("define".equals(attribute)) define(formStart, formEnd);
        else if ("undef".equals(attribute)) undef(formStart, formEnd);
        else if ("ifdef".equals(attribute) || "ifndef".equals(attribute) || "if".equals(attribute)) conditionalDepth++;
        else if ("endif".equals(attribute) && conditionalDepth > 0) conditionalDepth--;
        p = formEnd + 1;
      }
      flushTo(b.text.length());
    }

    @Nullable String attributeName(int from, int to) {
      if (to - from < 1 || st(from) != ERL_OP_MINUS || st(from + 1) != ERL_ATOM_NAME) return null;
      return stext(from + 1);
    }

    Tok tokAt(int p, int kind) {
      int raw = sig[p];
      return new Tok(b.type[raw], b.text(raw), kind, b.start[raw], b.end[raw]);
    }

    void define(int from, int to) {
      int p = from + 3;
      if (p > to || st(from + 2) != ERL_PAR_LEFT) return;
      Name n = nameAt(p, to);
      if (n == null) return;
      String name = n.name;
      p = n.last + 1;
      int arity = -1;
      List<String> params = new ArrayList<>();
      if (st(p) == ERL_PAR_LEFT) {
        p++;
        if (st(p) == ERL_PAR_RIGHT) {
          p++;
        }
        else {
          while (true) {
            if (st(p) != ERL_VAR) return;
            params.add(stext(p));
            p++;
            if (st(p) == ERL_COMMA) {
              p++;
              continue;
            }
            if (st(p) == ERL_PAR_RIGHT) {
              p++;
              break;
            }
            return;
          }
        }
        arity = params.size();
      }
      int bodyEnd = st(to) == ERL_DOT ? to - 1 : to;
      if (st(bodyEnd) != ERL_PAR_RIGHT) return;
      List<Tok> body = new ArrayList<>();
      if (st(p) == ERL_COMMA) {
        for (int q = p + 1; q < bodyEnd; q++) body.add(tokAt(q, SRC_BODY));
      }
      else if (p != bodyEnd) {
        return;
      }
      if (PREDEFINED.contains(name)) return;
      Def def = new Def(name, arity, params, body, isFragment(body, params));
      defs.computeIfAbsent(name, k -> new ArrayList<>()).add(def);
    }

    void undef(int from, int to) {
      int p = from + 3;
      if (conditionalDepth > 0 || st(from + 2) != ERL_PAR_LEFT) return;
      Name n = nameAt(p, to);
      if (n != null) defs.remove(n.name);
    }

    boolean isFragment(List<Tok> body, List<String> params) {
      if (body.isEmpty()) return true;
      Deque<IElementType> stack = new ArrayDeque<>();
      int blocks = 0;
      for (int i = 0; i < body.size(); i++) {
        IElementType t = body.get(i).type;
        if (t == ERL_COLON_COLON) return true;
        Name nestedName = t == ERL_QMARK ? tokName(body, i + 1) : null;
        if (nestedName != null) {
          List<Def> nested = defs.get(nestedName.name);
          if (nested != null) {
            for (Def d : nested) if (d.fragment) return true;
          }
        }
        if (t == ERL_PAR_LEFT || t == ERL_BRACKET_LEFT || t == ERL_CURLY_LEFT || t == ERL_BIN_START) {
          stack.push(t);
          continue;
        }
        if (t == ERL_FUN && i + 1 < body.size() &&
            (body.get(i + 1).type == ERL_PAR_LEFT ||
             body.get(i + 1).type == ERL_VAR && i + 2 < body.size() && body.get(i + 2).type == ERL_PAR_LEFT)) {
          stack.push(ERL_FUN);
          blocks++;
          continue;
        }
        if (BLOCK_OPENERS.contains(t)) {
          stack.push(t);
          blocks++;
          continue;
        }
        if (t == ERL_PAR_RIGHT || t == ERL_BRACKET_RIGHT || t == ERL_CURLY_RIGHT || t == ERL_BIN_END) {
          IElementType open = stack.isEmpty() ? null : stack.pop();
          if (open == null || matching(open) != t) return true;
          continue;
        }
        if (t == ERL_END) {
          IElementType open = stack.isEmpty() ? null : stack.pop();
          if (open == null || matching(open) != ERL_END) return true;
          blocks--;
          continue;
        }
        if (stack.isEmpty() && NOT_IN_EXPRESSION_TOP.contains(t)) return true;
        if (blocks == 0 && (t == ERL_ARROW || t == ERL_WHEN || t == ERL_SEMI)) return true;
      }
      if (!stack.isEmpty()) return true;
      if (BINARY_ONLY_OPERATORS.contains(body.get(0).type)) return true;
      if (ANY_OPERATOR_OR_DANGLING.contains(body.get(body.size() - 1).type)) return true;
      return body.size() >= 3 && body.get(0).type == ERL_OP_MINUS && body.get(1).type == ERL_ATOM_NAME &&
             body.get(2).type == ERL_PAR_LEFT;
    }

    static IElementType matching(IElementType open) {
      if (open == ERL_PAR_LEFT) return ERL_PAR_RIGHT;
      if (open == ERL_BRACKET_LEFT) return ERL_BRACKET_RIGHT;
      if (open == ERL_CURLY_LEFT) return ERL_CURLY_RIGHT;
      if (open == ERL_BIN_START) return ERL_BIN_END;
      return ERL_END;
    }

    Selection select(String name, int arity) {
      if (PREDEFINED.contains(name)) return new Selection(null, Outcome.PREDEFINED);
      List<Def> list = defs.get(name);
      if (list == null || list.isEmpty()) return new Selection(null, Outcome.NOT_IN_FILE);
      boolean allNone = true;
      for (Def d : list) if (d.arity != -1) allNone = false;
      List<Def> candidates = new ArrayList<>();
      for (Def d : list) if (allNone || d.arity == arity) candidates.add(d);
      if (candidates.isEmpty()) return new Selection(null, Outcome.ARITY_MISMATCH);
      Def first = candidates.get(0);
      for (Def d : candidates) {
        if (!d.key.equals(first.key)) return new Selection(null, Outcome.AMBIGUOUS);
      }
      return new Selection(first, Outcome.SUBSTITUTED);
    }

    /**
     * Handles a {@code ?} at significant position q: returns a {@link Use} when the use is substituted, else the
     * {@link Outcome} that stopped it, or null when it is not a macro use.
     */
    @Nullable Object handleUse(int q, int formEnd) {
      if (q > 0 && st(q - 1) == ERL_QMARK && sig[q - 1] + 1 == sig[q]) return null; // second '?' of '??X'
      Name n = nameAt(q + 1, formEnd);
      if (n == null) return null;
      String name = n.name;
      int nameLast = n.last;
      int last = nameLast;
      List<List<Tok>> args = null;
      int arity = -1;
      if (nameLast + 1 <= formEnd && st(nameLast + 1) == ERL_PAR_LEFT) {
        int from = nameLast + 1;
        // a lazy view: parseArgs reads only up to the closing parenthesis
        List<Tok> tokens = new AbstractList<>() {
          @Override
          public Tok get(int i) {
            return tokAt(from + i, SRC_ARGUMENT);
          }

          @Override
          public int size() {
            return formEnd - from + 1;
          }
        };
        ArgsResult parsed = parseArgs(tokens, 0);
        if (parsed == null) {
          count(Outcome.BAD_ARGUMENTS);
          return Outcome.BAD_ARGUMENTS;
        }
        args = parsed.args;
        arity = args.size();
        last = from + parsed.closeIndex;
      }
      Selection selection = select(name, arity);
      if (selection.def == null) {
        count(selection.outcome);
        return selection.outcome;
      }
      Def def = selection.def;
      if (def.arity == -1) {
        // a definition without parentheses: the argument list, if any, stays in the text
        last = nameLast;
        args = null;
      }
      try {
        List<Tok> result = expand(def, args, 1, new ArrayDeque<>());
        if (formSubstitutedTokens + result.size() > MAX_TOKENS_PER_FORM) throw new LimitException(Outcome.LIMIT_TOKENS);
        formSubstitutedTokens += result.size();
        count(Outcome.SUBSTITUTED);
        return new Use(q, last, def, result);
      }
      catch (LimitException e) {
        count(e.outcome);
        return e.outcome;
      }
    }

    List<Tok> expand(Def def, @Nullable List<List<Tok>> args, int depth, Deque<String> active) throws LimitException {
      if (depth > MAX_DEPTH) throw new LimitException(Outcome.LIMIT_DEPTH);
      String activeKey = def.name + "/" + def.arity;
      if (active.contains(activeKey)) throw new LimitException(Outcome.LIMIT_RECURSION);
      // the arguments are expanded first, in the context of the use (epp rescans them after the substitution and only
      // rejects macros whose own bodies are circular)
      List<List<Tok>> expandedArgs = null;
      if (args != null) {
        expandedArgs = new ArrayList<>(args.size());
        for (List<Tok> arg : args) expandedArgs.add(rescan(arg, depth, active));
      }
      List<Tok> substituted = new ArrayList<>();
      List<Tok> body = def.body;
      for (int i = 0; i < body.size(); i++) {
        Tok t = body.get(i);
        if (args != null && t.type == ERL_QMARK && i + 2 < body.size() && body.get(i + 1).type == ERL_QMARK &&
            body.get(i + 2).type == ERL_VAR) {
          int index = def.params.indexOf(body.get(i + 2).text);
          if (index >= 0) {
            substituted.add(new Tok(ERL_STRING, stringify(args.get(index))));
            i += 2;
            continue;
          }
        }
        if (args != null && t.type == ERL_VAR) {
          int index = def.params.indexOf(t.text);
          if (index >= 0) {
            substituted.addAll(expandedArgs.get(index));
            continue;
          }
        }
        substituted.add(t);
      }
      active.push(activeKey);
      try {
        return rescan(substituted, depth, active);
      }
      finally {
        active.pop();
      }
    }

    /** Expands the nested uses of same-file macros in a substitution result. */
    List<Tok> rescan(List<Tok> tokens, int depth, Deque<String> active) throws LimitException {
      List<Tok> out = new ArrayList<>(tokens.size());
      for (int i = 0; i < tokens.size(); i++) {
        Tok t = tokens.get(i);
        if (t.type == ERL_QMARK && i + 1 < tokens.size() && tokens.get(i + 1).type == ERL_QMARK) {
          out.add(t);
          out.add(tokens.get(++i));
          continue;
        }
        Name n = t.type == ERL_QMARK ? tokName(tokens, i + 1) : null;
        if (n == null) {
          out.add(t);
          continue;
        }
        String name = n.name;
        int next = n.last + 1;
        List<List<Tok>> args = null;
        int arity = -1;
        ArgsResult parsed = null;
        if (next < tokens.size() && tokens.get(next).type == ERL_PAR_LEFT) {
          parsed = parseArgs(tokens, next);
          if (parsed != null) {
            args = parsed.args;
            arity = args.size();
          }
        }
        Selection selection = select(name, arity);
        if (selection.def == null || parsed == null && arity != -1) {
          out.add(t);
          continue; // left as it is; the name follows as a plain token
        }
        Def def = selection.def;
        List<Tok> expanded = expand(def, def.arity == -1 ? null : args, depth + 1, active);
        out.addAll(expanded);
        if (formSubstitutedTokens + out.size() > MAX_TOKENS_PER_FORM) throw new LimitException(Outcome.LIMIT_TOKENS);
        i = def.arity == -1 || parsed == null ? n.last : next + parsed.closeIndex;
      }
      return out;
    }
  }

  private record Name(String name, int last) {
  }

  /** A macro name in a token list at index i (an atom, a variable or a quoted atom), or null. */
  static @Nullable Name tokName(List<Tok> tokens, int i) {
    if (i >= tokens.size()) return null;
    IElementType t = tokens.get(i).type;
    if (t == ERL_VAR || t == ERL_ATOM_NAME) return new Name(tokens.get(i).text, i);
    if (t == ERL_SINGLE_QUOTE && i + 2 < tokens.size() && tokens.get(i + 1).type == ERL_ATOM_NAME &&
        tokens.get(i + 2).type == ERL_SINGLE_QUOTE) {
      return new Name(tokens.get(i + 1).text, i + 2);
    }
    return null;
  }

  record ArgsResult(List<List<Tok>> args, int closeIndex) {
  }

  /**
   * Splits the arguments of a call that starts with '(' at {@code open}, as epp's macro_arg does. Returns the
   * arguments and the index of the closing ')' relative to {@code open}, or null if there is no closing ')'.
   */
  static @Nullable ArgsResult parseArgs(List<Tok> tokens, int open) {
    List<List<Tok>> args = new ArrayList<>();
    int i = open + 1;
    if (i < tokens.size() && tokens.get(i).type == ERL_PAR_RIGHT) return new ArgsResult(args, i - open);
    Deque<IElementType> expected = new ArrayDeque<>();
    List<Tok> arg = new ArrayList<>();
    for (; i < tokens.size(); i++) {
      Tok t = tokens.get(i);
      IElementType type = t.type;
      if (expected.isEmpty() && (type == ERL_COMMA || type == ERL_PAR_RIGHT)) {
        args.add(arg);
        if (type == ERL_PAR_RIGHT) return new ArgsResult(args, i - open);
        arg = new ArrayList<>();
        continue;
      }
      if (type == ERL_PAR_LEFT) expected.push(ERL_PAR_RIGHT);
      else if (type == ERL_BIN_START) expected.push(ERL_BIN_END);
      else if (type == ERL_BRACKET_LEFT) expected.push(ERL_BRACKET_RIGHT);
      else if (type == ERL_CURLY_LEFT) expected.push(ERL_CURLY_RIGHT);
      else if (BLOCK_OPENERS.contains(type)) expected.push(ERL_END);
      else if (type == ERL_FUN && i + 1 < tokens.size() &&
               (tokens.get(i + 1).type == ERL_PAR_LEFT ||
                tokens.get(i + 1).type == ERL_VAR && i + 2 < tokens.size() && tokens.get(i + 2).type == ERL_PAR_LEFT)) {
        expected.push(ERL_END);
      }
      else if (!expected.isEmpty() && expected.peek() == type) expected.pop();
      arg.add(t);
    }
    return null;
  }

  /** {@code ??Arg}: the argument tokens joined with spaces, as one string token (epp's stringify, roughly). */
  static String stringify(List<Tok> arg) {
    StringBuilder text = new StringBuilder();
    for (int i = 0; i < arg.size(); i++) {
      Tok t = arg.get(i);
      if (t.type == ERL_SINGLE_QUOTE && i + 2 < arg.size() && arg.get(i + 2).type == ERL_SINGLE_QUOTE) {
        if (!text.isEmpty()) text.append(' ');
        text.append('\'').append(arg.get(i + 1).text).append('\'');
        i += 2;
        continue;
      }
      if (!text.isEmpty()) text.append(' ');
      text.append(t.text);
    }
    StringBuilder s = new StringBuilder("\"");
    for (int i = 0; i < text.length(); i++) {
      char c = text.charAt(i);
      if (c == '"' || c == '\\') s.append('\\');
      if (c == '\n') {
        s.append("\\n");
        continue;
      }
      s.append(c);
    }
    return s.append('"').toString();
  }
}
