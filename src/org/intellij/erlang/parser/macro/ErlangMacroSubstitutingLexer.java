package org.intellij.erlang.parser.macro;

import com.intellij.lexer.Lexer;
import com.intellij.lexer.LexerBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static org.intellij.erlang.ErlangTypes.*;

/**
 * Spike: a lexer for the PSI parser that substitutes the macros defined earlier in the same file.
 * <p>
 * Purity: the output is a function of the text and the base lexer only. It takes no project, file, SDK or index, reads
 * no other file, and does not evaluate {@code -ifdef}/{@code -if}: every {@code -define} earlier in the text counts.
 * <p>
 * For a substituted use it emits one {@link ErlangMacroSubstitution#MACRO_CALL_TEXT} token over the whole use text
 * ({@code ?NAME} or {@code ?NAME(Args)}), which the parser skips as a comment, and then the substituted tokens as
 * zero-length {@link ErlangMacroForeignLeafType} tokens at the end of the use. Everything else is the base lexer's
 * output, unchanged.
 * <p>
 * Selection: a use is substituted only when, among the definitions earlier in the file (after the last
 * {@code -undef} of the name), the definitions with its name and arity all have the same parameters and body. epp's
 * rule that a single definition without parentheses serves every arity is kept. Predefined macros, macros with no
 * definition in the file, and uses inside {@code -define}, {@code -undef}, {@code -ifdef} and {@code -ifndef} are left
 * as they are.
 */
public class ErlangMacroSubstitutingLexer extends LexerBase {
  public static final int MAX_DEPTH = 32;
  public static final int MAX_TOKENS_PER_FORM = 50_000;

  public static final Set<String> PREDEFINED = Set.of(
    "FILE", "FUNCTION_NAME", "FUNCTION_ARITY", "LINE", "MODULE", "MODULE_STRING", "BASE_MODULE", "BASE_MODULE_STRING",
    "MACHINE", "BEAM", "OTP_RELEASE", "FEATURE_AVAILABLE", "FEATURE_ENABLED");

  /** What happened to a macro use in the text (outside {@code -define} bodies). */
  public enum Outcome {
    SUBSTITUTED, PREDEFINED, NOT_IN_FILE, AMBIGUOUS, ARITY_MISMATCH, EXPRESSION_SHAPE_SKIPPED,
    LIMIT_DEPTH, LIMIT_TOKENS, LIMIT_RECURSION, BAD_ARGUMENTS
  }

  /** Receives every macro use the lexer sees; for the spike's statistics. */
  public interface UseListener {
    void use(int offset, @NotNull String name, int arity, @NotNull Outcome outcome, @Nullable Boolean fragmentBody,
             int substitutedTokens);
  }

  private static final TokenSet SKIPPED = TokenSet.orSet(ErlangParserDefinition.COMMENTS, TokenSet.create(TokenType.WHITE_SPACE));
  private static final TokenSet BLOCK_OPENERS = TokenSet.create(ERL_BEGIN, ERL_CASE, ERL_IF, ERL_RECEIVE, ERL_TRY, ERL_MAYBE);
  private static final TokenSet BINARY_ONLY_OPERATORS = TokenSet.create(
    ERL_OP_AR_MUL, ERL_OP_AR_DIV, ERL_DIV, ERL_REM, ERL_BAND, ERL_BOR, ERL_BXOR, ERL_BSL, ERL_BSR, ERL_AND, ERL_OR,
    ERL_XOR, ERL_ANDALSO, ERL_ORELSE, ERL_OP_EQ_EQ, ERL_OP_DIV_EQ, ERL_OP_EQ_LT, ERL_OP_LT, ERL_OP_GT_EQ, ERL_OP_GT,
    ERL_OP_EQ_COL_EQ, ERL_OP_EQ_DIV_EQ, ERL_OP_PLUS_PLUS, ERL_OP_MINUS_MINUS, ERL_OP_EQ, ERL_OP_EXL, ERL_OP_MAYBE_EQ);
  private static final TokenSet ANY_OPERATOR_OR_DANGLING = TokenSet.orSet(BINARY_ONLY_OPERATORS, TokenSet.create(
    ERL_OP_PLUS, ERL_OP_MINUS, ERL_NOT, ERL_BNOT, ERL_CATCH, ERL_RADIX, ERL_COLON, ERL_QMARK));
  /** Tokens that cannot be at the top level of one complete expression. */
  private static final TokenSet NOT_IN_EXPRESSION_TOP = TokenSet.create(
    ERL_COMMA, ERL_SEMI, ERL_ARROW, ERL_WHEN, ERL_OP_OR, ERL_OR_OR, ERL_ASSOC, ERL_MATCH, ERL_OP_LT_MINUS,
    ERL_OP_LT_EQ, ERL_OP_LT_COLON_MINUS, ERL_OP_LT_COLON_EQ, ERL_OP_AND_AND, ERL_DOT_DOT, ERL_DOT_DOT_DOT, ERL_OF,
    ERL_AFTER, ERL_END, ERL_ELSE);

  private final Lexer myBase;
  private final ErlangMacroSubstitution.Mode myMode;
  private @Nullable UseListener myListener;

  private CharSequence myBuffer = "";
  private int myBufferEnd;

  // output
  private IElementType[] myTypes = new IElementType[0];
  private int[] myStarts = ArrayUtil.EMPTY_INT_ARRAY;
  private int[] myEnds = ArrayUtil.EMPTY_INT_ARRAY;
  private int myCount;
  private int myIndex;

  // statistics of the last run
  private int myBaseTokenCount;
  private int myForeignTokenCount;

  public ErlangMacroSubstitutingLexer(@NotNull Lexer base, @NotNull ErlangMacroSubstitution.Mode mode) {
    myBase = base;
    myMode = mode;
  }

  public void setListener(@Nullable UseListener listener) {
    myListener = listener;
  }

  public int getBaseTokenCount() {
    return myBaseTokenCount;
  }

  public int getForeignTokenCount() {
    return myForeignTokenCount;
  }

  @Override
  public void start(@NotNull CharSequence buffer, int startOffset, int endOffset, int initialState) {
    myBuffer = buffer;
    myBufferEnd = endOffset;
    myCount = 0;
    myIndex = 0;
    myForeignTokenCount = 0;
    Base base = Base.lex(myBase, buffer, startOffset, endOffset, initialState);
    myBaseTokenCount = base.n;
    new Run(base).run();
  }

  @Override
  public int getState() {
    return 0;
  }

  @Override
  public @Nullable IElementType getTokenType() {
    return myIndex < myCount ? myTypes[myIndex] : null;
  }

  @Override
  public int getTokenStart() {
    return myIndex < myCount ? myStarts[myIndex] : myBufferEnd;
  }

  @Override
  public int getTokenEnd() {
    return myIndex < myCount ? myEnds[myIndex] : myBufferEnd;
  }

  @Override
  public void advance() {
    if (myIndex < myCount) myIndex++;
  }

  @Override
  public @NotNull CharSequence getBufferSequence() {
    return myBuffer;
  }

  @Override
  public int getBufferEnd() {
    return myBufferEnd;
  }

  private void emit(IElementType type, int start, int end) {
    if (myCount == myTypes.length) {
      int size = Math.max(16, myCount * 2);
      myTypes = Arrays.copyOf(myTypes, size);
      myStarts = Arrays.copyOf(myStarts, size);
      myEnds = Arrays.copyOf(myEnds, size);
    }
    myTypes[myCount] = type;
    myStarts[myCount] = start;
    myEnds[myCount] = end;
    myCount++;
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

  /**
   * A token of a macro body or of a substitution result, with its source: {@code kind} is
   * {@link ErlangMacroForeignLeafType#SOURCE_NONE} (made by the substitution, as {@code ??Arg}),
   * {@link ErlangMacroForeignLeafType#SOURCE_ARGUMENT} (argument {@code a}, significant token {@code index} of the top-level
   * use) or {@link ErlangMacroForeignLeafType#SOURCE_BODY} (significant token {@code index} of the body of the
   * {@code -define} of {@code defName}/{@code a}).
   */
  record Tok(@NotNull IElementType type, @NotNull String text, int kind, @Nullable String defName, int a, int index) {
    Tok(@NotNull IElementType type, @NotNull String text) {
      this(type, text, ErlangMacroForeignLeafType.SOURCE_NONE, null, 0, 0);
    }

    Tok asArgument(int argIndex, int tokenIndex) {
      return new Tok(type, text, ErlangMacroForeignLeafType.SOURCE_ARGUMENT, null, argIndex, tokenIndex);
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

  /** Lookup result. */
  private record Selection(@Nullable Def def, @NotNull Outcome outcome) {
  }

  private final class Run {
    final Base b;
    final int[] sig; // raw indices of the significant tokens
    final int sigCount;
    final Map<String, List<Def>> defs = new HashMap<>();
    int formSubstitutedTokens;
    int conditionalDepth;

    Run(Base b) {
      this.b = b;
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

    /** The macro name at significant position p and the position of its last token, or null. */
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

    void run() {
      int raw = 0; // next raw token to copy
      int p = 0;
      while (p < sigCount) {
        int formStart = p;
        int formEnd = p;
        while (formEnd < sigCount - 1 && !isFormEnd(formEnd)) formEnd++;
        formSubstitutedTokens = 0;
        String attribute = attributeName(formStart, formEnd);
        boolean directive = "define".equals(attribute) || "undef".equals(attribute) ||
                            "ifdef".equals(attribute) || "ifndef".equals(attribute);
        if (!directive) {
          for (int q = formStart; q <= formEnd; q++) {
            if (st(q) != ERL_QMARK) continue;
            int useEnd = handleUse(q, formEnd, raw);
            if (useEnd >= 0) {
              raw = sig[useEnd] + 1;
              q = useEnd;
            }
          }
        }
        if ("define".equals(attribute)) define(formStart, formEnd);
        else if ("undef".equals(attribute)) undef(formStart, formEnd);
        else if ("ifdef".equals(attribute) || "ifndef".equals(attribute) || "if".equals(attribute)) conditionalDepth++;
        else if ("endif".equals(attribute) && conditionalDepth > 0) conditionalDepth--;
        p = formEnd + 1;
      }
      while (raw < b.n) {
        emit(b.type[raw], b.start[raw], b.end[raw]);
        raw++;
      }
    }

    @Nullable String attributeName(int from, int to) {
      if (to - from < 1 || st(from) != ERL_OP_MINUS || st(from + 1) != ERL_ATOM_NAME) return null;
      return stext(from + 1);
    }

    // ---------------------------------------------------------------- definitions

    void define(int from, int to) {
      // - define ( Name [( Params )] , Body ) .
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
      // the body is everything between the comma and the ')' before the final dot
      int bodyEnd = st(to) == ERL_DOT ? to - 1 : to;
      if (st(bodyEnd) != ERL_PAR_RIGHT) return;
      List<Tok> body = new ArrayList<>();
      if (st(p) == ERL_COMMA) {
        for (int q = p + 1; q < bodyEnd; q++) {
          body.add(new Tok(st(q), stext(q), ErlangMacroForeignLeafType.SOURCE_BODY, name, arity, q - p - 1));
        }
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
      // An -undef inside -ifdef/-ifndef/-if may or may not run, and conditions are not evaluated: keep the earlier
      // definitions, so that a redefinition after it makes the name ambiguous instead of silently winning.
      if (conditionalDepth > 0 || st(from + 2) != ERL_PAR_LEFT) return;
      Name n = nameAt(p, to);
      if (n != null) defs.remove(n.name);
    }

    /**
     * The cheap shape test of the {@code fragments} mode. A body is "one complete expression" unless one of these holds:
     * it is empty; its brackets or blocks do not balance; it has {@code , ; -> when | || => := <- <= <:- <:= && .. ...}
     * outside every bracket and block ({@code -> when ;} also inside brackets, unless inside a
     * {@code fun/begin/case/if/receive/try/maybe ... end} block); it has {@code ::} anywhere; it starts with a binary-only
     * operator; it ends with an operator, {@code #}, {@code :} or {@code ?}; it starts with {@code - atom (} (an
     * attribute); or it uses a same-file macro that is a fragment.
     */
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

    // ---------------------------------------------------------------- uses in the text

    /**
     * Handles a {@code ?} at significant position q. If the use is substituted, copies the raw tokens before it, emits
     * the call text and the substituted tokens, and returns the significant position of the last token of the use.
     * Otherwise returns -1.
     */
    int handleUse(int q, int formEnd, int raw) {
      if (q > 0 && st(q - 1) == ERL_QMARK && sig[q - 1] + 1 == sig[q]) return -1; // second '?' of '??X'
      Name n = nameAt(q + 1, formEnd);
      if (n == null) return -1;
      String name = n.name;
      int nameLast = n.last;
      int last = nameLast;
      List<List<Tok>> args = null;
      int arity = -1;
      if (nameLast + 1 <= formEnd && st(nameLast + 1) == ERL_PAR_LEFT) {
        List<Tok> tokens = new ArrayList<>();
        for (int i = nameLast + 1; i <= formEnd; i++) tokens.add(new Tok(st(i), stext(i)));
        ArgsResult parsed = parseArgs(tokens, 0);
        if (parsed == null) {
          report(q, name, -1, Outcome.BAD_ARGUMENTS, null, 0);
          return -1;
        }
        args = new ArrayList<>(parsed.args.size());
        for (int i = 0; i < parsed.args.size(); i++) {
          List<Tok> arg = parsed.args.get(i);
          List<Tok> sourced = new ArrayList<>(arg.size());
          for (int j = 0; j < arg.size(); j++) sourced.add(arg.get(j).asArgument(i, j));
          args.add(sourced);
        }
        arity = args.size();
        last = nameLast + 1 + parsed.closeIndex;
      }
      Selection selection = select(name, arity);
      if (selection.def == null) {
        report(q, name, arity, selection.outcome, null, 0);
        return -1;
      }
      Def def = selection.def;
      if (myMode == ErlangMacroSubstitution.Mode.FRAGMENTS && !def.fragment) {
        report(q, name, arity, Outcome.EXPRESSION_SHAPE_SKIPPED, false, 0);
        return -1;
      }
      if (def.arity == -1) {
        // a definition without parentheses: the argument list, if any, stays in the text
        last = nameLast;
        args = null;
      }
      List<Tok> result;
      try {
        result = expand(def, args, 1, new ArrayDeque<>());
        if (result.size() > ErlangMacroSubstitution.MAX_TOKENS_PER_USE) throw new LimitException(Outcome.LIMIT_TOKENS);
        if (formSubstitutedTokens + result.size() > MAX_TOKENS_PER_FORM) throw new LimitException(Outcome.LIMIT_TOKENS);
        formSubstitutedTokens += result.size();
      }
      catch (LimitException e) {
        report(q, name, arity, e.outcome, def.fragment, 0);
        return -1;
      }
      // copy the raw tokens before the use, then the call text, then the substituted tokens
      int useStartRaw = sig[q];
      int useEndRaw = sig[last];
      for (int i = raw; i < useStartRaw; i++) emit(b.type[i], b.start[i], b.end[i]);
      int startOffset = b.start[useStartRaw];
      int endOffset = b.end[useEndRaw];
      emit(ErlangMacroSubstitution.MACRO_CALL_TEXT, startOffset, endOffset);
      for (Tok t : result) {
        emit(new ErlangMacroForeignLeafType(t.type, t.text, name, 1, t.kind, t.defName, t.a, t.index), endOffset, endOffset);
      }
      myForeignTokenCount += result.size();
      report(q, name, arity, Outcome.SUBSTITUTED, def.fragment, result.size());
      return last;
    }

    void report(int q, String name, int arity, Outcome outcome, @Nullable Boolean fragment, int tokens) {
      UseListener listener = myListener;
      if (listener != null) listener.use(b.start[sig[q]], name, arity, outcome, fragment, tokens);
    }

    // ---------------------------------------------------------------- expansion

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
