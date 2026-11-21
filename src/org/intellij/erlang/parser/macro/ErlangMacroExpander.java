package org.intellij.erlang.parser.macro;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.progress.ProgressManager;
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
  /**
   * The work of the expansions of one form: expansions, tokens scanned and tokens added. Above it the use gets
   * {@link Outcome#LIMIT_TOKENS}. Empty bodies make no tokens, so the token limit alone does not stop
   * {@code -define(L2, ?L1 ?L1)}-style chains (2^n expansions); a 50,000-token form at depth 32 is far below.
   */
  public static final long MAX_WORK_PER_FORM = 4_000_000;
  /** The work of all forms of a file; after it every use gets {@link Outcome#LIMIT_TOKENS}. */
  public static final long MAX_WORK_PER_FILE = 40_000_000;
  private static volatile long ourWorkPerForm = MAX_WORK_PER_FORM;
  private static volatile long ourWorkPerFile = MAX_WORK_PER_FILE;

  /** Tests: other work limits (to check that a cancelled indicator stops a long expansion). */
  @org.jetbrains.annotations.TestOnly
  public static void setWorkLimitsForTests(long perForm, long perFile, @NotNull com.intellij.openapi.Disposable parent) {
    ourWorkPerForm = perForm;
    ourWorkPerFile = perFile;
    com.intellij.openapi.util.Disposer.register(parent, () -> {
      ourWorkPerForm = MAX_WORK_PER_FORM;
      ourWorkPerFile = MAX_WORK_PER_FILE;
    });
  }

  public static final int SRC_NONE = 0;
  public static final int SRC_ARGUMENT = 1;
  public static final int SRC_BODY = 2;
  public static final int SRC_VERBATIM = 3;
  /** A token of a {@code -define} body in an included header; its origin is a {@link HeaderTok}. */
  public static final int SRC_HEADER = 4;

  /** Where a header token comes from: the header (an opaque key of the caller), the directive and the token index. */
  public record HeaderTok(@NotNull Object header, int directive, int index) {
  }

  /** Resolves an {@code -include}/{@code -include_lib} directive of a file (owner null) or of a header. */
  public interface IncludeResolver {
    /** The summary of the included header, or null: then nothing of that include is visible. */
    @Nullable HeaderSummary resolve(@Nullable HeaderSummary owner, @NotNull String attribute, @Nullable String includeString);
  }

  /** One macro directive of a header, as significant tokens. */
  public static final class Directive {
    public final @NotNull String attribute;
    public final @NotNull List<Tok> tokens;

    Directive(@NotNull String attribute, @NotNull List<Tok> tokens) {
      this.attribute = attribute;
      this.tokens = tokens;
    }

    /** The unquoted string of an include directive, else null. */
    public @Nullable String includeString() {
      for (Tok t : tokens) {
        if (t.type() == ERL_STRING) {
          String x = t.text();
          return x.length() >= 2 ? x.substring(1, x.length() - 1) : x;
        }
      }
      return null;
    }

    public @NotNull String name() {
      return tokens.size() > 3 && tokens.get(2).type() == ERL_PAR_LEFT ? tokens.get(3).text() : "";
    }
  }

  /**
   * The macro summary of a header, made by lexing its text (no PSI). Only the directive forms are kept:
   * {@code -define}, {@code -undef}, {@code -include}, {@code -include_lib} and the conditional directives.
   * {@link #hash} depends only on the token texts of those forms: an edit anywhere else keeps it.
   */
  public static final class HeaderSummary {
    public final @NotNull Object header;
    public final @NotNull List<Directive> directives;
    public final long hash;
    public final int tokens;
    public final int defines;

    HeaderSummary(@NotNull Object header, @NotNull List<Directive> directives, long hash, int tokens, int defines) {
      this.header = header;
      this.directives = directives;
      this.hash = hash;
      this.tokens = tokens;
      this.defines = defines;
    }

    private volatile List<DefineShape> myShapes;

    /** Round 5: the shapes of the header's {@code -define} directives (for the "needs substitution" gate); cached. */
    public @NotNull List<DefineShape> defineShapes() {
      List<DefineShape> r = myShapes;
      if (r == null) {
        r = new ArrayList<>();
        for (Directive d : directives) {
          if (!"define".equals(d.attribute)) continue;
          DefineShape shape = defineShape(d.tokens);
          r.add(shape != null ? shape : DefineShape.unknown(d.name()));
        }
        myShapes = r = List.copyOf(r);
      }
      return r;
    }

    /** The include directives in text order. */
    public @NotNull List<Directive> includes() {
      List<Directive> r = new ArrayList<>();
      for (Directive d : directives) if (isInclude(d.attribute)) r.add(d);
      return r;
    }
  }

  static final Set<String> DIRECTIVES = Set.of("define", "undef", "include", "include_lib", "ifdef", "ifndef", "if",
                                               "elif", "else", "endif");

  static boolean isInclude(String attribute) {
    return "include".equals(attribute) || "include_lib".equals(attribute);
  }

  public static final Set<String> PREDEFINED = Set.of(
    "FILE", "FUNCTION_NAME", "FUNCTION_ARITY", "LINE", "MODULE", "MODULE_STRING", "BASE_MODULE", "BASE_MODULE_STRING",
    "MACHINE", "BEAM", "OTP_RELEASE", "FEATURE_AVAILABLE", "FEATURE_ENABLED");

  /** What happened to a macro use in the text (outside {@code -define} bodies). */
  public enum Outcome {
    SUBSTITUTED, PREDEFINED, NOT_IN_FILE, AMBIGUOUS, ARITY_MISMATCH, CONDITIONAL,
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
    /**
     * Round 6: every macro use of the form, nested ones included, was substituted (or is a predefined macro), and the
     * form is not inside an {@code -ifdef}, {@code -ifndef} or {@code -if} branch: its expanded text is what epp gives.
     */
    public boolean complete;
    /**
     * Round 6 (next), item 5: a use of the form (not inside a conditional branch) that the expander refused because
     * the body of {@link #circularMacro} comes back to itself (epp: "circular macro"), with its source range; else null.
     */
    public @Nullable String circularMacro;
    public int circularStart = -1, circularEnd = -1;

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
    private final Object[] myOrigin;
    /** Uses substituted with a definition from a header; uses of header names lost to "one visible definition". */
    public int headerUses, headerAmbiguous, headerConditional;
    /** Significant tokens of the source, and of the expanded text (estimated from the substitutions). */
    public int sourceTokens, expandedTokens;
    /** Spike 5: time of the base lexing inside {@link #expand} (measurement only). */
    public long lexNanos;
    /** The work of all expansions (see {@link #MAX_WORK_PER_FORM}). */
    public long work;
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
      myOrigin = out.origin;
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

    /**
     * Round 5: the expanded offsets of every copy of a {@code -define} body token: a same-file body token by its source
     * offset ({@code origin} null), a header body token by its origin.
     */
    public int @NotNull [] bodyCopies(int srcOffset, @Nullable HeaderTok origin) {
      int[] r = new int[0];
      for (int i = 0; i < mySegments; i++) {
        boolean match = origin == null ? myKind[i] == SRC_BODY && mySrcStart[i] == srcOffset
                                       : myKind[i] == SRC_HEADER && origin.equals(myOrigin[i]);
        if (!match) continue;
        r = Arrays.copyOf(r, r.length + 1);
        r[r.length - 1] = myExpStart[i];
      }
      return r;
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

    /** The header origin of a single expanded token range, or null when the range is not one header token. */
    public @Nullable HeaderTok headerOrigin(int expStart, int expEnd) {
      int s = segmentAt(expStart);
      if (s < 0 || myKind[s] != SRC_HEADER || myExpStart[s] != expStart || myExpEnd[s] != expEnd) return null;
      return myOrigin[s] instanceof HeaderTok h ? h : null;
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
        if (s < 0 || myKind[s] == SRC_NONE || myKind[s] == SRC_HEADER) return null;
        int at = mySrcStart[s] + (expStart - myExpStart[s]);
        return TextRange.create(at, at);
      }
      int first = segmentAt(expStart);
      int last = segmentAt(expEnd - 1);
      if (first < 0 || last < 0) return null;
      int kind = myKind[first];
      if (kind == SRC_NONE || kind == SRC_HEADER) return null;
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

    /**
     * Round 6: the source range of the outermost macro use that produced the expanded offset (a body, header or
     * argument token): the source between the copied (verbatim) text before and after it, without the spaces at its
     * ends. Null for an offset in copied text.
     */
    public @Nullable TextRange useSourceRange(int expOffset) {
      int s = segmentAt(expOffset);
      if (s < 0 || myKind[s] == SRC_VERBATIM) return null;
      int p = s - 1;
      while (p >= 0 && myKind[p] != SRC_VERBATIM) p--;
      int n = s + 1;
      while (n < mySegments && myKind[n] != SRC_VERBATIM) n++;
      if (p < 0 || n >= mySegments) return null;
      int start = mySrcEnd[p], end = mySrcStart[n];
      while (start < end && Character.isWhitespace(sourceText.charAt(start))) start++;
      while (end > start && Character.isWhitespace(sourceText.charAt(end - 1))) end--;
      return start < end ? TextRange.create(start, end) : null;
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
    Object[] origin = new Object[64];
    int n;

    void segment(int es, int ee, int ss, int se, int k) {
      segment(es, ee, ss, se, k, null);
    }

    void segment(int es, int ee, int ss, int se, int k, @Nullable Object o) {
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
        origin = Arrays.copyOf(origin, size);
      }
      origin[n] = o;
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
    return expand(text, base, formCap, null);
  }

  /** Expands the text with the macros of the included headers ({@code resolver} null: same-file macros only). */
  public static @NotNull Expansion expand(@NotNull CharSequence text, @NotNull Lexer base, int formCap,
                                          @Nullable IncludeResolver resolver) {
    long l0 = System.nanoTime();
    Base b = Base.lex(base, text, 0, text.length(), 0);
    long lexNanos = System.nanoTime() - l0;
    Run run = new Run(b, formCap, resolver);
    run.run();
    Expansion e = new Expansion(text, run.out.text.toString(), run.forms, run.outcomes, run.out);
    e.headerUses = run.headerUses;
    e.headerAmbiguous = run.headerAmbiguous;
    e.headerConditional = run.headerConditional;
    e.sourceTokens = run.sigCount;
    e.expandedTokens = run.sigCount + run.expandedDelta;
    e.lexNanos = lexNanos;
    e.work = run.fileWork + run.work;
    return e;
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
        if ((b.n & 1023) == 0) ProgressManager.checkCanceled();
        lexer.advance();
      }
      return b;
    }

    String text(int i) {
      return text.subSequence(start[i], end[i]).toString();
    }
  }

  /** A token of a macro body, an argument or a result, with its source kind and absolute source range. */
  public record Tok(@NotNull IElementType type, @NotNull String text, int kind, int start, int end, @Nullable Object origin) {
    Tok(@NotNull IElementType type, @NotNull String text) {
      this(type, text, SRC_NONE, -1, -1, null);
    }

    Tok(@NotNull IElementType type, @NotNull String text, int kind, int start, int end) {
      this(type, text, kind, start, end, null);
    }
  }

  /** Lexes a header and keeps its directive forms. Checks for cancellation. */
  public static @NotNull HeaderSummary summarize(@NotNull CharSequence text, @NotNull Lexer lexer, @NotNull Object header) {
    Base b = Base.lex(lexer, text, 0, text.length(), 0);
    Run run = new Run(b, Integer.MAX_VALUE, null);
    List<Directive> directives = new ArrayList<>();
    long hash = 17;
    int defines = 0;
    int p = 0;
    while (p < run.sigCount) {
      if ((p & 255) == 0) ProgressManager.checkCanceled();
      int formEnd = p;
      while (formEnd < run.sigCount - 1 && !run.isFormEnd(formEnd)) formEnd++;
      String attribute = run.attributeName(p, formEnd);
      if (attribute != null && DIRECTIVES.contains(attribute)) {
        int di = directives.size();
        List<Tok> toks = new ArrayList<>(formEnd - p + 1);
        for (int q = p; q <= formEnd; q++) {
          int raw = run.sig[q];
          String t = b.text(raw);
          toks.add(new Tok(b.type[raw], t, SRC_HEADER, b.start[raw], b.end[raw], new HeaderTok(header, di, q - p)));
          hash = hash * 1_000_003L + t.hashCode();
        }
        hash = hash * 31 + 7;
        directives.add(new Directive(attribute, List.copyOf(toks)));
        if ("define".equals(attribute)) defines++;
      }
      p = formEnd + 1;
    }
    return new HeaderSummary(header, List.copyOf(directives), hash, run.sigCount, defines);
  }

  /**
   * Round 5: what the "needs substitution" gate of the macro layer knows of one {@code -define}: its name, its
   * parameters, whether its body alone is "locally plain" (see {@link #isPlainBody}), the macro names its body uses
   * (to be checked transitively by the caller), and the variables of its body that are not parameters.
   */
  public record DefineShape(@NotNull String name, int arity, @NotNull List<String> params, boolean plainBody,
                            @NotNull Set<String> nestedUses, @NotNull Set<String> freeVars) {
    /** A definition that could not be read: never plain. */
    static @NotNull DefineShape unknown(@NotNull String name) {
      return new DefineShape(name, -1, List.of(), false, Set.of(), Set.of());
    }
  }

  /**
   * Round 5: the shape of a {@code -define} directive given as its significant tokens ({@code - define ( Name ... ) .}),
   * as the expander reads it; null when the expander would ignore it (a malformed or a predefined name).
   */
  public static @Nullable DefineShape defineShape(@NotNull List<Tok> t) {
    int to = t.size() - 1;
    int p = 3;
    if (p > to || Run.type(t, 2) != ERL_PAR_LEFT) return null;
    Name n = tokName(t, p);
    if (n == null) return null;
    p = n.last + 1;
    int arity = -1;
    List<String> params = new ArrayList<>();
    if (Run.type(t, p) == ERL_PAR_LEFT) {
      p++;
      if (Run.type(t, p) == ERL_PAR_RIGHT) {
        p++;
      }
      else {
        while (true) {
          if (Run.type(t, p) != ERL_VAR) return null;
          params.add(t.get(p).text());
          p++;
          if (Run.type(t, p) == ERL_COMMA) {
            p++;
            continue;
          }
          if (Run.type(t, p) == ERL_PAR_RIGHT) {
            p++;
            break;
          }
          return null;
        }
      }
      arity = params.size();
    }
    int bodyEnd = Run.type(t, to) == ERL_DOT ? to - 1 : to;
    if (Run.type(t, bodyEnd) != ERL_PAR_RIGHT) return null;
    List<Tok> body;
    if (Run.type(t, p) == ERL_COMMA) body = t.subList(p + 1, bodyEnd);
    else if (p == bodyEnd) body = List.of();
    else return null;
    if (PREDEFINED.contains(n.name)) return null;
    Set<String> nested = new HashSet<>();
    Set<String> free = new HashSet<>();
    boolean plain = isPlainBody(body, params, nested, free);
    return new DefineShape(n.name, arity, List.copyOf(params), plain, nested, free);
  }

  /** Tokens that never occur in a plain body: they bind (comprehension generators, maybe match), send or type. */
  private static final TokenSet NOT_IN_PLAIN_BODY = TokenSet.create(
    ERL_OR_OR, ERL_OP_LT_MINUS, ERL_OP_LT_EQ, ERL_OP_LT_COLON_MINUS, ERL_OP_LT_COLON_EQ, ERL_OP_MAYBE_EQ, ERL_OP_EXL,
    ERL_COLON_COLON, ERL_DOT_DOT, ERL_DOT_DOT_DOT, ERL_OP_AND_AND);
  /** Tokens that open a block closed by {@code end}. */
  private static final TokenSet PLAIN_BLOCK_OPENERS = TokenSet.create(ERL_BEGIN, ERL_CASE, ERL_IF, ERL_RECEIVE, ERL_TRY, ERL_MAYBE);

  /** An open bracket or block of a body being classified. */
  private static final class Open {
    final IElementType type;
    final boolean block;
    final boolean recordBrace;
    /** In a clause head (a pattern): from the start of a clause to its {@code when} or {@code ->}. */
    boolean head;

    Open(IElementType type, boolean block, boolean recordBrace, boolean head) {
      this.type = type;
      this.block = block;
      this.recordBrace = recordBrace;
      this.head = head;
    }
  }

  /**
   * Round 5: a body is "locally plain" when, read as tokens, it cannot bind or capture anything around it or in its
   * arguments:
   * <ul>
   *   <li>it is not empty, and its only variables are its parameters and {@code _} ({@code ??P} only for a parameter P);</li>
   *   <li>no parameter occurs in a clause head (a pattern): from the start of a clause of {@code case ... of},
   *   {@code receive}, {@code try ... of / catch}, {@code fun (...)}, {@code maybe ... else} or {@code if} to its
   *   {@code when} or {@code ->}; so the patterns of the body bind nothing;</li>
   *   <li>it has no {@code || <- <= <:- <:= ?= ! :: .. ... &&} (no comprehension generator, maybe match, send or type);
   *   {@code =} occurs only directly inside the braces of a record, right after a field name ({@code #r{f = A}}; not
   *   the match of {@code #r{f = A = B}});</li>
   *   <li>{@code ,} occurs only inside brackets or blocks, {@code |} only directly inside a list; {@code ; -> when of
   *   after catch else} only directly inside a block; {@code .} only after {@code #name} (record field access);
   *   {@code fun} is followed by {@code (} only as a fun with clauses (a block);</li>
   *   <li>its brackets ({@code () [] {} << >>}) and blocks ({@code ... end}) are balanced, it does not start with a
   *   binary-only operator and does not end with an operator, and it does not look like an attribute
   *   ({@code -name(});</li>
   *   <li>every {@code ?} starts a nested macro use {@code ?Name} (collected in {@code nestedUses}, which the caller
   *   checks transitively) or a stringified parameter {@code ??P}.</li>
   * </ul>
   * Such a body put in place of a use cannot bind a variable (its patterns hold no variable), and its blocks only
   * narrow the scope of the bindings of its arguments; blocks and brackets are primary expressions, and its other
   * operators bind tighter than {@code =}, so the variables around the use stay on the same side of every match.
   * The variables of the body that are neither parameters nor {@code _} go to {@code freeVars} in every case.
   */
  static boolean isPlainBody(@NotNull List<Tok> body, @NotNull List<String> params, @NotNull Set<String> nestedUses,
                             @NotNull Set<String> freeVars) {
    boolean plain = !body.isEmpty();
    Deque<Open> stack = new ArrayDeque<>();
    for (int i = 0; i < body.size(); i++) {
      Tok tok = body.get(i);
      IElementType t = tok.type;
      Open top = stack.peek();
      if (t == ERL_QMARK) {
        if (i + 1 < body.size() && body.get(i + 1).type == ERL_QMARK) {
          // ??P: the stringified argument; only for a parameter
          if (!(i + 2 < body.size() && body.get(i + 2).type == ERL_VAR && params.contains(body.get(i + 2).text))) plain = false;
          else i += 2;
          continue;
        }
        Name nested = tokName(body, i + 1);
        if (nested == null) plain = false;
        else {
          nestedUses.add(nested.name);
          i = nested.last;
        }
        continue;
      }
      if (t == ERL_VAR) {
        if (tok.text.equals("_")) continue;
        if (!params.contains(tok.text)) {
          freeVars.add(tok.text);
          plain = false;
        }
        else if (top != null && top.head) plain = false; // a parameter in a pattern of the body
        continue;
      }
      if (!plain) continue; // keep collecting the free variables and nested uses
      if (NOT_IN_PLAIN_BODY.contains(t)) {
        plain = false;
        continue;
      }
      boolean inBlock = top != null && top.block;
      if (PLAIN_BLOCK_OPENERS.contains(t)) {
        // the clauses of receive, if and maybe ... else start at once (if: guards, counted as heads, conservatively)
        stack.push(new Open(t, true, false, t == ERL_RECEIVE || t == ERL_IF));
        continue;
      }
      if (t == ERL_FUN && i + 1 < body.size() && body.get(i + 1).type == ERL_PAR_LEFT) {
        stack.push(new Open(t, true, false, true));
        continue;
      }
      if (t == ERL_END) {
        Open open = stack.isEmpty() ? null : stack.pop();
        if (open == null || !open.block) plain = false;
        continue;
      }
      if (t == ERL_PAR_LEFT && i + 1 < body.size() && body.get(i + 1).type == ERL_PAR_RIGHT && !isCallee(body, i - 1)) {
        // round 6: "()" alone is no expression (erlc: syntax error before: ')'); the plugin's parser accepts it silently
        plain = false;
        continue;
      }
      if (t == ERL_PAR_LEFT || t == ERL_BRACKET_LEFT || t == ERL_CURLY_LEFT || t == ERL_BIN_START) {
        stack.push(new Open(t, false, t == ERL_CURLY_LEFT && isRecordBrace(body, i), top != null && top.head));
        continue;
      }
      if (t == ERL_PAR_RIGHT || t == ERL_BRACKET_RIGHT || t == ERL_CURLY_RIGHT || t == ERL_BIN_END) {
        Open open = stack.isEmpty() ? null : stack.pop();
        if (open == null || open.block || Run.matching(open.type) != t) plain = false;
        continue;
      }
      if (t == ERL_OF || t == ERL_CATCH || t == ERL_ELSE || t == ERL_SEMI) {
        if (!inBlock) plain = false;
        else top.head = true;
      }
      else if (t == ERL_ARROW || t == ERL_WHEN) {
        if (!inBlock) plain = false;
        else top.head = false;
      }
      else if (t == ERL_AFTER) {
        // receive ... after Timeout -> (an expression, counted as a head, conservatively); try ... after Body end
        if (!inBlock) plain = false;
        else top.head = top.type == ERL_RECEIVE;
      }
      else if (t == ERL_COMMA && stack.isEmpty()) plain = false;
      // round 6 (next), item 5: a map field (p => utf8string()) is no expression: its use goes through the copy
      else if (t == ERL_ASSOC && stack.isEmpty()) plain = false;
      else if (t == ERL_OP_OR && (top == null || top.type != ERL_BRACKET_LEFT)) plain = false;
      else if (t == ERL_OP_EQ && (top == null || !top.recordBrace || !isFieldNameBefore(body, i))) plain = false;
      else if (t == ERL_DOT && !(i >= 2 && body.get(i - 2).type == ERL_RADIX ||
                                 i >= 3 && body.get(i - 3).type == ERL_RADIX && body.get(i - 2).type == ERL_QMARK)) {
        plain = false;
      }
    }
    if (!plain) return false;
    if (!stack.isEmpty()) return false;
    if (BINARY_ONLY_OPERATORS.contains(body.get(0).type)) return false;
    if (ANY_OPERATOR_OR_DANGLING.contains(body.get(body.size() - 1).type)) return false;
    return !(body.size() >= 3 && body.get(0).type == ERL_OP_MINUS && body.get(1).type == ERL_ATOM_NAME &&
             body.get(2).type == ERL_PAR_LEFT);
  }

  /** The token at i can be followed by an argument list: a name, a quoted atom, or a closing bracket. */
  private static boolean isCallee(@NotNull List<Tok> body, int i) {
    if (i < 0) return false;
    IElementType t = body.get(i).type;
    return t == ERL_ATOM_NAME || t == ERL_VAR || t == ERL_SINGLE_QUOTE || t == ERL_PAR_RIGHT || t == ERL_CURLY_RIGHT ||
           t == ERL_BRACKET_RIGHT || t == ERL_BIN_END || t == ERL_END;
  }

  /**
   * The '=' at i follows a field name that starts a field of a record brace: {@code {name =}, {@code , name =}, with
   * the name an atom, a quoted atom, {@code _} or {@code ?NAME}. Any other '=' in a record brace is a match (for
   * example the second '=' of {@code #r{a = V = W}}), so the body is not plain.
   */
  private static boolean isFieldNameBefore(@NotNull List<Tok> body, int i) {
    int start;
    IElementType prev = i >= 1 ? body.get(i - 1).type : null;
    if (prev == ERL_ATOM_NAME || prev == ERL_VAR) {
      start = i - 1;
      if (i >= 2 && body.get(i - 2).type == ERL_QMARK) start = i - 2;
      else if (prev == ERL_VAR && !body.get(i - 1).text.equals("_")) return false;
    }
    else if (prev == ERL_SINGLE_QUOTE && i >= 3 && body.get(i - 2).type == ERL_ATOM_NAME &&
             body.get(i - 3).type == ERL_SINGLE_QUOTE) {
      start = i - 3;
    }
    else {
      return false;
    }
    if (start < 1) return false;
    IElementType before = body.get(start - 1).type;
    return before == ERL_CURLY_LEFT || before == ERL_COMMA;
  }

  /** The '{' at i opens a record: {@code #name{}, #'name'{}, #?NAME{}}. */
  private static boolean isRecordBrace(@NotNull List<Tok> body, int i) {
    if (i >= 2 && body.get(i - 1).type == ERL_ATOM_NAME && body.get(i - 2).type == ERL_RADIX) return true;
    if (i >= 4 && body.get(i - 1).type == ERL_SINGLE_QUOTE && body.get(i - 3).type == ERL_SINGLE_QUOTE &&
        body.get(i - 4).type == ERL_RADIX) {
      return true;
    }
    return i >= 3 && (body.get(i - 1).type == ERL_ATOM_NAME || body.get(i - 1).type == ERL_VAR) &&
           body.get(i - 2).type == ERL_QMARK && body.get(i - 3).type == ERL_RADIX;
  }

  /** Round 5: the name is the name of a directive ({@code -define}, {@code -include}, {@code -ifdef}, ...). */
  public static boolean isDirectiveName(@Nullable String attribute) {
    return attribute != null && DIRECTIVES.contains(attribute);
  }

  static final class Def {
    final String name;
    final int arity; // -1: no parentheses
    final List<String> params;
    final List<Tok> body;
    final String key;
    final boolean fragment;
    Branch branch;
    boolean fromHeader;

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
    /** Round 6 (next): for {@link Outcome#LIMIT_RECURSION}, the macro whose body comes back to itself. */
    final @Nullable String macro;

    LimitException(Outcome outcome) {
      this(outcome, null);
    }

    LimitException(Outcome outcome, @Nullable String macro) {
      super(null, null, false, false);
      this.outcome = outcome;
      this.macro = macro;
    }
  }

  private record Selection(@Nullable Def def, @NotNull Outcome outcome) {
  }

  /**
   * A branch of a conditional directive. Conditions are not evaluated. A definition made inside a branch is visible
   * only to uses inside the same branch, except in an {@code -ifndef(N)} branch that itself defines N (an include
   * guard or a default value): such a branch is "transparent".
   */
  static final class Branch {
    final String attribute;
    final String condition;
    final Branch parent;
    boolean transparent;
    /** Round 6 (next): the branch before this {@code -elif}/{@code -else} in the same conditional. */
    Branch previous;
    /** Round 6 (next): the definitions made directly in this branch. */
    final List<Def> defined = new ArrayList<>();

    Branch(String attribute, String condition, Branch parent) {
      this.attribute = attribute;
      this.condition = condition;
      this.parent = parent;
    }
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
    Branch branch;
    final @Nullable IncludeResolver resolver;
    int headerUses, headerAmbiguous, headerConditional;
    int replayDepth;
    int expandedDelta;
    /** The last {@link #handleUse} met a use that epp rejects (an empty argument, no ')', no definition of its arity). */
    boolean useRejected;
    /** Round 6: a nested use in the current form was left as it is (no definition to substitute). */
    boolean nestedLeft;

    Run(Base b, int formCap, @Nullable IncludeResolver resolver) {
      this.b = b;
      this.formCap = formCap;
      this.resolver = resolver;
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

    /** The work of the current form, and of the forms before it. */
    long work, fileWork;
    private int myTicks;

    /** Counts n steps of work; checks for cancellation; stops the use above the limits. */
    void tick(int n) throws LimitException {
      work += n;
      if ((++myTicks & 1023) == 0) ProgressManager.checkCanceled();
      if (work > ourWorkPerForm || fileWork + work > ourWorkPerFile) throw new LimitException(Outcome.LIMIT_TOKENS);
    }

    /** Adds the tokens to a result list within the limits. */
    void addAll(List<Tok> to, List<Tok> tokens) throws LimitException {
      tick(tokens.size());
      if (to.size() + tokens.size() > MAX_TOKENS_PER_FORM) throw new LimitException(Outcome.LIMIT_TOKENS);
      to.addAll(tokens);
    }

    void add(List<Tok> to, Tok token) throws LimitException {
      tick(1);
      if (to.size() >= MAX_TOKENS_PER_FORM) throw new LimitException(Outcome.LIMIT_TOKENS);
      to.add(token);
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
        boolean glued = prev != null && prev.kind == t.kind && t.kind != SRC_NONE && prev.end == t.start &&
                        (t.kind != SRC_HEADER || prev.origin instanceof HeaderTok a && t.origin instanceof HeaderTok c &&
                                                 a.header() == c.header() && a.directive() == c.directive());
        if (prev != null && !glued) out.text.append(' ');
        int es = out.text.length();
        out.text.append(t.text);
        out.segment(es, out.text.length(), t.start, t.end, t.kind, t.origin);
        prev = t;
      }
      out.text.append(' ');
    }

    void run() {
      int p = 0;
      while (p < sigCount) {
        ProgressManager.checkCanceled();
        int formStart = p;
        int formEnd = p;
        while (formEnd < sigCount - 1 && !isFormEnd(formEnd)) formEnd++;
        formSubstitutedTokens = 0;
        fileWork += work;
        work = 0;
        Form form = new Form(b.start[sig[formStart]], b.end[sig[formEnd]]);
        String attribute = attributeName(formStart, formEnd);
        boolean directive = attribute != null && DIRECTIVES.contains(attribute);
        List<Use> uses = new ArrayList<>();
        boolean ambiguous = false, refused = false, rejected = false, unknown = false;
        nestedLeft = false;
        boolean inBranch = branch != null;
        if (!directive) {
          for (int q = formStart; q <= formEnd; q++) {
            if ((q & 1023) == 0) ProgressManager.checkCanceled();
            if (st(q) != ERL_QMARK) continue;
            useRejected = false;
            Object r = handleUse(q, formEnd);
            rejected |= useRejected;
            if (r instanceof Use u) {
              uses.add(u);
              q = u.last;
            }
            else if (r == Outcome.AMBIGUOUS) ambiguous = true;
            if (r == Outcome.LIMIT_RECURSION && !inBranch && form.circularMacro == null) {
              // round 6 (next), item 5: epp's "circular macro", reported on the use by the expansion-error pass
              form.circularMacro = circularMacro;
              form.circularStart = circularStart;
              form.circularEnd = circularEnd;
            }
            else if (r instanceof Outcome o && o != Outcome.PREDEFINED && o != Outcome.NOT_IN_FILE) refused = true;
            else if (r == Outcome.NOT_IN_FILE) unknown = true;
          }
        }
        boolean complete = !ambiguous && !refused && !unknown && !nestedLeft && !inBranch;
        if (rejected && !uses.isEmpty()) {
          // epp rejects a use of this form, so erlc rejects the file: the form is not expanded, and its syntax
          // errors stay as master shows them
          outcomes.merge(Outcome.SUBSTITUTED, -uses.size(), Integer::sum);
          uses.clear();
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
            form.complete = complete;
            expandedDelta += size - (formEnd - formStart + 1);
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
        if (directive) {
          List<Tok> toks = new ArrayList<>(formEnd - formStart + 1);
          for (int q = formStart; q <= formEnd; q++) toks.add(tokAt(q, SRC_BODY));
          directive(attribute, toks, null);
        }
        p = formEnd + 1;
      }
      flushTo(b.text.length());
    }

    @Nullable String attributeName(int from, int to) {
      if (to - from < 1 || st(from) != ERL_OP_MINUS) return null;
      IElementType t = st(from + 1);
      if (t == ERL_ATOM_NAME) return stext(from + 1);
      String x = stext(from + 1); // keywords: -if, -else
      return "if".equals(x) || "else".equals(x) ? x : null;
    }

    /** One directive of the file (owner null) or of a replayed header. */
    void directive(String attribute, List<Tok> toks, @Nullable HeaderSummary owner) {
      switch (attribute) {
        case "define" -> define(toks, owner != null);
        case "undef" -> undef(toks);
        case "ifdef", "ifndef", "if" -> branch = new Branch(attribute, toks.size() > 3 ? toks.get(3).text() : "", branch);
        case "elif", "else" -> {
          if (branch != null) {
            Branch next = new Branch(attribute, "", branch.parent);
            next.previous = branch;
            branch = next;
          }
        }
        case "endif" -> {
          if (branch != null) {
            promoteAlternatives(branch);
            branch = branch.parent;
          }
        }
        case "include", "include_lib" -> {
          if (resolver == null || owner != null) return; // a header's own includes are replayed by replay()
          HeaderSummary h = resolver.resolve(null, attribute, new Directive(attribute, toks).includeString());
          if (h == null) incompleteIncludes = true;
          replay(h);
        }
        default -> {
        }
      }
    }

    /** Q2: an include before this point did not resolve. */
    boolean incompleteIncludes;

    void replay(@Nullable HeaderSummary h) {
      if (h == null || replayDepth > MAX_DEPTH) return;
      replayDepth++;
      try {
        for (Directive d : h.directives) {
          if (isInclude(d.attribute)) {
            HeaderSummary child = resolver.resolve(h, d.attribute, d.includeString());
            if (child == null) incompleteIncludes = true;
            replay(child);
            continue;
          }
          directive(d.attribute, d.tokens, h);
        }
      }
      finally {
        replayDepth--;
      }
    }

    boolean visible(Def d) {
      for (Branch x = d.branch; x != null; x = x.parent) {
        if (x.transparent) continue;
        boolean enclosing = false;
        for (Branch y = branch; y != null; y = y.parent) {
          if (y == x) {
            enclosing = true;
            break;
          }
        }
        if (!enclosing) return false;
      }
      return true;
    }

    Tok tokAt(int p, int kind) {
      int raw = sig[p];
      return new Tok(b.type[raw], b.text(raw), kind, b.start[raw], b.end[raw]);
    }

    static IElementType type(List<Tok> t, int p) {
      return p < t.size() ? t.get(p).type() : null;
    }

    void define(List<Tok> t, boolean fromHeader) {
      int to = t.size() - 1;
      int p = 3;
      if (p > to || type(t, 2) != ERL_PAR_LEFT) return;
      Name n = tokName(t, p);
      if (n == null) return;
      String name = n.name;
      p = n.last + 1;
      int arity = -1;
      List<String> params = new ArrayList<>();
      if (type(t, p) == ERL_PAR_LEFT) {
        p++;
        if (type(t, p) == ERL_PAR_RIGHT) {
          p++;
        }
        else {
          while (true) {
            if (type(t, p) != ERL_VAR) return;
            params.add(t.get(p).text());
            p++;
            if (type(t, p) == ERL_COMMA) {
              p++;
              continue;
            }
            if (type(t, p) == ERL_PAR_RIGHT) {
              p++;
              break;
            }
            return;
          }
        }
        arity = params.size();
      }
      int bodyEnd = type(t, to) == ERL_DOT ? to - 1 : to;
      if (type(t, bodyEnd) != ERL_PAR_RIGHT) return;
      List<Tok> body = new ArrayList<>();
      if (type(t, p) == ERL_COMMA) {
        body.addAll(t.subList(p + 1, bodyEnd));
      }
      else if (p != bodyEnd) {
        return;
      }
      if (PREDEFINED.contains(name)) return;
      Def def = new Def(name, arity, params, body, isFragment(body, params));
      def.branch = branch;
      def.fromHeader = fromHeader;
      // Q2: not after an include that does not resolve: the missing header may define the name, and then epp skips this
      if (branch != null && "ifndef".equals(branch.attribute) && name.equals(branch.condition) && !incompleteIncludes) branch.transparent = true;
      if (branch != null) branch.defined.add(def);
      defs.computeIfAbsent(name, k -> new ArrayList<>()).add(def);
    }

    /**
     * Round 6 (next), item 4: a conditional closed by {@code -else} covers every configuration. When each of its
     * branches defines a name with the same arity, one definition is made visible after {@code -endif}: the common one
     * when all bodies are the same (epp gives that text whatever the condition), else the one of the active branch
     * when {@code -ifdef(X)}/{@code -ifndef(X)} can be decided (X predefined or defined earlier outside any branch;
     * a name the file does not define may be a build flag, so that branch stays unknown and nothing is made visible).
     */
    void promoteAlternatives(Branch last) {
      if (!"else".equals(last.attribute)) return;
      List<Branch> chain = new ArrayList<>();
      for (Branch x = last; x != null; x = x.previous) chain.add(0, x);
      Map<String, Def[]> per = new LinkedHashMap<>();
      for (int i = 0; i < chain.size(); i++) {
        for (Def d : chain.get(i).defined) per.computeIfAbsent(d.name + "/" + d.arity, k -> new Def[chain.size()])[i] = d;
      }
      for (Def[] alternatives : per.values()) {
        boolean all = true, same = true;
        for (Def d : alternatives) {
          if (d == null) all = false;
          else if (!d.key.equals(alternatives[0] == null ? null : alternatives[0].key)) same = false;
        }
        if (!all) continue;
        Def chosen = same ? alternatives[0] : activeAlternative(chain, alternatives);
        if (chosen == null) continue;
        Def promoted = new Def(chosen.name, chosen.arity, chosen.params, chosen.body, chosen.fragment);
        promoted.branch = last.parent;
        promoted.fromHeader = chosen.fromHeader;
        if (last.parent != null) last.parent.defined.add(promoted);
        defs.computeIfAbsent(chosen.name, k -> new ArrayList<>()).add(promoted);
      }
    }

    private @Nullable Def activeAlternative(List<Branch> chain, Def[] alternatives) {
      if (chain.size() != 2) return null;
      Branch first = chain.get(0);
      boolean ifdef = "ifdef".equals(first.attribute);
      if (!ifdef && !"ifndef".equals(first.attribute)) return null;
      boolean defined = PREDEFINED.contains(first.condition);
      for (Def d : defs.getOrDefault(first.condition, List.of())) if (d.branch == null) defined = true;
      if (!defined) return null;
      return ifdef ? alternatives[0] : alternatives[1];
    }

    void undef(List<Tok> t) {
      if (branch != null || type(t, 2) != ERL_PAR_LEFT) return;
      Name n = tokName(t, 3);
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

    /** No definition of the name, visible or not, takes this many arguments: epp says "argument mismatch". */
    boolean noDefinitionWithArity(String name, int arity) {
      for (Def d : defs.getOrDefault(name, List.of())) if (d.arity == arity) return false;
      return true;
    }

    Selection select(String name, int arity) {
      if (PREDEFINED.contains(name)) return new Selection(null, Outcome.PREDEFINED);
      List<Def> all = defs.get(name);
      if (all == null || all.isEmpty()) return new Selection(null, Outcome.NOT_IN_FILE);
      List<Def> list = new ArrayList<>(all.size());
      for (Def d : all) if (visible(d)) list.add(d);
      if (list.isEmpty()) return new Selection(null, Outcome.CONDITIONAL);
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
        if (parsed == null || parsed.hasEmptyArgument()) {
          // no closing ')' in the form, or an empty argument: epp says "badly formed argument" or "argument mismatch"
          useRejected = true;
          count(Outcome.BAD_ARGUMENTS);
          return Outcome.BAD_ARGUMENTS;
        }
        args = parsed.args;
        arity = args.size();
        last = from + parsed.closeIndex;
      }
      Selection selection = select(name, arity);
      if (selection.def == null) {
        if (selection.outcome == Outcome.ARITY_MISMATCH && noDefinitionWithArity(name, arity)) useRejected = true;
        count(selection.outcome);
        if (selection.outcome == Outcome.AMBIGUOUS || selection.outcome == Outcome.CONDITIONAL) {
          boolean header = false;
          for (Def d : defs.get(name)) header |= d.fromHeader;
          if (header && selection.outcome == Outcome.AMBIGUOUS) headerAmbiguous++;
          if (header && selection.outcome == Outcome.CONDITIONAL) headerConditional++;
        }
        return selection.outcome == Outcome.CONDITIONAL ? Outcome.NOT_IN_FILE : selection.outcome;
      }
      Def def = selection.def;
      if (def.fromHeader) headerUses++;
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
        if (e.outcome == Outcome.LIMIT_RECURSION) {
          circularMacro = e.macro;
          circularStart = b.start[sig[q]];
          circularEnd = b.end[sig[last]];
        }
        return e.outcome;
      }
    }

    /** Round 6 (next): the last use that {@link #handleUse} refused as circular, and the macro of the cycle. */
    @Nullable String circularMacro;
    int circularStart, circularEnd;

    List<Tok> expand(Def def, @Nullable List<List<Tok>> args, int depth, Deque<String> active) throws LimitException {
      tick(1 + def.body.size());
      if (depth > MAX_DEPTH) throw new LimitException(Outcome.LIMIT_DEPTH);
      String activeKey = def.name + "/" + def.arity;
      if (active.contains(activeKey)) throw new LimitException(Outcome.LIMIT_RECURSION, def.name);
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
            add(substituted, new Tok(ERL_STRING, stringify(args.get(index))));
            i += 2;
            continue;
          }
        }
        if (args != null && t.type == ERL_VAR) {
          int index = def.params.indexOf(t.text);
          if (index >= 0) {
            addAll(substituted, expandedArgs.get(index));
            continue;
          }
        }
        add(substituted, t);
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
          add(out, t);
          add(out, tokens.get(++i));
          continue;
        }
        Name n = t.type == ERL_QMARK ? tokName(tokens, i + 1) : null;
        if (n == null) {
          add(out, t);
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
          if (selection.outcome != Outcome.PREDEFINED) nestedLeft = true;
          add(out, t);
          continue; // left as it is; the name follows as a plain token
        }
        Def def = selection.def;
        List<Tok> expanded = expand(def, def.arity == -1 ? null : args, depth + 1, active);
        addAll(out, expanded);
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
    /** {@code ?M(, A)}, {@code ?M(A, )}, {@code ?M(A, , B)}: epp rejects an empty argument. */
    boolean hasEmptyArgument() {
      for (List<Tok> a : args) if (a.isEmpty()) return true;
      return false;
    }
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
