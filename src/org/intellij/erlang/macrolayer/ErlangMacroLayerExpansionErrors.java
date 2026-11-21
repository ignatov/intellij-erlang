package org.intellij.erlang.macrolayer;

import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosBody;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.ErlangMacros;
import org.intellij.erlang.psi.ErlangMacrosArg;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.intellij.erlang.psi.ErlangParenthesizedExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Round 6: a macro use whose expansion erlc rejects is an error. The main tree parses {@code ?PAR,} with
 * {@code -define(PAR, ())} as a plain expression, but epp gives {@code (),}: "syntax error before: ')'".
 * <p>
 * Shown only when the layer is sure: the top-level form has no syntax error in the main tree, or only errors next to
 * bare macro uses, which the error filter then hides (a form being typed is left alone), the round-5 gate sends it to the copy (a form with only plain macros pays nothing), its expansion is
 * complete (every use, nested ones too, was substituted; no ambiguous or conditional definition; not in an
 * {@code -ifdef} branch; under the size cap; the file's includes resolve), and its expanded text does not parse
 * (a syntax error of the copy, or an empty {@code ()} that the plugin's parser accepts silently), and every macro the
 * use goes through has a body that the main tree could parse (not raw tokens: see {@link #bodiesParsed}). The
 * error goes on the outermost macro use in the edited file that made the broken token, or on the argument token when
 * the broken token came from an argument; never into a header.
 */
public final class ErlangMacroLayerExpansionErrors {
  private static final TokenSet PLAIN = TokenSet.create(ErlangTypes.ERL_ATOM_NAME, ErlangTypes.ERL_VAR, ErlangTypes.ERL_STRING,
                                                        ErlangTypes.ERL_INTEGER, ErlangTypes.ERL_FLOAT, ErlangTypes.ERL_CHAR);

  private ErlangMacroLayerExpansionErrors() {
  }

  /** One error: the range in the edited file and the message. */
  public record Problem(@NotNull TextRange range, @NotNull String message) {
  }

  /**
   * The expansion errors of the file, one per expanded form at most; null when a form needs the copy and the layer
   * gives none now (refused while the file is typed in, or a table not built yet). Forms the round-5 gate sends to the
   * copy are checked, and, with {@link #checksPlainForms()}, also the forms whose macro uses are all plain.
   */
  /**
   * Round 6 (next), item 3f: the pass may find a problem in the file: the layer is on, it is a layer file, and its table
   * (the valid one, else the last one; never computed here) has a form that needs the copy, or is not known yet. Cheap:
   * the platform asks it when it creates the passes of a highlighting.
   */
  public static boolean mayHaveProblems(@NotNull PsiFile file) {
    if (!ErlangMacroLayer.isEnabled()) return false;
    ErlangFile f = ErlangMacroLayer.layerFile(file);
    if (f == null) return false;
    if (checksPlainForms()) return true;
    // the daemon creates its passes off the UI thread: the table of this version (the main highlighting needs it too);
    // on the UI thread only a table that is there
    ErlangMacroLayer.Table t = com.intellij.openapi.application.ApplicationManager.getApplication().isDispatchThread() || DumbService.isDumb(f.getProject())
                               ? ErlangMacroLayer.cachedTable(f) : ErlangMacroLayer.table(f);
    if (t != null) return t.refusal == null && !t.names.isEmpty() && ErlangMacroLayer.anyFormNeedsCopy(f, t) && hasCandidateForm(f, t);
    Boolean known = ErlangMacroLayer.anyFormNeedsCopyNoWait(f);
    return known == null || known;
  }

  /**
   * Some form is one that {@link #problems} checks: it needs the copy and has no syntax error of its own (a form that
   * needs the copy only because it is broken, as an unfinished call in erl_types.erl, gives the pass nothing to check).
   */
  private static boolean hasCandidateForm(@NotNull ErlangFile file, @NotNull ErlangMacroLayer.Table t) {
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof PsiErrorElement || c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      ProgressManager.checkCanceled();
      if (ErlangMacroLayer.formNeedsCopy(t, c) && onlyMacroErrors(c)) return true;
    }
    return false;
  }

  public static @Nullable List<Problem> problems(@NotNull ErlangFile file) {
    if (!ErlangMacroLayer.isEnabled() || ErlangMacroLayer.layerFile(file) == null || DumbService.isDumb(file.getProject())) return List.of();
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(file);
    if (t == null) return null;
    if (t.refusal != null || t.names.isEmpty()) return List.of();
    boolean plain = checksPlainForms();
    if (!plain && !ErlangMacroLayer.anyFormNeedsCopy(file, t)) return List.of();
    List<PsiElement> forms = new ArrayList<>();
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof PsiErrorElement || c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      ProgressManager.checkCanceled();
      boolean candidate = ErlangMacroLayer.formNeedsCopy(t, c) || plain && ErlangMacroLayer.formUsesMacro(t, c);
      // a form with a syntax error of its own (being typed) is left alone, and asks for no copy
      if (candidate && onlyMacroErrors(c)) forms.add(c);
    }
    if (forms.isEmpty()) return List.of();
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(file, "expansion-errors");
    if (copy == null) return null;
    // round 6 (next): a file whose only uses are circular has no expanded form, so no parsed copy, and still errors
    if (copy.copyFile == null && copy.expansion.forms.stream().noneMatch(f -> f.circularMacro != null)) return List.of();
    Set<Problem> r = new LinkedHashSet<>();
    for (PsiElement form : forms) {
      ProgressManager.checkCanceled();
      Problem p = expansionProblem(form, file, copy);
      if (p != null) r.add(p);
    }
    return List.copyOf(r);
  }

  /**
   * The expansion error of the expanded form that contains this top-level form of the main tree, cached on the copy.
   * The main tree may split one form of the source into several (it ends a function at {@code foo?PAR,}); the error
   * may then lie in another piece.
   */
  private static @Nullable Problem expansionProblem(@NotNull PsiElement form, @NotNull ErlangFile file, @NotNull ErlangMacroLayer.Copy copy) {
    if (!onlyMacroErrors(form)) return null;
    Form f = copy.expansion.formAtSource(form.getTextRange().getStartOffset());
    if (f != null && f.circularMacro != null && form.getTextRange().contains(f.circularStart)) {
      // round 6 (next), item 5: epp's "circular macro", on the use
      String use = copy.expansion.sourceText.subSequence(f.circularStart, f.circularEnd).toString();
      java.util.regex.Matcher m = java.util.regex.Pattern.compile("\\?\\s*('[^']*'|[A-Za-z_@][A-Za-z0-9_@]*)").matcher(use);
      boolean found = m.lookingAt();
      String name = found ? m.group(1) : f.circularMacro;
      return new Problem(TextRange.create(f.circularStart, f.circularStart + (found ? m.end() : use.length())),
                         "The expansion of ?" + name + " is not valid Erlang: circular macro '" + f.circularMacro + "'");
    }
    if (copy.copyFile == null || f == null || f.status != ErlangMacroExpander.FormStatus.EXPANDED || !f.complete) return null;
    String key = "x" + f.srcStart;
    Object cached = copy.answers().get(key);
    if (cached != null) return cached instanceof Problem p ? p : null;
    Problem p = compute(copy, file, f);
    copy.answers().put(key, p == null ? NONE : p);
    ErlangMacroLayerLog.query("expansion-errors", file.getName());
    return p;
  }

  /**
   * Round 6: {@code erlang.macro.layer.red.plain} / {@code ERLANG_MACRO_LAYER_RED_PLAIN} = {@code on}: the pass also checks
   * the forms whose macros are all plain. Off by default: it builds a copy for files that need none otherwise
   * ({@code erl_types.erl}: about 350 ms per pass, measured), for one rare case (a plain body that an argument breaks).
   */
  private static final boolean PLAIN_DEFAULT = "on".equals(ErlangMacroLayer.read("erlang.macro.layer.red.plain", "ERLANG_MACRO_LAYER_RED_PLAIN"));
  private static volatile @Nullable Boolean ourTestPlain;

  /** The separate pass also checks forms whose macro uses are all plain (it then builds the copy for such files). */
  public static boolean checksPlainForms() {
    Boolean b = ourTestPlain;
    return b != null ? b : PLAIN_DEFAULT;
  }

  @org.jetbrains.annotations.TestOnly
  public static void setChecksPlainFormsForTests(boolean on, @NotNull com.intellij.openapi.Disposable parent) {
    Boolean previous = ourTestPlain;
    ourTestPlain = on;
    com.intellij.openapi.util.Disposer.register(parent, () -> ourTestPlain = previous);
  }

  private static final Object NONE = new Object();

  private static volatile boolean ourCheckBodies = true;

  /** Measurement only: shows the errors of raw bodies too (the corpus count of what {@link #bodiesParsed} removes). */
  @org.jetbrains.annotations.TestOnly
  public static void setCheckBodiesForTests(boolean check, @NotNull com.intellij.openapi.Disposable parent) {
    ourCheckBodies = check;
    com.intellij.openapi.util.Disposer.register(parent, () -> ourCheckBodies = true);
  }

  /**
   * The main-tree error is next to a bare macro use (its '?', its name or the ')' of its arguments) in a form that
   * gets an expansion error: the expansion error tells what is wrong, so the error filter hides this one.
   */
  public static boolean isReplacedByExpansionError(@NotNull PsiErrorElement error) {
    if (!ErlangMacroLayer.isEnabled() || !isMacroAdjacent(error)) return false;
    PsiElement form = ErlangMacroLayer.topForm(error);
    ErlangFile file = ErlangMacroLayer.layerFile(error);
    if (form == null || form instanceof PsiErrorElement || file == null || DumbService.isDumb(file.getProject())) return false;
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(file);
    if (t == null || t.refusal != null || !ErlangMacroLayer.formNeedsCopy(t, form)) return false;
    // round 6 (next): a trigger of its own, so that a build with the trigger "expansion-errors" is one of the pass
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(file, "expansion-errors-filter");
    return copy != null && expansionProblem(form, file, copy) != null;
  }

  /** The form has no syntax error in the main tree, or only errors next to bare macro uses (not a form being typed). */
  private static boolean onlyMacroErrors(@NotNull PsiElement form) {
    for (PsiErrorElement e : PsiTreeUtil.findChildrenOfType(form, PsiErrorElement.class)) {
      if (!isMacroAdjacent(e)) return false;
    }
    return true;
  }

  private static boolean isMacroAdjacent(@NotNull PsiErrorElement error) {
    PsiElement first = error.getTextLength() > 0 ? firstVisibleLeaf(error) : null;
    if (first != null && isBareMacroPart(first)) return true;
    PsiElement prev = PsiTreeUtil.prevVisibleLeaf(error);
    return prev != null && isBareMacroPart(prev);
  }

  /** A token of a macro use that the parser left bare: its '?', its name, or the ')' that closes its arguments. */
  private static boolean isBareMacroPart(@NotNull PsiElement leaf) {
    IElementType type = leaf.getNode().getElementType();
    if (type == ErlangTypes.ERL_QMARK) return isBareQuestionMark(leaf);
    if (type == ErlangTypes.ERL_VAR || type == ErlangTypes.ERL_ATOM_NAME) {
      PsiElement prev = PsiTreeUtil.prevVisibleLeaf(leaf);
      return prev != null && prev.getNode().getElementType() == ErlangTypes.ERL_QMARK && isBareQuestionMark(prev) &&
             PsiTreeUtil.getParentOfType(leaf, ErlangMacrosName.class) == null;
    }
    if (type == ErlangTypes.ERL_PAR_RIGHT) {
      int depth = 0;
      for (PsiElement l = leaf; l != null; l = PsiTreeUtil.prevVisibleLeaf(l)) {
        IElementType t = l.getNode().getElementType();
        if (t == ErlangTypes.ERL_PAR_RIGHT) depth++;
        else if (t == ErlangTypes.ERL_PAR_LEFT && --depth == 0) {
          PsiElement name = PsiTreeUtil.prevVisibleLeaf(l);
          return name != null && name.getNode().getElementType() != ErlangTypes.ERL_PAR_RIGHT && isBareMacroPart(name);
        }
        else if (t == ErlangTypes.ERL_DOT) return false;
      }
    }
    return false;
  }

  private static boolean isBareQuestionMark(@NotNull PsiElement q) {
    PsiElement parent = q.getParent();
    return !(parent instanceof ErlangMacros) && !(parent instanceof ErlangMacrosArg);
  }

  private static @Nullable Problem compute(@NotNull ErlangMacroLayer.Copy copy, @NotNull ErlangFile file, @NotNull Form f) {
    // every piece of the form in the main tree: no error but next to a bare macro use
    for (PsiElement leaf = file.findElementAt(f.srcStart); leaf != null; ) {
      PsiElement piece = ErlangMacroLayer.topForm(leaf);
      if (piece == null || piece.getTextRange().getStartOffset() > f.srcEnd) break;
      if (piece instanceof PsiErrorElement e ? !isMacroAdjacent(e) : !onlyMacroErrors(piece)) return null;
      leaf = PsiTreeUtil.nextLeaf(piece);
    }
    PsiElement token = firstBrokenToken(copy, f);
    if (token == null || token.getTextRange().getStartOffset() > f.expEnd) return null;
    ErlangMacroExpander.Expansion x = copy.expansion;
    int at = token.getTextRange().getStartOffset();
    int kind = x.kindAt(at);
    // only a token that a macro made (from a body or an argument): an error at a copied token of the file may be the
    // file's own (erlc then reports it with or without the macro; the main tree's checks may already show it there)
    TextRange use;
    if (kind == ErlangMacroExpander.SRC_VERBATIM || kind < 0) {
      // except the '.' that ends the form: the expansion left the form unfinished ("syntax error before: '.'"), and
      // nothing of the file's own was wrong there (the main tree had no error); the last use before it did it
      if (token.getNode().getElementType() != ErlangTypes.ERL_DOT || token.getTextRange().getEndOffset() < f.expEnd) return null;
      use = useBefore(x, f, at);
    }
    else {
      use = x.useSourceRange(at);
    }
    TextRange range = kind == ErlangMacroExpander.SRC_ARGUMENT ? x.toSource(at, token.getTextRange().getEndOffset()) : use;
    if (use == null) return null;
    if (use == null || range == null || range.isEmpty() || range.getStartOffset() < f.srcStart || range.getEndOffset() > f.srcEnd) return null;
    String name = macroName(x.sourceText, use);
    if (name == null) return null;
    if (ourCheckBodies && !bodiesParsed(file, use, name)) return null;
    String message = "The expansion of ?" + name + " is not valid Erlang: syntax error before: " + present(token);
    return new Problem(range, message);
  }

  /**
   * The token before which the expanded form stops being Erlang: the first syntax error of the copy in the form (an
   * error just after it counts for it, as in the copy), or the ')' of an empty parenthesized expression, which the
   * plugin's parser accepts silently and erlc rejects.
   */
  /**
   * Round 6 (next), item 5: the first {@code =>} of the element whose innermost open bracket is not {@code #{}: erlc
   * rejects it ("syntax error before: '=>'"), the plugin's parser accepts it (39 missed mutants of map-field macros,
   * {@code -define(P, p => utf8string())} used outside a map). In Erlang {@code =>} stands only directly inside
   * {@code #{...}} (maps, map updates, map comprehensions, map patterns and map types).
   */
  private static @Nullable PsiElement misplacedAssociation(@NotNull PsiElement e) {
    Deque<Boolean> open = new ArrayDeque<>();
    int end = e.getTextRange().getEndOffset();
    for (PsiElement leaf = PsiTreeUtil.firstChild(e); leaf != null && leaf.getTextRange().getStartOffset() < end; leaf = PsiTreeUtil.nextLeaf(leaf)) {
      IElementType t = leaf.getNode().getElementType();
      if (t == ErlangTypes.ERL_CURLY_LEFT) {
        PsiElement prev = PsiTreeUtil.prevVisibleLeaf(leaf);
        open.push(prev != null && prev.getNode().getElementType() == ErlangTypes.ERL_RADIX);
      }
      else if (t == ErlangTypes.ERL_PAR_LEFT || t == ErlangTypes.ERL_BRACKET_LEFT || t == ErlangTypes.ERL_BIN_START) open.push(false);
      else if (t == ErlangTypes.ERL_CURLY_RIGHT || t == ErlangTypes.ERL_PAR_RIGHT || t == ErlangTypes.ERL_BRACKET_RIGHT || t == ErlangTypes.ERL_BIN_END) {
        if (!open.isEmpty()) open.pop();
      }
      else if (t == ErlangTypes.ERL_ASSOC && (open.isEmpty() || !open.peek())) return leaf;
    }
    return null;
  }

  private static @Nullable PsiElement firstBrokenToken(@NotNull ErlangMacroLayer.Copy copy, @NotNull Form f) {
    PsiElement best = null;
    PsiElement start = copy.copyFile.findElementAt(f.expStart);
    PsiElement top = start == null ? null : ErlangMacroLayer.topForm(start);
    for (PsiElement e = top; e != null && e.getTextRange().getStartOffset() <= f.expEnd; e = e.getNextSibling()) {
      List<PsiElement> candidates = new ArrayList<>();
      if (e instanceof PsiErrorElement err) candidates.add(tokenOf(err));
      for (PsiErrorElement err : PsiTreeUtil.findChildrenOfType(e, PsiErrorElement.class)) candidates.add(tokenOf(err));
      for (ErlangParenthesizedExpression p : PsiTreeUtil.findChildrenOfType(e, ErlangParenthesizedExpression.class)) {
        if (p.getExpression() == null) candidates.add(p.getLastChild());
      }
      candidates.add(misplacedAssociation(e));
      for (PsiElement c : candidates) {
        if (c != null && (best == null || c.getTextRange().getStartOffset() < best.getTextRange().getStartOffset())) best = c;
      }
      if (best != null) break;
    }
    return best;
  }

  private static @Nullable PsiElement tokenOf(@NotNull PsiErrorElement error) {
    return error.getTextLength() > 0 ? firstVisibleLeaf(error) : PsiTreeUtil.nextVisibleLeaf(error);
  }

  private static @Nullable PsiElement firstVisibleLeaf(@NotNull PsiElement e) {
    PsiElement leaf = PsiTreeUtil.firstChild(e);
    while (leaf != null && (leaf instanceof PsiWhiteSpace || leaf instanceof PsiComment || leaf.getTextLength() == 0)) {
      leaf = PsiTreeUtil.nextLeaf(leaf);
    }
    return leaf;
  }

  /** The nearest macro use before the expanded offset in the form. */
  private static @Nullable TextRange useBefore(@NotNull ErlangMacroExpander.Expansion x, @NotNull Form f, int at) {
    for (int o = at - 1; o >= f.expStart; o--) {
      int k = x.kindAt(o);
      if (k >= 0 && k != ErlangMacroExpander.SRC_VERBATIM) return x.useSourceRange(o);
    }
    return null;
  }

  /**
   * Every macro the use goes through (its own definition and, transitively, the macros of its body) has a body that
   * the main tree parsed as expressions or typed fields, or a raw body without the tokens of the parser's known gaps
   * ({@link #rawBodyIsReadable}). The plugin's parser falls back to raw tokens when it cannot read a body; a parse
   * error of such a body's expansion may then be one of the parser's own gaps, not erlc's.
   */
  private static boolean bodiesParsed(@NotNull ErlangFile file, @NotNull TextRange use, @NotNull String name) {
    PsiElement q = file.findElementAt(use.getStartOffset());
    PsiElement n = q == null ? null : PsiTreeUtil.nextVisibleLeaf(q);
    if (n != null && n.getNode().getElementType() == ErlangTypes.ERL_SINGLE_QUOTE) n = PsiTreeUtil.nextVisibleLeaf(n);
    if (n == null) return false;
    return bodiesParsed(file, name, ErlangMacroResolve.arityAfter(n), new HashSet<>(), 0);
  }

  private static boolean bodiesParsed(@NotNull ErlangFile file, @NotNull String name, int arity, @NotNull Set<String> seen, int depth) {
    if (ErlangMacroExpander.PREDEFINED.contains(name) || !seen.add(name + "/" + arity)) return true;
    if (depth > ErlangMacroExpander.MAX_DEPTH) return false;
    ErlangMacrosDefinition d = ErlangMacroResolve.resolve(file, name, arity);
    ErlangMacrosBody body = d == null ? null : d.getMacrosBody();
    if (body == null) return false;
    if (PsiTreeUtil.getChildOfType(body, ErlangCompositeElement.class) == null && !rawBodyIsReadable(body)) return false;
    for (ErlangMacros m : PsiTreeUtil.findChildrenOfType(body, ErlangMacros.class)) {
      ErlangMacrosName mn = m.getMacrosName();
      if (mn == null) return false;
      PsiElement id = ErlangPsiImplUtil.getNameIdentifier(mn);
      String nested = id.getText();
      if (!bodiesParsed(file, nested, ErlangMacroResolve.arityAfter(id), seen, depth + 1)) return false;
    }
    return true;
  }

  /**
   * A body the parser keeps as raw tokens (a fragment: a separator, clauses, map fields, ...) without the tokens where
   * the parser's own gaps were found on the corpus: a based integer with '_' ({@code 16#FFFF_FFFF}) and char literals
   * ({@code $\)}, {@code $;}, ...). Of the 16 false errors that raw bodies gave on 6,017 corpus files, all 16 came from
   * these two kinds of token.
   */
  private static boolean rawBodyIsReadable(@NotNull ErlangMacrosBody body) {
    for (PsiElement leaf = PsiTreeUtil.firstChild(body); leaf != null && PsiTreeUtil.isAncestor(body, leaf, false); leaf = PsiTreeUtil.nextLeaf(leaf)) {
      IElementType t = leaf.getNode().getElementType();
      if (t == ErlangTypes.ERL_CHAR) return false;
      // the lexer reads 16#FFFF_FFFF as 16#FFFF and a variable _FFFF: any based integer counts
      if (t == ErlangTypes.ERL_INTEGER && (leaf.getText().contains("#") || leaf.getText().contains("_"))) return false;
      if (t == com.intellij.psi.TokenType.BAD_CHARACTER) return false;
    }
    return true;
  }

  private static @Nullable String macroName(@NotNull CharSequence text, @NotNull TextRange use) {
    String s = use.subSequence(text).toString();
    if (!s.startsWith("?")) return null;
    int i = 1;
    while (i < s.length() && Character.isWhitespace(s.charAt(i))) i++;
    if (i < s.length() && s.charAt(i) == '\'') {
      int j = s.indexOf('\'', i + 1);
      return j > i ? s.substring(i + 1, j) : null;
    }
    int j = i;
    while (j < s.length() && (Character.isLetterOrDigit(s.charAt(j)) || s.charAt(j) == '_' || s.charAt(j) == '@')) j++;
    return j > i ? s.substring(i, j) : null;
  }

  /** The token as erlc prints it after "syntax error before:". */
  private static @NotNull String present(@NotNull PsiElement token) {
    IElementType type = token.getNode().getElementType();
    String text = token.getText();
    return PLAIN.contains(type) ? text : "'" + text + "'";
  }
}
