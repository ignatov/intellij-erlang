package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementDecorator;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import org.intellij.erlang.completion.ErlangCompletionContributor;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Completion in a part of a form that the main tree lost. In
 * {@code f(Abc1, Abc2) -> foo?PAR, X1 = Ab<caret>} the main tree's error recovery after {@code ?PAR} puts the rest of
 * the clause into dummy blocks, so the Erlang completion providers find no clause, no expression and no variable there.
 * The expanded copy of the file parses that clause. This contributor runs first. When the main tree lost the caret's
 * place (the caret's top-level element is a bare token, an error, a dummy block, a form with a syntax error, or the
 * rest of a form that the recovery split), the text from the start of that broken form to the caret uses a macro the
 * file sees, the caret is not in a macro name (the main tree completes those), and the copy's form there is expanded,
 * it inserts the completion's dummy identifier into the copy's text at the caret's place, parses that text, and runs
 * the remaining contributors (the plugin's own providers) at the dummy identifier there instead of at the main tree's
 * position. Their lookup elements are passed on with real targets: a
 * function, record or type of the parsed text is replaced by the real element with the same name (in the main tree, or
 * the name that a macro makes), and any other element of the parsed text is dropped from the lookup element (the
 * lookup string stays). Elsewhere it does nothing.
 * <p>
 * The copy is the layer's copy of the real file, under the layer's build rule: an explicit completion builds it if it
 * is not there (cancellably); the auto-popup is a passive query: while the file is typed in it waits for the copy that
 * the daemon builds and starts no build (see {@code ErlangMacroLayerCopies.awaitWhileTyped}). The cost per completion in such
 * a form is one parse of the expanded text with the dummy identifier (the keyword provider already parses the text
 * before the caret once per completion).
 */
public final class ErlangMacroLayerCompletionContributor extends CompletionContributor {
  /** On the parsed text: the main tree's original position and the caret offset in the real file. */
  public record Origin(@Nullable PsiElement originalPosition, int offset) {
  }

  public static final Key<Origin> ORIGIN = Key.create("erlang.macro.layer.completion.origin");

  /** Completions answered through the copy, for tests and measurements. */
  static final AtomicInteger TAKEN_OVER = new AtomicInteger();
  /** Nanoseconds of the last parse of the copy's text with the dummy identifier (tests and measurements). */
  static volatile long ourLastParseNanos;

  /** The real original position of a completion that runs at a position in the layer's parsed text, else the platform's. */
  public static @Nullable PsiElement originalPosition(@NotNull CompletionParameters parameters) {
    Origin o = parameters.getPosition().getContainingFile().getUserData(ORIGIN);
    return o != null ? o.originalPosition() : parameters.getOriginalPosition();
  }

  /** The caret offset in the real file (see {@link #originalPosition}). */
  public static int originalOffset(@NotNull CompletionParameters parameters) {
    Origin o = parameters.getPosition().getContainingFile().getUserData(ORIGIN);
    return o != null ? o.offset() : parameters.getOffset();
  }

  @Override
  public void fillCompletionVariants(@NotNull CompletionParameters parameters, @NotNull CompletionResultSet result) {
    if (parameters.getCompletionType() != CompletionType.BASIC || !ErlangMacroLayer.isEnabled()) return;
    if (parameters.getPosition().getContainingFile().getUserData(ORIGIN) != null) return; // already the layer's text
    boolean auto = parameters.isAutoPopup();
    long t0 = System.nanoTime();
    if (!mayTakeOver(parameters)) {
      Made made = made(parameters);
      if (made != null && !made.isEmpty()) addMadeNames(parameters, result, made);
      return;
    }
    // The main tree's items first, as fast as on master; the copy's items after them
    // the main tree's items, by name and target: a copy item is dropped only when the main tree has the same one
    // (bar/0 and bar/1 have the same name and both stay)
    Set<Object> seen = new HashSet<>();
    result.runRemainingContributors(parameters, r -> {
      seen.add(key(r.getLookupElement()));
      result.passResult(r);
    });
    ourLastMainNanos = System.nanoTime() - t0;
    // The auto-popup too builds the copy if it is not there, under the completion's own indicator
    // (the next key cancels it): as a passive query it waited for the daemon's copy and opened empty for about 1 s
    Position p = position(parameters);
    if (p != null) {
      TAKEN_OVER.incrementAndGet();
      result.runRemainingContributors(parameters.withPosition(p.position, p.offset), r -> {
        LookupElement real = realLookup(p, r.getLookupElement());
        if (!seen.contains(key(real))) result.passResult(r.withLookupElement(real));
      });
    }
    ourLastTotalNanos = System.nanoTime() - t0;
    result.stopHere();
  }

  /** An item's name and, when it has one, its target. */
  private static @NotNull Object key(@NotNull LookupElement e) {
    PsiElement psi = e.getPsiElement();
    return psi == null ? e.getLookupString() : Arrays.asList(e.getLookupString(), psi);
  }

  /** Nanoseconds to the main tree's items and to the end of the last completion that could take over (tests, measurements). */
  static volatile long ourLastMainNanos, ourLastTotalNanos;

  /**
   * The cheap part of {@link #position}: the caret's place in the main tree may be lost (no table, no copy asked):
   * its top-level element is a bare token, an error or a dummy block, the rest of a split form, or a form with a
   * syntax error before the caret. A syntax error after the caret does not count: there the main tree's position is
   * whole, and no copy is parsed.
   */
  private static boolean mayTakeOver(@NotNull CompletionParameters parameters) {
    if (!(parameters.getOriginalFile() instanceof ErlangFile real) || ErlangMacroLayer.layerFile(real) != real) return false;
    int offset = parameters.getOffset();
    PsiElement at = real.findElementAt(Math.max(0, offset - 1));
    PsiElement top = at == null ? null : ErlangMacroLayer.topForm(at);
    if (top == null || isMacroNamePosition(at)) return false;
    if (!(top instanceof ErlangCompositeElement) || !afterADot(top)) return true;
    for (PsiErrorElement e : PsiTreeUtil.findChildrenOfType(top, PsiErrorElement.class)) {
      if (e.getTextRange().getStartOffset() < offset) return true;
    }
    return boundByMacrosCandidate(real, top);
  }

  /** The form may bind a variable through a macro (see {@link #boundByMacros}); a cached table only. */
  private static boolean boundByMacrosCandidate(@NotNull ErlangFile real, @NotNull PsiElement top) {
    ErlangMacroLayer.Table t = ErlangMacroLayer.cachedTable(real);
    return t != null && t.refusal == null && !t.names.isEmpty() && ErlangMacroLayer.formNeedsCopy(t, top) &&
           !(ErlangMacroLayer.formFreeBodyVars(t, top).isEmpty() && ErlangMacroLayer.substitutedArgumentVars(t, top).isEmpty());
  }

  // ------------------------------------------------------------------------------------------------ made names

  /**
   * The functions (by name/arity), records and record fields that only the expansions of the
   * file make, with their real targets (null when the list is one edit old). Kept on the file from the last copy
   * (strings only, see {@link #MADE_NAMES}).
   */
  record Made(@NotNull Map<String, PsiElement> functions, @NotNull Map<String, PsiElement> records,
              @NotNull Map<String, List<String>> fields) {
    boolean isEmpty() {
      return functions.isEmpty() && records.isEmpty();
    }

    static @NotNull Made of(@NotNull ErlangMacroLayer.Copy copy) {
      Map<String, List<String>> fields = new LinkedHashMap<>();
      Map<String, PsiElement> records = copy.recordsMadeByMacros();
      if (copy.copyFile != null) {
        for (String name : records.keySet()) {
          ErlangRecordDefinition d = copy.copyFile.getRecord(name);
          ErlangTypedRecordFields f = d == null ? null : d.getTypedRecordFields();
          if (f == null) continue;
          List<String> names = new ArrayList<>();
          for (ErlangTypedExpr t : f.getTypedExprList()) names.add(t.getQAtom().getText());
          for (ErlangQAtom q : PsiTreeUtil.getChildrenOfTypeAsList(f, ErlangQAtom.class)) names.add(q.getText());
          fields.put(name, names);
        }
      }
      return new Made(copy.functionsMadeByMacros(), records, fields);
    }

    @NotNull Made namesOnly() {
      Map<String, PsiElement> fn = new LinkedHashMap<>(), rec = new LinkedHashMap<>();
      for (String k : functions.keySet()) fn.put(k, null);
      for (String k : records.keySet()) rec.put(k, null);
      return new Made(fn, rec, fields);
    }
  }

  /**
   * The names only, from the last copy of the file: what the auto-popup uses when the copy of this version is not
   * there (the names that top-level macro forms make rarely change while one types inside a function). A few strings
   * per file that has a top-level macro form.
   */
  static final Key<Made> MADE_NAMES = Key.create("erlang.macro.layer.completion.made.names");

  /**
   * The names that expansions make in the completion's file, or null. The auto-popup too builds the table and the copy
   * if they are not there, under the completion's own indicator (the next key cancels it), as the take-over path does:
   * the cached ones are stale after every typed key, so it offered no made name (#1537).
   */
  private static @Nullable Made made(@NotNull CompletionParameters parameters) {
    ErlangFile real = ErlangMacroLayer.layerFile(parameters.getOriginalFile());
    if (real == null || real != parameters.getOriginalFile()) return null;
    boolean auto = parameters.isAutoPopup();
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(real);
    if (t == null) return auto ? real.getUserData(MADE_NAMES) : null;
    if (t.refusal != null || t.names.isEmpty()) return null;
    // only a top-level form that starts with a macro use, or a broken one, can make a name (and then it needs the copy)
    if (!ErlangMacroLayer.anyFormNeedsCopy(real, t) || !ErlangMacroLayer.hasTopLevelMacroForm(real, t)) return null;
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(real, "completion-names");
    if (copy == null) return auto ? real.getUserData(MADE_NAMES) : null;
    Made m = Made.of(copy);
    real.putUserData(MADE_NAMES, m.namesOnly());
    return m;
  }

  /**
   * Runs the remaining contributors at the main tree's position and adds the names that expansions make where those
   * contributors offer that kind of name: functions where they offer a function of the file (or in an export list),
   * records after {@code #} or where they offer a record, and the fields of a record that a macro makes in its braces.
   */
  private static void addMadeNames(@NotNull CompletionParameters parameters, @NotNull CompletionResultSet result, @NotNull Made made) {
    PsiElement position = parameters.getPosition();
    boolean export = PsiTreeUtil.getParentOfType(position, ErlangExportFunctions.class) != null;
    boolean withArity = export || PsiTreeUtil.getParentOfType(position, ErlangFunctionWithArityVariables.class) != null;
    PsiElement prev = PsiTreeUtil.prevVisibleLeaf(position);
    boolean afterRadix = prev != null && prev.getNode().getElementType() == ErlangTypes.ERL_RADIX;
    ErlangRecordExpression record = PsiTreeUtil.getParentOfType(position, ErlangRecordExpression.class);
    ErlangRecordRef recordRef = record == null ? null : record.getRecordRef();
    boolean inBraces = PsiTreeUtil.getParentOfType(position, ErlangRecordTuple.class, ErlangRecordField.class) != null;
    List<String> fields = inBraces && recordRef != null ? made.fields().get(recordRef.getText()) : null;
    Set<String> seen = new HashSet<>();
    Set<String> seenFunctions = new HashSet<>();
    boolean[] saw = new boolean[2];
    result.runRemainingContributors(parameters, r -> {
      PsiElement psi = r.getLookupElement().getPsiElement();
      if (psi instanceof ErlangFunction f) {
        saw[0] = true;
        // only the file's own functions: other:hello/0 does not offer this module's hello/0
        PsiFile fnFile = f.getContainingFile();
        if (fnFile != null && fnFile.getOriginalFile() == parameters.getOriginalFile()) seenFunctions.add(f.getName() + "/" + f.getArity());
      }
      if (psi instanceof ErlangRecordDefinition) saw[1] = true;
      seen.add(r.getLookupElement().getLookupString());
      result.passResult(r);
    });
    ErlangFile real = (ErlangFile)parameters.getOriginalFile();
    if ((saw[0] || export || functionPosition(position)) && !afterRadix && !inBraces) {
      for (Map.Entry<String, PsiElement> e : made.functions().entrySet()) {
        // a function the recovery made from the rest of a split form has the same name/arity: offered once
        if (seenFunctions.contains(e.getKey())) continue;
        int slash = e.getKey().lastIndexOf('/');
        String name = e.getKey().substring(0, slash);
        int arity = Integer.parseInt(e.getKey().substring(slash + 1));
        LookupElement l = ErlangPsiImplUtil.createFunctionLookupElement(name, arity, withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY);
        result.addElement(new RealTarget(l, realTarget(e.getValue(), real)));
      }
    }
    if ((afterRadix || saw[1]) && !inBraces) {
      for (Map.Entry<String, PsiElement> e : made.records().entrySet()) {
        if (seen.contains(e.getKey())) continue;
        result.addElement(new RealTarget(LookupElementBuilder.create(e.getKey()).withIcon(ErlangIcons.RECORD), realTarget(e.getValue(), real)));
      }
    }
    if (fields != null) {
      for (String f : fields) if (!seen.contains(f)) result.addElement(LookupElementBuilder.create(f).withIcon(ErlangIcons.FIELD));
    }
    MADE_ADDED.incrementAndGet();
    result.stopHere();
  }

  /**
   * The position where the main provider offers the local functions: an expression in a function, not after
   * {@code module:} (its condition; the results that reach this contributor are only those that match the prefix, so
   * a function offered there may not be seen).
   */
  private static boolean functionPosition(@NotNull PsiElement position) {
    PsiElement parent = position.getParent();
    if (parent instanceof ErlangAtom) parent = parent.getParent();
    PsiElement grandPa = parent == null ? null : parent.getParent();
    return grandPa instanceof ErlangExpression && PsiTreeUtil.getParentOfType(position, ErlangColonQualifiedExpression.class) == null &&
           PsiTreeUtil.getParentOfType(position, ErlangFunction.class) != null;
  }

  private static @Nullable PsiElement realTarget(@Nullable PsiElement e, @NotNull ErlangFile real) {
    return e == null || !e.isValid() ? null : ErlangMacroLayer.exit(e, real);
  }

  /** Completions that added names made by expansions (tests and measurements). */
  static final AtomicInteger MADE_ADDED = new AtomicInteger();

  private record Position(@NotNull PsiElement position, int offset, @NotNull ErlangMacroLayer.Copy copy, @NotNull ErlangFile real,
                          @NotNull PsiFile parsed) {
  }

  /** The position in the copy's text with the dummy identifier, or null when the main tree's position is to be used. */
  private static @Nullable Position position(@NotNull CompletionParameters parameters) {
    ErlangFile real = ErlangMacroLayer.layerFile(parameters.getOriginalFile());
    if (real == null || real != parameters.getOriginalFile()) return null;
    int offset = parameters.getOffset();
    PsiElement at = real.findElementAt(Math.max(0, offset - 1));
    PsiElement top = at == null ? null : ErlangMacroLayer.topForm(at);
    if (top == null) return skip(real, "no-form");
    if (isMacroNamePosition(at)) return null; // the bare macro reference of the main tree completes macro names
    // the main tree lost the caret's place: its top-level element is a bare token, an error or a dummy block (the
    // recovery after a fragment use left the rest of the clause there), a form with a syntax error, or the rest of a
    // form that the recovery split (the element before it does not end with a dot: ?ID(Ab) after foo?PAR,)
    boolean lost = !(top instanceof ErlangCompositeElement) || !afterADot(top) || errorBefore(top, offset);
    if (!lost) return boundByMacros(parameters, real, top, offset);
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(real);
    if (t == null || t.refusal != null || t.names.isEmpty()) return skip(real, t == null ? "no-table" : "table-refused-or-empty");
    // the broken form: back to the last element that ends with a dot; it must use a macro the file sees
    PsiElement first = top;
    for (PsiElement p = top.getPrevSibling(); p != null; p = p.getPrevSibling()) {
      if (p instanceof PsiWhiteSpace || p instanceof PsiComment) continue;
      if (endsForm(p)) break;
      first = p;
    }
    CharSequence region = real.getViewProvider().getContents().subSequence(first.getTextRange().getStartOffset(), offset);
    if (!ErlangMacroLayer.usesKnownMacro(region, t.names)) return skip(real, "no-macro-use-before-the-caret");
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(real, "completion");
    if (copy == null || copy.copyFile == null) return skip(real, "no-copy");
    return inCopy(parameters, real, copy, offset);
  }

  /**
   * A variable that only a macro binds ({@code ?BIND(X), X<caret>}, or the free {@code Xh} of
   * {@code -define(HX, Xh = 5)}) in a form that the main tree parses: the form needs the copy and has a candidate
   * (a free variable of a body it uses, or a variable in a substituted argument), and the prefix is a variable's (or
   * empty). The auto-popup only uses a copy that is there.
   */
  private static @Nullable Position boundByMacros(@NotNull CompletionParameters parameters, @NotNull ErlangFile real,
                                                  @NotNull PsiElement top, int offset) {
    CharSequence text = real.getViewProvider().getContents();
    int s = offset;
    while (s > 0 && (Character.isLetterOrDigit(text.charAt(s - 1)) || text.charAt(s - 1) == '_' || text.charAt(s - 1) == '@')) s--;
    if (s < offset && !Character.isUpperCase(text.charAt(s)) && text.charAt(s) != '_') return null;
    if (PsiTreeUtil.getParentOfType(real.findElementAt(Math.max(0, offset - 1)), ErlangClauseBody.class) == null) return null;
    boolean auto = parameters.isAutoPopup();
    ErlangMacroLayer.Table t = auto ? ErlangMacroLayer.cachedTable(real) : ErlangMacroLayer.table(real);
    if (t == null || t.refusal != null || t.names.isEmpty() || !ErlangMacroLayer.formNeedsCopy(t, top)) return null;
    if (ErlangMacroLayer.formFreeBodyVars(t, top).isEmpty() && ErlangMacroLayer.substitutedArgumentVars(t, top).isEmpty()) return null;
    ErlangMacroLayer.Copy copy = auto ? ErlangMacroLayer.cachedCopy(real) : ErlangMacroLayer.getCopy(real, "completion");
    if (copy == null || copy.copyFile == null) return skip(real, "bound:no-copy");
    return inCopy(parameters, real, copy, offset);
  }

  /** The position at the caret's place in the copy's text with the dummy identifier, or null. */
  private static @Nullable Position inCopy(@NotNull CompletionParameters parameters, @NotNull ErlangFile real,
                                           @NotNull ErlangMacroLayer.Copy copy, int offset) {
    ErlangMacroExpander.Form f = copy.expansion.formAtSource(offset);
    if (f == null || f.status != ErlangMacroExpander.FormStatus.EXPANDED) return skip(real, "form-not-expanded");
    int e = caretInCopy(copy, real, offset);
    if (e < 0) return skip(real, "caret-not-in-copy");
    String dummy = dummyIdentifier(parameters.getPosition().getContainingFile(), real, offset);
    if (dummy == null) return skip(real, "no-dummy");
    long p0 = System.nanoTime();
    String text = copy.expansion.text;
    PsiFile parsed = PsiFileFactory.getInstance(real.getProject())
      .createFileFromText(real.getName(), ErlangLanguage.INSTANCE, text.substring(0, e) + dummy + text.substring(e), false, false);
    if (!(parsed instanceof ErlangFile)) return null;
    parsed.putUserData(ErlangMacroLayer.COPY_OF, real);
    if (parsed instanceof PsiFileImpl impl) impl.setOriginalFile(real);
    parsed.putUserData(ORIGIN, new Origin(parameters.getOriginalPosition(), offset));
    PsiElement position = parsed.findElementAt(e);
    ourLastParseNanos = System.nanoTime() - p0;
    ErlangMacroLayerLog.event("COMPLETION", real.getName(), ourLastParseNanos, text.length(), 0,
                              parameters.isAutoPopup() ? "auto-popup" : "explicit", "completion");
    return position == null ? null : new Position(position, e, copy, real, parsed);
  }

  /** The caret is right after {@code ?} or in the name after it: a macro name, completed in the main tree. */
  private static boolean isMacroNamePosition(@NotNull PsiElement before) {
    if (before.getNode().getElementType() == ErlangTypes.ERL_QMARK) return true;
    PsiElement prev = PsiTreeUtil.prevVisibleLeaf(before);
    return prev != null && prev.getNode().getElementType() == ErlangTypes.ERL_QMARK && prev.getTextRange().getEndOffset() == before.getTextRange().getStartOffset();
  }

  private static boolean errorBefore(@NotNull PsiElement top, int offset) {
    for (PsiErrorElement e : PsiTreeUtil.findChildrenOfType(top, PsiErrorElement.class)) {
      if (e.getTextRange().getStartOffset() < offset) return true;
    }
    return false;
  }

  /** The element before the top-level element ends a form (or there is none). */
  private static boolean afterADot(@NotNull PsiElement top) {
    PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(top);
    return prev == null || endsForm(prev);
  }

  /**
   * The element ends with the dot that ends a form: not the dot of a record field access ({@code R#rec.field}, which
   * the main tree's recovery may take for the end of the form after a fragment use), as the expander tells them apart.
   */
  private static boolean endsForm(@NotNull PsiElement e) {
    PsiElement last = PsiTreeUtil.getDeepestVisibleLast(e);
    if (last == null || last.getNode().getElementType() != ErlangTypes.ERL_DOT) return false;
    PsiElement name = PsiTreeUtil.prevVisibleLeaf(last);
    PsiElement radix = name == null ? null : PsiTreeUtil.prevVisibleLeaf(name);
    return radix == null || radix.getNode().getElementType() != ErlangTypes.ERL_RADIX;
  }

  private static @Nullable Position skip(@NotNull ErlangFile file, @NotNull String why) {
    ErlangMacroLayerLog.event("COMPLETION", file.getName(), 0, 0, 0, "skip:" + why, "completion");
    return null;
  }

  /** The caret's place in the expanded text: right after the character before the caret, else at the caret. */
  private static int caretInCopy(@NotNull ErlangMacroLayer.Copy copy, @NotNull ErlangFile real, int offset) {
    if (offset > 0) {
      int before = copy.expansion.toExpanded(offset - 1);
      if (before >= 0) return before + 1;
      // the character before the caret is in a macro argument: the first copy of that argument token
      PsiElement leaf = real.findElementAt(offset - 1);
      if (leaf != null) {
        int start = leaf.getTextRange().getStartOffset();
        int[] copies = copy.expansion.argumentCopies(start);
        if (copies.length > 0) return copies[0] + offset - start;
      }
    }
    return copy.expansion.toExpanded(offset);
  }

  /** The text that the platform inserted at the caret of its completion copy, or null when it did more than insert. */
  private static @Nullable String dummyIdentifier(@NotNull PsiFile completionCopy, @NotNull PsiFile real, int offset) {
    CharSequence c = completionCopy.getViewProvider().getContents();
    CharSequence r = real.getViewProvider().getContents();
    int delta = c.length() - r.length();
    if (delta <= 0 || offset > r.length()) return null;
    if (!StringUtil.equals(c.subSequence(0, offset), r.subSequence(0, offset)) ||
        !StringUtil.equals(c.subSequence(offset + delta, c.length()), r.subSequence(offset, r.length()))) {
      return null;
    }
    return c.subSequence(offset, offset + delta).toString();
  }

  /** The lookup element with a real target instead of an element of the parsed text (see the class comment). */
  private static @NotNull LookupElement realLookup(@NotNull Position p, @NotNull LookupElement element) {
    PsiElement psi = element.getPsiElement();
    if (psi == null || psi.getContainingFile() != p.parsed) return element;
    PsiElement real = null;
    if (psi instanceof ErlangFunction f) {
      real = p.real.getFunction(f.getName(), f.getArity());
      if (real == null) real = p.copy.functionsMadeByMacros().get(f.getName() + "/" + f.getArity());
    }
    else if (psi instanceof ErlangRecordDefinition r) {
      real = p.real.getRecord(String.valueOf(r.getName()));
      if (real == null) real = p.copy.recordsMadeByMacros().get(String.valueOf(r.getName()));
    }
    else if (psi instanceof ErlangTypeDefinition d) {
      real = p.real.getType(String.valueOf(d.getName()));
      if (real == null) real = p.copy.typesMadeByMacros().get(String.valueOf(d.getName()));
    }
    return new RealTarget(element, real == null ? null : ErlangMacroLayer.exit(real, p.real));
  }

  /** A lookup element of the parsed text whose target is a real element (or none). */
  private static final class RealTarget extends LookupElementDecorator<LookupElement> {
    private final @Nullable PsiElement myReal;

    RealTarget(@NotNull LookupElement delegate, @Nullable PsiElement real) {
      super(delegate);
      myReal = real;
    }

    @Override
    public @Nullable PsiElement getPsiElement() {
      return myReal;
    }

    @Override
    public @NotNull Object getObject() {
      return myReal != null ? myReal : getLookupString();
    }

    @Override
    public boolean isValid() {
      return myReal == null || myReal.isValid();
    }
  }
}
