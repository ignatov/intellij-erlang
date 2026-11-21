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
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Round 6 (next), item 2: completion in a part of a form that the main tree lost. In
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
 * the daemon builds and starts no build (see {@code ErlangMacroLayer.awaitWhileTyped}). The cost per completion in such
 * a form is one parse of the expanded text with the dummy identifier (the keyword provider already parses the text
 * before the caret once per completion).
 */
public final class ErlangMacroLayerCompletionContributor extends CompletionContributor {
  /** On the parsed text: the main tree's original position and the caret offset in the real file. */
  public record Origin(@Nullable PsiElement originalPosition, int offset) {
  }

  public static final Key<Origin> ORIGIN = Key.create("erlang.macro.layer.completion.origin");

  /** Completions answered through the copy, for tests and measurements. */
  static final java.util.concurrent.atomic.AtomicInteger TAKEN_OVER = new java.util.concurrent.atomic.AtomicInteger();
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
    Position p = parameters.isAutoPopup() ? ErlangMacroLayer.asPassiveQuery(() -> position(parameters)) : position(parameters);
    if (p == null) return;
    TAKEN_OVER.incrementAndGet();
    result.runRemainingContributors(parameters.withPosition(p.position, p.offset),
                                    r -> result.passResult(r.withLookupElement(realLookup(p, r.getLookupElement()))));
    result.stopHere();
  }

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
    boolean lost = !(top instanceof ErlangCompositeElement) || PsiTreeUtil.hasErrorElements(top) || !afterADot(top);
    if (!lost) return null;
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(real);
    if (t == null || t.refusal != null || t.names.isEmpty()) return skip(real, t == null ? "no-table" : "table-refused-or-empty");
    // the broken form: back to the last element that ends with a dot; it must use a macro the file sees
    PsiElement first = top;
    for (PsiElement p = top.getPrevSibling(); p != null; p = p.getPrevSibling()) {
      if (p instanceof PsiWhiteSpace || p instanceof PsiComment) continue;
      PsiElement last = PsiTreeUtil.getDeepestVisibleLast(p);
      if (last != null && last.getNode().getElementType() == ErlangTypes.ERL_DOT) break;
      first = p;
    }
    CharSequence region = real.getViewProvider().getContents().subSequence(first.getTextRange().getStartOffset(), offset);
    if (!ErlangMacroLayer.usesKnownMacro(region, t.names)) return skip(real, "no-macro-use-before-the-caret");
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(real, "completion");
    if (copy == null || copy.copyFile == null) return skip(real, "no-copy");
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

  /** The element before the top-level element ends with a dot (or there is none). */
  private static boolean afterADot(@NotNull PsiElement top) {
    PsiElement prev = PsiTreeUtil.skipWhitespacesAndCommentsBackward(top);
    if (prev == null) return true;
    PsiElement last = PsiTreeUtil.getDeepestVisibleLast(prev);
    return last != null && last.getNode().getElementType() == ErlangTypes.ERL_DOT;
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
