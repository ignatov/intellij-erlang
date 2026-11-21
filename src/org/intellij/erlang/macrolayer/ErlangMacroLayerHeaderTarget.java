package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.highlighting.HighlightUsagesDescriptionLocation;
import com.intellij.ide.util.DeleteNameDescriptionLocation;
import com.intellij.ide.util.DeleteTypeDescriptionLocation;
import com.intellij.lang.Language;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.Navigatable;
import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.impl.FakePsiElement;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.ErlangQVar;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

/**
 * Spike 4: a resolve target that lies in a {@code -define} body of an included header. It is named by the header,
 * the directive and the token index, not by an offset: an edit of the header outside its directive lines keeps it.
 * Highlighting needs only "it resolves", so the header's PSI tree is not loaded. The header element is made only when
 * navigation asks ({@link #getNavigationElement()}). Rename refuses: renaming a name of a header macro body from one
 * includer would break the other includers.
 */
public final class ErlangMacroLayerHeaderTarget extends FakePsiElement implements PsiNamedElement {
  private final Project myProject;
  private final VirtualFile myHeader;
  private final int myDirective;
  private final int myIndex;
  private final String myText;
  private final Class<?> myPreferred;
  /** The last answer of {@link #isValid()} under read access; it was valid when the layer made it. */
  private volatile boolean myLastValid = true;

  ErlangMacroLayerHeaderTarget(@NotNull Project project, @NotNull VirtualFile header, int directive, int index,
                               @NotNull String text, @NotNull Class<?> preferred) {
    myProject = project;
    myHeader = header;
    myDirective = directive;
    myIndex = index;
    myText = text;
    myPreferred = preferred;
  }

  private @Nullable PsiFile headerFile() {
    return myHeader.isValid() ? PsiManager.getInstance(myProject).findFile(myHeader) : null;
  }

  /** The current range of the token in the header, from the header's summary (no PSI tree), or null. */
  public @Nullable TextRange currentRange() {
    PsiFile f = headerFile();
    if (f == null) return null;
    ErlangMacroExpander.HeaderSummary s = ErlangMacroLayer.summaryOf(f);
    if (myDirective >= s.directives.size()) return null;
    var toks = s.directives.get(myDirective).tokens;
    if (myIndex >= toks.size() || !toks.get(myIndex).text().equals(myText)) return null;
    return TextRange.create(toks.get(myIndex).start(), toks.get(myIndex).end());
  }

  @Override
  public @Nullable PsiElement getParent() {
    return headerFile();
  }

  @Override
  public @Nullable PsiFile getContainingFile() {
    return headerFile();
  }

  /** Erlang: find usages and the other language services take the target as an Erlang element. */
  @Override
  public @NotNull Language getLanguage() {
    return ErlangLanguage.INSTANCE;
  }

  @Override
  public @NotNull Project getProject() {
    return myProject;
  }

  /**
   * Like {@code PsiFileImpl.isValid()}, this may be called without read access (listeners and expiration checks do).
   * Then it reads no PSI: it checks the header's virtual file and returns the last answer given under read access.
   * The answer cannot be fresher anyway: without read access a write action may change the header at any moment.
   * Under read access it checks that the header's directive still has the token.
   */
  @Override
  public boolean isValid() {
    if (myProject.isDisposed() || !myHeader.isValid()) return false;
    if (!ApplicationManager.getApplication().isReadAccessAllowed()) return myLastValid;
    boolean valid = currentRange() != null;
    myLastValid = valid;
    return valid;
  }

  @Override
  public boolean isPhysical() {
    PsiFile f = headerFile();
    return f != null && f.isPhysical();
  }

  @Override
  public @NotNull String getName() {
    return myText;
  }

  @Override
  public @NotNull String getText() {
    return myText;
  }

  @Override
  public PsiElement setName(@NotNull String name) throws IncorrectOperationException {
    throw new IncorrectOperationException("A name in the body of a header macro is not renamed from an includer");
  }

  @Override
  public @Nullable TextRange getTextRange() {
    return currentRange();
  }

  @Override
  public int getTextOffset() {
    TextRange r = currentRange();
    return r == null ? 0 : r.getStartOffset();
  }

  @Override
  public int getTextLength() {
    return myText.length();
  }

  /** The real element in the header. This loads the header's PSI tree. */
  @Override
  public @NotNull PsiElement getNavigationElement() {
    PsiFile f = headerFile();
    TextRange r = currentRange();
    if (f == null) return this;
    if (r == null) return f;
    ErlangMacroLayer.HEADER_TARGETS_MATERIALIZED.incrementAndGet();
    PsiElement e = ErlangMacroLayer.findReal(f, r, myPreferred);
    return e != null ? e : f;
  }

  @Override
  public void navigate(boolean requestFocus) {
    if (getNavigationElement() instanceof Navigatable n) n.navigate(requestFocus);
  }

  @Override
  public boolean canNavigate() {
    return isValid();
  }

  @Override
  public boolean isEquivalentTo(PsiElement another) {
    if (this == another || equals(another)) return true;
    if (another == null || another instanceof ErlangMacroLayerHeaderTarget) return false;
    PsiFile f = another.getContainingFile();
    TextRange r = currentRange();
    return f != null && myHeader.equals(f.getVirtualFile()) && r != null && r.equals(another.getTextRange());
  }

  /** What the name is, for the rename dialog, usage views and conflict texts: as for the same element in the header. */
  public @NotNull String getKind() {
    if (ErlangQVar.class.isAssignableFrom(myPreferred)) return "variable";
    if (ErlangFunction.class.isAssignableFrom(myPreferred)) return "function";
    if (ErlangQAtom.class.isAssignableFrom(myPreferred)) return "atom";
    return "name";
  }

  /**
   * The description of the target for the Erlang description provider: its kind or its name. Other locations
   * (refactoring texts) are left to the platform's default, which combines the kind and the name.
   */
  public @Nullable String describe(@NotNull ElementDescriptionLocation location) {
    return describe(getKind(), myText, location);
  }

  /** A description of a layer target by its kind and name; null for the locations left to the platform's default. */
  static @Nullable String describe(@NotNull String kind, @NotNull String name, @NotNull ElementDescriptionLocation location) {
    if (location == UsageViewTypeLocation.INSTANCE || location instanceof DeleteTypeDescriptionLocation) return kind;
    if (location == UsageViewShortNameLocation.INSTANCE || location == UsageViewLongNameLocation.INSTANCE ||
        location == UsageViewNodeTextLocation.INSTANCE || location == HighlightUsagesDescriptionLocation.INSTANCE ||
        location instanceof DeleteNameDescriptionLocation) {
      return name;
    }
    return null;
  }

  @Override
  public @NotNull String getPresentableText() {
    return myText;
  }

  @Override
  public @Nullable String getLocationString() {
    return myHeader.getName();
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof ErlangMacroLayerHeaderTarget t && t.myHeader.equals(myHeader) && t.myDirective == myDirective &&
           t.myIndex == myIndex;
  }

  @Override
  public int hashCode() {
    return Objects.hash(myHeader, myDirective, myIndex);
  }

  @Override
  public String toString() {
    return "HeaderTarget(" + myHeader.getName() + "#" + myDirective + ":" + myIndex + " " + myText + ")";
  }
}
