package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.highlighting.HighlightUsagesDescriptionLocation;
import com.intellij.find.FindManager;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.find.impl.FindManagerBase;
import com.intellij.lang.LanguageDocumentation;
import com.intellij.lang.LanguageRefactoringSupport;
import com.intellij.lang.documentation.DocumentationProvider;
import com.intellij.lang.documentation.psi.UtilKt;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.lang.findUsages.LanguageFindUsages;
import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.platform.backend.documentation.DocumentationTarget;
import com.intellij.psi.*;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.refactoring.safeDelete.SafeDeleteProcessor;
import com.intellij.refactoring.util.NonCodeSearchDescriptionLocation;
import com.intellij.refactoring.util.RefactoringDescriptionLocation;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.usageView.*;
import com.intellij.util.CommonProcessors;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

/**
 * Pushes one element through the platform APIs that hand a {@link PsiElement} to plugin code, in the order and with
 * the preconditions the platform keeps (a find-usages handler only when {@code canFindUsages} said yes, and so on):
 * descriptions at every location, presentation, find usages (provider, factory, handler, primary and secondary elements,
 * the search), rename (processor, substitution, preparation), safe delete, refactoring support, documentation (the
 * provider and the documentation targets with their presentation), use scope, references.
 */
public final class ElementProbe {
  public static final ElementDescriptionLocation[] LOCATIONS = {
    UsageViewTypeLocation.INSTANCE, UsageViewShortNameLocation.INSTANCE, UsageViewLongNameLocation.INSTANCE,
    UsageViewNodeTextLocation.INSTANCE, HighlightUsagesDescriptionLocation.INSTANCE, RefactoringDescriptionLocation.WITH_PARENT,
    RefactoringDescriptionLocation.WITHOUT_PARENT, NonCodeSearchDescriptionLocation.NON_JAVA,
    NonCodeSearchDescriptionLocation.STRINGS_AND_COMMENTS};

  private final FuzzRecorder myRec;
  private final Project myProject;
  private final @Nullable Editor myEditor;
  /** Whether to also run the searches (find usages, references) in the background; they cost the most. */
  public boolean searches = true;

  public ElementProbe(@NotNull FuzzRecorder rec, @NotNull Project project, @Nullable Editor editor) {
    myRec = rec;
    myProject = project;
    myEditor = editor;
  }

  /** {@code kind} names the element in the action labels, for example {@code dir-empty} or {@code target@123}. */
  public void probe(@NotNull String kind, @NotNull PsiElement e) {
    probe(kind, e, true);
  }

  /**
   * With {@code full} false, only the entry points the platform calls for any element (of a selection, at a caret) are
   * asked: find usages availability, the rename processors, safe delete, refactoring support, the references. Use it for
   * elements the platform never hands over as targets (an arbitrary inner element, a reference host, an invalid element);
   * descriptions, presentation and documentation are asked only of targets and of elements of other kinds.
   */
  public void probe(@NotNull String kind, @NotNull PsiElement e, boolean full) {
    String p = "elem[" + kind + "] ";
    boolean valid = e.isValid();
    if (!full) {
      myRec.guard(p + "FindManager.canFindUsages", () -> FindManager.getInstance(myProject).canFindUsages(e));
      myRec.guard(p + "rename-processor", () -> RenamePsiElementProcessor.forElement(e).substituteElementToRename(e, myEditor));
      myRec.guard(p + "SafeDelete.validElement", () -> SafeDeleteProcessor.validElement(e));
      myRec.guard(p + "refactoring-support", () -> {
        RefactoringSupportProvider sp = LanguageRefactoringSupport.getInstance().forContext(e);
        if (sp != null) {
          sp.isSafeDeleteAvailable(e);
          sp.isInplaceRenameAvailable(e, null);
          sp.isMemberInplaceRenameAvailable(e, null);
        }
      });
      if (valid) myRec.guard(p + "references", () -> {
        for (PsiReference r : e.getReferences()) {
          PsiElement t = r.resolve();
          if (r instanceof PsiPolyVariantReference pv) pv.multiResolve(false);
          if (t != null) r.isReferenceTo(t);
        }
      });
      return;
    }
    for (ElementDescriptionLocation loc : LOCATIONS) {
      myRec.guard(p + "describe " + loc.getClass().getSimpleName(), () -> ElementDescriptionUtil.getElementDescription(e, loc));
    }
    myRec.guard(p + "UsageViewUtil", () -> {
      UsageViewUtil.getType(e);
      UsageViewUtil.getShortName(e);
      UsageViewUtil.getLongName(e);
      UsageViewUtil.createNodeText(e);
    });
    myRec.guard(p + "presentation", () -> {
      if (e instanceof NavigationItem n) {
        ItemPresentation pr = n.getPresentation();
        if (pr != null) {
          pr.getPresentableText();
          pr.getLocationString();
          pr.getIcon(false);
        }
        n.getName();
      }
      if (e instanceof PsiNamedElement n) n.getName();
      e.getNavigationElement();
      e.getTextOffset();
    });
    myRec.guard(p + "doc-targets", () -> {
      for (DocumentationTarget t : UtilKt.psiDocumentationTargets(e, e)) {
        t.computePresentation();
        t.computeDocumentationHint();
        t.getNavigatable();
        t.createPointer().dereference();
      }
    });
    if (e.getLanguage().isKindOf("Erlang") || e instanceof PsiFileSystemItem) {
      myRec.guard(p + "doc-provider", () -> {
        DocumentationProvider dp = LanguageDocumentation.INSTANCE.forLanguage(e.getLanguage());
        if (dp != null) {
          dp.getQuickNavigateInfo(e, e);
          dp.getUrlFor(e, e);
          dp.generateDoc(e, e);
          dp.generateHoverDoc(e, e);
        }
      });
    }
    // find usages, as the Find Usages action and the Show Usages popup ask
    boolean[] canFind = new boolean[1];
    myRec.guard(p + "FindManager.canFindUsages", () -> canFind[0] = FindManager.getInstance(myProject).canFindUsages(e));
    myRec.guard(p + "FindUsagesProvider", () -> {
      FindUsagesProvider fp = LanguageFindUsages.INSTANCE.forLanguage(e.getLanguage());
      if (fp != null && fp.canFindUsagesFor(e)) {
        fp.getType(e);
        fp.getDescriptiveName(e);
        fp.getNodeText(e, true);
        fp.getNodeText(e, false);
        fp.getHelpId(e);
      }
    });
    if (canFind[0]) {
      for (boolean forHighlight : new boolean[]{false, true}) {
        String q = p + "find-handler" + (forHighlight ? "(highlight)" : "") + " ";
        FindUsagesHandler[] handler = new FindUsagesHandler[1];
        myRec.guard(q + "create", () -> handler[0] = ((FindManagerBase)FindManager.getInstance(myProject)).getFindUsagesManager()
          .getFindUsagesHandler(e, forHighlight ? FindUsagesHandlerFactory.OperationMode.HIGHLIGHT_USAGES
                                                : FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS));
        FindUsagesHandler h = handler[0];
        if (h == null) continue;
        myRec.guard(q + "elements", () -> {
          h.getPrimaryElements();
          h.getSecondaryElements();
          h.getFindUsagesOptions(null);
          h.getPsiElement();
        });
        if (searches && !forHighlight) {
          myRec.guard(q + "processElementUsages", () -> background(() -> {
            FindUsagesOptions options = h.getFindUsagesOptions(null);
            CommonProcessors.CollectProcessor<UsageInfo> c = new CommonProcessors.CollectProcessor<>();
            for (PsiElement pe : h.getPrimaryElements()) h.processElementUsages(pe, c, options);
            for (PsiElement pe : h.getSecondaryElements()) h.processElementUsages(pe, c, options);
            for (UsageInfo u : c.getResults()) {
              u.getElement();
              u.getNavigationRange();
            }
            return null;
          }));
        }
      }
    }
    if (searches && valid && e.getContainingFile() != null && !(e instanceof PsiFileSystemItem)) {
      myRec.guard(p + "ReferencesSearch(local)", () -> background(() -> {
        PsiFile f = e.getContainingFile();
        SearchScope scope = new LocalSearchScope(f);
        for (PsiReference r : ReferencesSearch.search(e, scope).findAll()) {
          r.getElement();
          r.getRangeInElement();
          r.isReferenceTo(e);
        }
        return null;
      }));
      myRec.guard(p + "useScope", () -> PsiSearchHelper.getInstance(myProject).getUseScope(e));
    }
    // rename, as the Rename action asks before it opens the dialog or starts the inplace rename
    myRec.guard(p + "rename-processor", () -> {
      RenamePsiElementProcessor rp = RenamePsiElementProcessor.forElement(e);
      PsiElement sub = rp.substituteElementToRename(e, myEditor);
      if (sub == null) return;
      RenamePsiElementProcessor rp2 = RenamePsiElementProcessor.forElement(sub);
      Map<PsiElement, String> all = new LinkedHashMap<>();
      all.put(sub, "fuzz_renamed");
      rp2.prepareRenaming(sub, "fuzz_renamed", all);
      rp2.isInplaceRenameSupported();
      rp2.getQualifiedNameAfterRename(sub, "fuzz_renamed", true);
      rp2.getHelpID(sub);
    });
    myRec.guard(p + "refactoring-support", () -> {
      RefactoringSupportProvider sp = LanguageRefactoringSupport.getInstance().forContext(e);
      if (sp != null) {
        sp.isSafeDeleteAvailable(e);
        sp.isInplaceRenameAvailable(e, null);
        sp.isMemberInplaceRenameAvailable(e, null);
        sp.isAvailable(e);
      }
    });
    myRec.guard(p + "SafeDelete.validElement", () -> SafeDeleteProcessor.validElement(e));
    if (valid && !(e instanceof PsiFileSystemItem)) {
      myRec.guard(p + "references", () -> {
        for (PsiReference r : e.getReferences()) {
          r.getRangeInElement();
          r.getCanonicalText();
          PsiElement t = r.resolve();
          if (r instanceof PsiPolyVariantReference pv) pv.multiResolve(false);
          if (t != null) r.isReferenceTo(t);
          r.isReferenceTo(e);
        }
      });
    }
  }

  /** Runs in a pooled thread in a read action under a progress indicator, like background work of the IDE. */
  public <T> T background(@NotNull Callable<T> c) throws Exception {
    Future<T> f = ApplicationManager.getApplication().executeOnPooledThread(
      () -> ProgressManager.getInstance().runProcess(() -> ReadAction.compute(() -> {
        try {
          return c.call();
        }
        catch (RuntimeException e) {
          throw e;
        }
        catch (Exception e) {
          throw new RuntimeException(e);
        }
      }), new EmptyProgressIndicator()));
    return PlatformTestUtil.waitForFuture(f, FuzzConfig.actionMillis() * 2);
  }
}
