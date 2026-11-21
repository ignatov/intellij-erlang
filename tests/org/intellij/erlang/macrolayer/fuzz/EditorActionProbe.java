package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.lang.LanguageStructureViewBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.lang.folding.LanguageFolding;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.command.undo.UndoManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.impl.text.TextEditorProvider;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.ui.breadcrumbs.BreadcrumbsProvider;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * Editor actions through the paths the IDE takes: the fixture's highlighting, completion and typing, the real action ids
 * ({@link CodeInsightTestFixture#performEditorAction}), quick fixes and intentions, rename and find usages of the target
 * at the caret, the structure view, folding, breadcrumbs and Go to Symbol. A read-only action must not change the
 * document; after every action the tree must match the text.
 */
public final class EditorActionProbe {
  public record Act(String name, boolean readOnly, Body body) {
  }

  public interface Body {
    void run(Random rnd) throws Throwable;
  }

  /** Real action ids that run headless (no dialog). Read-only ones first. */
  static final String[] READ_ONLY_IDS = {"GotoDeclaration", "QuickJavaDoc", "HighlightUsagesInFile", "ParameterInfo", "EditorSelectWord",
    "EditorUnSelectWord", "ExpandRegion", "CollapseRegion", "ExpandAllRegions", "CollapseAllRegions", "MethodDown", "MethodUp",
    "EditorCodeBlockEnd", "EditorCodeBlockStart", "GotoImplementation", "GotoTypeDeclaration", "GotoNextError", "SelectAllOccurrences", "SelectNextOccurrence", "EditorMatchBrace"};
  static final String[] EDITING_IDS = {"CommentByLineComment", "CommentByBlockComment", "ReformatCode", "EditorEnter", "EditorBackSpace",
    "EditorDelete", "EditorDuplicate", "MoveLineUp", "MoveLineDown", "EditorJoinLines", "EditorCompleteStatement", "EditorStartNewLine",
    "MoveStatementUp", "MoveStatementDown", "AutoIndentLines", "EditorToggleCase", "FillParagraph"};

  private final FuzzRecorder myRec;
  private final CodeInsightTestFixture myFixture;
  private final List<Act> myActs = new ArrayList<>();
  private static boolean ourInspectionsOn;

  public EditorActionProbe(@NotNull FuzzRecorder rec, @NotNull CodeInsightTestFixture fixture) {
    myRec = rec;
    myFixture = fixture;
    for (String id : READ_ONLY_IDS) myActs.add(new Act(id, true, r -> action(id)));
    for (String id : EDITING_IDS) myActs.add(new Act(id, false, r -> action(id)));
    myActs.add(new Act("highlight", true, r -> myFixture.doHighlighting()));
    myActs.add(new Act("complete", false, r -> {
      myFixture.completeBasic();
      LookupManager.hideActiveLookup(project());
    }));
    myActs.add(new Act("complete-and-pick", false, r -> {
      var items = myFixture.completeBasic();
      if (items != null && items.length > 0 && LookupManager.getActiveLookup(editor()) != null) {
        myFixture.getLookup().setCurrentItem(items[r.nextInt(items.length)]);
        myFixture.finishLookup('\n');
      }
      LookupManager.hideActiveLookup(project());
    }));
    myActs.add(new Act("intention", false, r -> {
      // the inspection-management intentions of the platform change the test profile or open settings: left out
      List<IntentionAction> all = new ArrayList<>();
      for (IntentionAction ia : myFixture.getAvailableIntentions()) {
        String text = ia.getText();
        if (text.startsWith("Disable") || text.startsWith("Suppress") || text.startsWith("Edit inspection") || text.startsWith("Run inspection") ||
            text.startsWith("Fix all") || text.startsWith("Edit intention") || text.startsWith("Configure") || text.contains("inspection")) continue;
        all.add(ia);
      }
      if (!all.isEmpty()) myFixture.launchAction(all.get(r.nextInt(all.size())));
    }));
    myActs.add(new Act("rename-at-caret", false, r -> {
      PsiElement t = target();
      if (t != null) myFixture.renameElement(t, validName(t, r), r.nextBoolean(), r.nextBoolean());
    }));
    myActs.add(new Act("find-usages-at-caret", true, r -> {
      PsiElement t = target();
      if (t != null && com.intellij.find.FindManager.getInstance(project()).canFindUsages(t)) myFixture.findUsages(t);
    }));
    myActs.add(new Act("references-at-caret", true, r -> {
      PsiReference ref = file().findReferenceAt(editor().getCaretModel().getOffset());
      if (ref != null) {
        ref.resolve();
        if (ref instanceof com.intellij.psi.PsiPolyVariantReference pv) pv.multiResolve(false);
        ref.getVariants();
      }
    }));
    myActs.add(new Act("structure-view", true, r -> structureView()));
    myActs.add(new Act("folding", true, r -> folding()));
    myActs.add(new Act("breadcrumbs", true, r -> breadcrumbs()));
    myActs.add(new Act("goto-symbol", true, this::gotoSymbol));
  }

  public @NotNull List<Act> acts() {
    return myActs;
  }

  public @NotNull Act random(@NotNull Random rnd, boolean readOnlyOnly) {
    return random(rnd, readOnlyOnly, Set.of());
  }

  public @NotNull Act random(@NotNull Random rnd, boolean readOnlyOnly, @NotNull Set<String> exclude) {
    while (true) {
      Act a = myActs.get(rnd.nextInt(myActs.size()));
      if ((!readOnlyOnly || a.readOnly) && !exclude.contains(a.name)) return a;
    }
  }

  /** Actions that wait for smart mode in tests (the daemon) and so never end inside dumb mode. */
  public static final Set<String> NOT_IN_DUMB_MODE = Set.of("highlight", "GotoNextError", "find-usages-at-caret");

  /** Turns on every inspection of the plugin (the Dialyzer one excepted) for the highlighting actions. */
  public static void enableInspections(@NotNull CodeInsightTestFixture fixture) {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    fixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  /** Runs the act under the recorder with the read-only and the consistency checks. */
  public void run(@NotNull Act a, @NotNull Random rnd, @NotNull String prefix) {
    Document doc = editor().getDocument();
    String before = doc.getText();
    long seed = rnd.nextLong();
    boolean ok = myRec.guard(prefix + a.name, () -> a.body.run(new Random(seed)));
    LookupManager.hideActiveLookup(project());
    if (a.readOnly && ok && !doc.getText().equals(before)) {
      myRec.fail("data-change", a.name, "the read-only action " + a.name + " changed the document");
    }
  }

  /** The tree matches the text, and the stubs match the tree. */
  public void checkConsistency(@NotNull String prefix) {
    myRec.guard(prefix + "consistency", () -> {
      PsiDocumentManager.getInstance(project()).commitAllDocuments();
      PsiFile f = file();
      if (!f.getText().equals(editor().getDocument().getText())) {
        myRec.fail("inconsistency", "tree-text", "the tree text differs from the document after commit");
      }
      PsiTestUtil.checkFileStructure(f);
      PsiTestUtil.checkStubsMatchText(f);
    });
  }

  public void undo() {
    myRec.guard("undo", () -> {
      FileEditor fe = TextEditorProvider.getInstance().getTextEditor(editor());
      UndoManager um = UndoManager.getInstance(project());
      if (um.isUndoAvailable(fe)) um.undo(fe);
    });
  }

  public void redo() {
    myRec.guard("redo", () -> {
      FileEditor fe = TextEditorProvider.getInstance().getTextEditor(editor());
      UndoManager um = UndoManager.getInstance(project());
      if (um.isRedoAvailable(fe)) um.redo(fe);
    });
  }

  private void action(String id) {
    myFixture.performEditorAction(id);
  }

  private Project project() {
    return myFixture.getProject();
  }

  private Editor editor() {
    return myFixture.getEditor();
  }

  private PsiFile file() {
    return myFixture.getFile();
  }

  private PsiElement target() {
    PsiDocumentManager.getInstance(project()).commitAllDocuments();
    return TargetElementUtil.findTargetElement(editor(), TargetElementUtil.getInstance().getAllAccepted());
  }

  /** A name the rename refactoring accepts for the target (same shape as the old one), sometimes a clashing one. */
  static String validName(PsiElement t, Random r) {
    String old = t instanceof com.intellij.psi.PsiNamedElement n && n.getName() != null ? n.getName() : t.getText();
    boolean upper = old != null && !old.isEmpty() && (Character.isUpperCase(old.charAt(0)) || old.charAt(0) == '_');
    String[] clash = upper ? new String[]{"X", "C", "MODULE", "LINE"} : new String[]{"f0", "module_info", "ok", "m"};
    if (r.nextInt(4) == 0) return clash[r.nextInt(clash.length)];
    return upper ? "Renamed" + r.nextInt(100) : "renamed_" + r.nextInt(100);
  }

  private void structureView() {
    StructureViewBuilder b = LanguageStructureViewBuilder.getInstance().getStructureViewBuilder(file());
    if (!(b instanceof TreeBasedStructureViewBuilder tb)) return;
    StructureViewModel model = tb.createStructureViewModel(editor());
    try {
      walk(model.getRoot(), 0);
      model.getCurrentEditorElement();
    }
    finally {
      model.dispose();
    }
  }

  private static void walk(TreeElement e, int depth) {
    ItemPresentation p = e.getPresentation();
    p.getPresentableText();
    p.getLocationString();
    p.getIcon(false);
    if (e instanceof StructureViewTreeElement s) s.getValue();
    if (depth < 6) for (TreeElement c : e.getChildren()) walk(c, depth + 1);
  }

  private void folding() {
    for (var builder : LanguageFolding.INSTANCE.allForLanguage(file().getLanguage())) {
      for (FoldingDescriptor d : LanguageFolding.buildFoldingDescriptors(builder, file(), editor().getDocument(), false)) {
        d.getPlaceholderText();
        d.getRange();
      }
    }
  }

  private void breadcrumbs() {
    int offset = editor().getCaretModel().getOffset();
    PsiElement e = file().findElementAt(Math.max(0, Math.min(offset, file().getTextLength() - 1)));
    for (BreadcrumbsProvider p : BreadcrumbsProvider.EP_NAME.getExtensionList()) {
      if (!Arrays.asList(p.getLanguages()).contains(file().getLanguage())) continue;
      for (PsiElement x = e; x != null && !(x instanceof PsiFile); x = p.getParent(x)) {
        if (p.acceptElement(x)) {
          p.getElementInfo(x);
          p.getElementTooltip(x);
          p.getElementIcon(x);
        }
      }
    }
  }

  private void gotoSymbol(Random r) {
    String[] prefixes = {"f", "g", "m", "gen", "?", "M", "H", "r", ""};
    for (var ep : List.of(ChooseByNameContributor.SYMBOL_EP_NAME, ChooseByNameContributor.CLASS_EP_NAME)) {
      for (ChooseByNameContributor c : ep.getExtensionList()) {
        if (!c.getClass().getName().startsWith("org.intellij.erlang.")) continue;
        String[] names = c.getNames(project(), false);
        String prefix = prefixes[r.nextInt(prefixes.length)];
        int n = 0;
        for (String name : names) {
          if (!name.startsWith(prefix) || n++ > 20) continue;
          for (NavigationItem item : c.getItemsByName(name, prefix, project(), false)) {
            ItemPresentation p = item.getPresentation();
            if (p != null) {
              p.getPresentableText();
              p.getLocationString();
            }
            item.getName();
          }
        }
      }
    }
  }
}
