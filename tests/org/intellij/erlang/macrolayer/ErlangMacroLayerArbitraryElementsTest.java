package org.intellij.erlang.macrolayer;

import com.intellij.find.FindManager;
import com.intellij.find.impl.FindManagerImpl;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.ErlangDescriptionProvider;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.rename.ErlangMacroLayerRenameProcessor;
import org.intellij.erlang.rename.ErlangRenameMacroProcessor;
import org.intellij.erlang.search.ErlangFindUsagesHandlerFactory;
import org.intellij.erlang.search.ErlangTargetElementEvaluator;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * The owner's finding (an NPE in the Project view popup): the platform calls the branch's extensions with any element,
 * here a {@link PsiDirectory}. Every entry point of the branch that the platform calls with an arbitrary element must
 * not throw, and for an element that is not Erlang code it must give master's answer (no layer answer).
 */
public class ErlangMacroLayerArbitraryElementsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  /** The elements, by name; the Erlang ones last. Non-Erlang elements must get no layer answer. */
  private Map<String, PsiElement> elements(List<String> nonErlang) throws Exception {
    Map<String, PsiElement> m = new LinkedHashMap<>();
    PsiFile erl = myFixture.addFileToProject("a/m.erl", "-module(m).\n-export([k/0]).\n-define(DEF(N), N() -> ok).\n?DEF(hello).\nk() -> hello().\n");
    PsiFile hrl = myFixture.addFileToProject("a/h.hrl", "-define(H, 1).\n");
    PsiFile txt = myFixture.addFileToProject("a/notes.txt", "hello\n");
    VirtualFile png = myFixture.getTempDirFixture().createFile("a/pic.png");
    com.intellij.openapi.application.WriteAction.run(() -> png.setBinaryContent(new byte[]{(byte)0x89, 'P', 'N', 'G', 0, 1, 2, 3}));
    PsiFile bin = getPsiManager().findFile(png);
    PsiFile xml = PsiFileFactory.getInstance(getProject()).createFileFromText("x.xml", FileTypeManager.getInstance().getFileTypeByExtension("xml"), "<a><b/></a>");
    PsiFile light = PsiFileFactory.getInstance(getProject()).createFileFromText("l.erl", ErlangLanguage.INSTANCE,
                                                                                "-module(l).\n-define(DEF(N), N() -> ok).\n?DEF(hi).\n", false, false);
    myFixture.addFileToProject("a/sub/inner/z.txt", "z\n");
    m.put("directory", erl.getContainingDirectory());
    m.put("directory whose first child is a directory", erl.getContainingDirectory().findSubdirectory("sub"));
    m.put("text file", txt);
    m.put("binary file", bin);
    m.put("xml file", xml);
    m.put("xml tag", PsiTreeUtil.getDeepestFirst(xml).getParent());
    nonErlang.addAll(m.keySet());
    m.put("erl file", erl);
    m.put("hrl file", hrl);
    m.put("non-physical erl file (no virtual file)", light);
    m.put("leaf in a file without a virtual file", light.findElementAt(light.getText().indexOf("hi")));
    m.put("atom leaf made by a macro", erl.findElementAt(erl.getText().indexOf("hello).")));
    m.put("atom of a use", erl.findElementAt(erl.getText().lastIndexOf("hello")).getParent());
    return m;
  }

  public void testEntryPointsTakeAnyElement() throws Exception {
    List<String> nonErlang = new ArrayList<>();
    Map<String, PsiElement> all = elements(nonErlang);
    myFixture.configureFromExistingVirtualFile(((PsiFile)all.get("erl file")).getVirtualFile());
    myFixture.doHighlighting();
    List<String> problems = new ArrayList<>();
    ErlangFindUsagesHandlerFactory findUsages = new ErlangFindUsagesHandlerFactory();
    ErlangMacroLayerRenameProcessor layerRename = new ErlangMacroLayerRenameProcessor();
    ErlangRenameMacroProcessor macroRename = new ErlangRenameMacroProcessor();
    ErlangTargetElementEvaluator evaluator = new ErlangTargetElementEvaluator();
    ErlangMacroLayerGotoDeclarationHandler gotoHandler = new ErlangMacroLayerGotoDeclarationHandler();
    ErlangDescriptionProvider description = new ErlangDescriptionProvider();
    ErlangMacroLayerExpansionErrorAnnotator annotator = new ErlangMacroLayerExpansionErrorAnnotator();
    ErlangMacroLayerExternalAnnotatorsFilter filter = new ErlangMacroLayerExternalAnnotatorsFilter();
    for (Map.Entry<String, PsiElement> e : all.entrySet()) {
      String name = e.getKey();
      PsiElement el = e.getValue();
      boolean foreign = nonErlang.contains(name);
      Map<String, Object> answers = new LinkedHashMap<>();
      try {
        ReadAction.run(() -> {
          answers.put("canFindUsages", findUsages.canFindUsages(el));
          answers.put("platform canFindUsages", ((FindManagerImpl)FindManager.getInstance(getProject())).getFindUsagesManager().canFindUsages(el));
          answers.put("layer rename", layerRename.canProcessElement(el));
          answers.put("macro rename", macroRename.canProcessElement(el));
          answers.put("named element", evaluator.getNamedElement(el));
          answers.put("goto", gotoHandler.getGotoDeclarationTargets(el, 0, myFixture.getEditor()));
          // master's provider logs an error for some Erlang elements (an atom) in this location: not asked for those
          if (foreign || name.startsWith("atom leaf")) answers.put("description", description.getElementDescription(el, UsageViewTypeLocation.INSTANCE));
          answers.put("made name", ErlangMacroLayer.isNameMadeByMacro(el));
          answers.put("made declaration", ErlangMacroLayer.isMadeNameDeclaration(el));
          answers.put("header body variable", ErlangMacroLayer.isHeaderBodyVariable(el));
          answers.put("rename target", ErlangMacroLayer.renameTargetOfMacroToken(el) == el ? "itself" : "other");
          answers.put("token references", ErlangMacroLayerTokenReference.find(el).length);
          answers.put("bare references", org.intellij.erlang.psi.impl.ErlangBareMacroReference.find(el).length);
          ErlangMacroLayer.prepareSearch(el, GlobalSearchScope.projectScope(getProject()));
          if (el instanceof PsiFile f) {
            answers.put("annotator info", annotator.collectInformation(f));
            answers.put("annotator prohibited", filter.isProhibited(annotator, f));
          }
        });
        if (!(el instanceof PsiDirectory) && !(el instanceof PsiBinaryFile)) {
          inBackgroundWithIndicator(() -> ReferencesSearch.search(el, GlobalSearchScope.projectScope(getProject())).findAll().size());
        }
      }
      catch (Throwable t) {
        problems.add(name + ": " + t);
        continue;
      }
      System.out.println("ARBITRARY " + name + ": " + answers);
      if (foreign) {
        // master's answers for an element that is not Erlang code: no find usages of this factory, no rename of these
        // processors, no named element, no go-to target, no description, no layer role, no annotator information
        Object[] expectedFalse = {answers.get("canFindUsages"), answers.get("layer rename"), answers.get("macro rename"), answers.get("made name"),
          answers.get("made declaration"), answers.get("header body variable")};
        for (Object o : expectedFalse) if (!Boolean.FALSE.equals(o)) problems.add(name + ": a layer answer " + answers);
        if (answers.get("named element") != null || answers.get("goto") != null || answers.get("description") != null ||
            answers.get("annotator info") != null || !answers.get("token references").equals(0) || !answers.get("bare references").equals(0)) {
          problems.add(name + ": a layer answer " + answers);
        }
      }
    }
    try {
      ReadAction.run(() -> {
        ErlangMacroLayer.isNameMadeByMacro(null);
        gotoHandler.getGotoDeclarationTargets(null, 0, myFixture.getEditor());
      });
    }
    catch (Throwable t) {
      problems.add("null: " + t);
    }
    assertEquals(List.of(), problems);
  }
}
