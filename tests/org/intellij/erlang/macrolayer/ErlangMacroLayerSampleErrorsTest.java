package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * D4: false errors of the owner's samples (one_file/m_made_by_macro.erl, one_file/m_positions.erl), which erlc 29.1
 * compiles. Every Erlang inspection is on.
 */
public class ErlangMacroLayerSampleErrorsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableAll(myFixture);
  }

  private PsiFile errors(String name, String text, String... expected) {
    PsiFile f = myFixture.addFileToProject(name, text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    List<String> errors = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity() == HighlightSeverity.ERROR) errors.add(h.getDescription());
    }
    assertEquals(List.of(expected), errors);
    return f;
  }

  /** A variable bound by a parameter of a function that a top-level macro use makes. */
  public void testVariableOfAFunctionMadeByATopLevelMacro() {
    errors("mm.erl", """
      -module(mm).
      -export([run/0]).
      -define(RECORD(Name, F1, F2), -record(Name, {F1 = 0, F2 = 0})).
      -define(FUN1(Name, Arg, Body), Name(Arg) -> Body).
      ?RECORD(shape, width, height).
      ?FUN1(area, S, S#shape.width * S#shape.height).
      ?FUN1(perimeter, S, 2 * (S#shape.width + S#shape.height)).
      run() ->
        R = #shape{width = 3, height = 4},
        {area(R), perimeter(R)}.
      """);
  }

  /** A record whose name in -record is a macro ({@code -record(?REC, ...)}), used by its plain name. */
  public void testRecordNamedByAMacroUsedByItsName() throws Exception {
    PsiFile f = errors("mp.erl", """
      -module(mp).
      -export([make/0]).
      -define(REC, item).
      -record(?REC, {id, size = 10}).
      -spec make() -> #item{}.
      make() ->
        R = #?REC{id = 1},
        R#item{size = 2}.
      """);
    String text = f.getText();
    for (String marker : List.of("#it|em{}", "R#it|em{size")) {
      int at = text.indexOf(marker.replace("|", "")) + marker.indexOf('|');
      PsiElement t = inBackgroundWithIndicator(() -> {
        PsiReference ref = f.findReferenceAt(at);
        return ref == null ? null : ref.resolve();
      });
      assertNotNull(marker, t);
      assertTrue(marker + ": " + t.getText(), t.getText().startsWith("-record(?REC"));
    }
  }
}
