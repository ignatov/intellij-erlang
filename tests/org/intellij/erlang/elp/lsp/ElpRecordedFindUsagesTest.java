/*
 * Copyright 2012-2026 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.elp.lsp;

import com.google.gson.reflect.TypeToken;
import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.openapi.util.Segment;
import com.intellij.openapi.util.TextRange;
import com.intellij.platform.lsp.api.LspServerState;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.usageView.UsageInfo;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.Location;
import org.intellij.erlang.elp.ElpMode;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

/**
 * Find usages with the answers of the real ELP to {@code textDocument/references}: what the user gets in the {@code ELP} position
 * for each kind of name, the fallback to the plugin when ELP serves nothing, and never both engines' usages at once.
 */
public class ElpRecordedFindUsagesTest extends ElpRecordedProjectTestBase {
  private static final String REFERENCES = "textDocument/references";

  /** Every recorded kind: ELP is asked once, at the recorded position, and its usages are exactly what the user gets. */
  public void testUsagesAreElpsAnswerForEveryKind() {
    List<String> checked = new ArrayList<>();
    for (ElpRecordedAnswers.Case recorded : myAnswers.cases(REFERENCES)) {
      List<Location> locations = recorded.result(new TypeToken<List<Location>>() {}.getType());
      if (locations == null || locations.isEmpty()) continue;
      PsiElement target = targetAt(recorded);
      String name = ((PsiNamedElement)target).getName();

      myServer.clearRequests();
      List<String> usages = usages(target);
      assertEquals(recorded.toString(), 1, myServer.requests(REFERENCES).size());
      assertEmpty(recorded + ": ELP was asked at another position than the recorder", myServer.unanswered());

      List<String> expected = new ArrayList<>();
      for (Location location : locations) expected.add(describe(location.getUri(), location.getRange()));
      assertEquals(recorded.toString(), new TreeSet<>(expected), new TreeSet<>(usages));
      assertEquals(recorded + ": no usage twice", new TreeSet<>(usages).size(), usages.size());
      for (String usage : usages) assertTrue(recorded + ": " + usage, usage.endsWith(" " + name));
      checked.add(recorded.kind());
    }
    assertEquals(List.of("module", "exported_function", "function_used_in_macro_argument", "local_function", "variable", "record",
                         "record_field", "macro", "macro_with_arguments", "type"), checked);
  }

  /** The usages of a module include the places the plugin does not search, such as {@code -behaviour} and {@code -import}. */
  public void testModuleUsages() {
    PsiElement module = targetAt(myAnswers.get(REFERENCES, "module"));
    assertEquals(List.of("src/shop.erl:10:13 shop_backend", "src/shop.erl:12:8 shop_backend", "src/shop.erl:17:14 shop_backend",
                         "src/shop.erl:25:17 shop_backend", "src/shop_defects.erl:10:9 shop_backend",
                         "src/shop_memory.erl:2:12 shop_backend", "src/shop_memory.erl:8:13 shop_backend"),
                 sorted(usages(module)));
  }

  /** A function used inside a macro argument: ELP finds the call in {@code ?LOG(price(Name))}. */
  public void testFunctionUsedInAMacroArgument() {
    PsiElement price = targetAt(myAnswers.get(REFERENCES, "function_used_in_macro_argument"));
    assertEquals(List.of("src/shop.erl:19:8 price", "src/shop.erl:27:8 price"), sorted(usages(price)));
  }

  /**
   * ELP answers no usages of an exported function that nothing calls, because it leaves out the {@code -export} entry; then the
   * plugin searches, as in the Plugin position, and the user gets the {@code -export} and {@code -spec} entries.
   */
  public void testPluginSearchesWhenElpFindsNothing() {
    ElpRecordedAnswers.Case label = myAnswers.get(REFERENCES, "exported_function_without_callers");
    assertEquals("[]", String.valueOf(label.result()));
    PsiElement target = targetAt(label);

    position(ElpMode.PLUGIN);
    List<String> plugin = usages(target);
    assertEquals(List.of("src/shop_typed.erl:3:10 label", "src/shop_typed.erl:6:7 label"), sorted(plugin));
    assertEmpty("the Plugin position asks no server", myServer.requests());

    position(ElpMode.ELP);
    assertEquals(plugin, usages(target));
    assertSize(1, myServer.requests(REFERENCES));
    assertEmpty(myServer.unanswered());
  }

  /**
   * ELP answers the usages of a callback with an empty list, but the plugin has no target on a {@code -callback} name, so the user
   * cannot ask for them in either position.
   */
  public void testNoTargetOnACallback() {
    ElpRecordedAnswers.Case callback = myAnswers.get(REFERENCES, "callback");
    assertEquals("[]", String.valueOf(callback.result()));
    openAt(callback);
    assertNull(TargetElementUtil.findTargetElement(myFixture.getEditor(), TargetElementUtil.getInstance().getAllAccepted()));
  }

  /**
   * The usages of an exported function differ between the positions: the plugin also lists the {@code -export} and {@code -spec}
   * entries, ELP's references without the declaration leave them out. In the Plugin position ELP is not asked, although it runs.
   */
  public void testExportAndSpecEntriesAreUsagesOnlyInThePluginPosition() {
    PsiElement lookup = targetAt(myAnswers.get(REFERENCES, "exported_function"));
    position(ElpMode.PLUGIN);
    assertEquals(List.of("src/shop.erl:12:21 lookup", "src/shop_backend.erl:11:7 lookup", "src/shop_backend.erl:3:10 lookup"),
                 sorted(usages(lookup)));
    assertEmpty(myServer.requests());

    position(ElpMode.ELP);
    assertEquals(List.of("src/shop.erl:12:21 lookup"), sorted(usages(lookup)));
  }

  public void testPluginAnswersWhileTheServerStartsOrAfterItStopped() {
    PsiElement lookup = targetAt(myAnswers.get(REFERENCES, "exported_function"));
    position(ElpMode.PLUGIN);
    List<String> plugin = usages(lookup);
    position(ElpMode.ELP);
    for (LspServerState state : List.of(LspServerState.Initializing, LspServerState.ShutdownNormally,
                                        LspServerState.ShutdownUnexpectedly)) {
      myClient.setState(state);
      assertEquals(state.name(), plugin, usages(lookup));
      assertEmpty(state.name(), myServer.requests());
    }
  }

  public void testPluginAnswersWhenTheServerDoesNotOfferReferences() {
    PsiElement lookup = targetAt(myAnswers.get(REFERENCES, "exported_function"));
    position(ElpMode.PLUGIN);
    List<String> plugin = usages(lookup);
    position(ElpMode.ELP);
    InitializeResult withoutReferences = myAnswers.initializeResult();
    withoutReferences.getCapabilities().setReferencesProvider(false);
    myClient.setInitializeResult(withoutReferences);
    assertFalse(new ElpFindUsagesHandlerFactory().canFindUsages(lookup));
    assertEquals(plugin, usages(lookup));
    assertEmpty(myServer.requests());
  }

  /** The variable's usages: ELP leaves out the declaration, as the plugin does. */
  public void testVariableUsagesMatchThePlugin() {
    PsiElement name = targetAt(myAnswers.get(REFERENCES, "variable"));
    List<String> elp = sorted(usages(name));
    position(ElpMode.PLUGIN);
    assertEquals(sorted(usages(name)), elp);
  }

  @NotNull
  private PsiElement targetAt(@NotNull ElpRecordedAnswers.Case recorded) {
    openAt(recorded);
    PsiElement target = TargetElementUtil.findTargetElement(myFixture.getEditor(), TargetElementUtil.getInstance().getAllAccepted());
    assertNotNull(recorded.toString(), target);
    return target;
  }

  /** The usages that Find Usages shows: the file, the line and column, and the text of each. */
  @NotNull
  private List<String> usages(@NotNull PsiElement target) {
    Collection<UsageInfo> usages = myFixture.findUsages(target);
    List<String> result = new ArrayList<>();
    for (UsageInfo usage : usages) {
      Segment segment = usage.getSegment();
      assertNotNull(segment);
      result.add(describe(usage.getVirtualFile(), TextRange.create(segment)));
    }
    return result;
  }

  @NotNull
  private static List<String> sorted(@NotNull Collection<String> values) {
    Set<String> set = new TreeSet<>(values);
    return new ArrayList<>(set);
  }
}
