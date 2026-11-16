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

package org.intellij.erlang.elp;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.elp.ElpPreflight.Facts;
import org.intellij.erlang.elp.ElpPreflight.Layout;
import org.intellij.erlang.elp.ElpPreflight.Reason;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.EnumSet;
import java.util.Set;

public class ElpPreflightTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String SDK_HOME = new File("testData/mockSdk-R27").getAbsolutePath();
  private static final Facts GOOD = facts("27", "/tools/rebar3", "3.28.0", "/opt/elp/bin/elp", true);
  private static final Layout REBAR = new Layout("/p", true, true, false, false);

  private ElpSettings.ElpSettingsState myOldElpState;
  private String myOldRebarPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldElpState = new ElpSettings.ElpSettingsState();
    myOldElpState.path = old.path;
    myOldElpState.source = old.source;
    myOldElpState.mode = old.mode;
    myOldElpState.eqwalizer = old.eqwalizer;
    myOldRebarPath = RebarSettings.getInstance(getProject()).getRebarPath();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldElpState);
      RebarSettings.getInstance(getProject()).setRebarPath(myOldRebarPath);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ApplicationManager.getApplication().runWriteAction(
          (Computable<Sdk>)() -> ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_27_0));
      }
    };
  }

  public void testRebarProjectWithEverythingIsOffered() {
    ElpPreflight.Result result = ElpPreflight.evaluate(GOOD, REBAR);
    assertTrue(result.canStart());
    assertTrue(result.offerEligible());
    assertEmpty(result.problems());
  }

  public void testEachReasonThatBlocksTheStart() {
    assertReasons(EnumSet.of(Reason.NO_SDK), facts(null, "/tools/rebar3", "3.28.0", "/opt/elp/bin/elp", true), REBAR);
    assertReasons(EnumSet.of(Reason.OTP_TOO_OLD), facts("26", "/tools/rebar3", "3.28.0", "/opt/elp/bin/elp", true), REBAR);
    assertReasons(EnumSet.of(Reason.OTP_TOO_OLD), facts("R16B", "/tools/rebar3", "3.28.0", "/opt/elp/bin/elp", true), REBAR);
    assertReasons(EnumSet.of(Reason.NO_REBAR3), facts("27", null, null, "/opt/elp/bin/elp", true), REBAR);
    assertReasons(EnumSet.of(Reason.REBAR3_NOT_RUNNABLE), facts("27", "/tools/rebar3", null, "/opt/elp/bin/elp", true), REBAR);
    assertReasons(EnumSet.of(Reason.REBAR3_TOO_OLD), facts("27", "/tools/rebar3", "3.23.0", "/opt/elp/bin/elp", true), REBAR);
    assertReasons(EnumSet.of(Reason.NO_SERVER_BINARY), facts("27", "/tools/rebar3", "3.28.0", "/no/elp", false), REBAR);
    assertTrue(ElpPreflight.evaluate(facts("28", "/tools/rebar3", "3.24", "/opt/elp/bin/elp", true), REBAR).offerEligible());
  }

  /**
   * A flat directory of modules, or a project with no manifest and no top-level src: ELP would walk up to an ancestor named src.
   */
  public void testFlatDirectoryNeverStarts() {
    ElpPreflight.Result result = ElpPreflight.evaluate(GOOD, new Layout("/p", false, false, false, false));
    assertFalse(result.canStart());
    assertEquals(EnumSet.of(Reason.UNSUPPORTED_LAYOUT, Reason.NO_REBAR_CONFIG), result.reasons());
    assertFalse(ElpPreflight.evaluate(GOOD, null).canStart());
  }

  /** A plain src project and an erlang.mk project can start; rebar3 is not needed there, but ELP is not offered. */
  public void testProjectsWithoutRebarConfigStartButAreNotOffered() {
    Facts noRebar3 = facts("27", null, null, "/opt/elp/bin/elp", true);
    ElpPreflight.Result plain = ElpPreflight.evaluate(noRebar3, new Layout("/p", false, true, false, false));
    assertTrue(plain.canStart());
    assertEquals(EnumSet.of(Reason.NO_REBAR_CONFIG), plain.reasons());
    ElpPreflight.Result erlangMk = ElpPreflight.evaluate(noRebar3, new Layout("/p", false, true, true, true));
    assertTrue(erlangMk.canStart());
    assertEquals(EnumSet.of(Reason.ERLANG_MK), erlangMk.reasons());
  }

  public void testOfferedOnMacOsOnly() {
    ElpPreflight.Result result = ElpPreflight.evaluate(new Facts(true, "27", "/tools/rebar3", "3.28.0", null, configuration("/opt/elp/bin/elp"),
                                                                 null, false, ""), REBAR);
    assertTrue(result.canStart());
    assertFalse(result.offerEligible());
    assertEquals(EnumSet.of(Reason.NOT_MACOS), result.reasons());
  }

  public void testRebar3VersionAndOtpRelease() {
    assertEquals("3.28.0", ElpPreflight.parseRebar3Version("rebar 3.28.0 on Erlang/OTP 29 Erts 17.1\n"));
    assertNull(ElpPreflight.parseRebar3Version("escript: command not found"));
    assertTrue(ElpPreflight.isAtLeastOtp("27", "27"));
    assertTrue(ElpPreflight.isAtLeastOtp("29.1", "27"));
    assertFalse(ElpPreflight.isAtLeastOtp("26", "27"));
    assertFalse(ElpPreflight.isAtLeastOtp("R16B", "27"));
  }

  /**
   * The whole check on the project: SDK, the layout of its root, rebar3 run for its version, and the ELP binary.
   */
  public void testCheckOfTheProject() throws Exception {
    String elp = script("elp", "exit 0");
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = elp;
    state.mode = ElpMode.ELP.name();
    ElpSettings.getInstance().loadState(state);
    myFixture.addFileToProject("src/a.erl", "-module(a).");
    myFixture.addFileToProject("rebar.config", "{erl_opts, []}.");

    RebarSettings.getInstance(getProject()).setRebarPath(script("rebar3", "echo 'rebar 3.28.0 on Erlang/OTP 29 Erts 17.1'"));
    assertEquals(EnumSet.of(Reason.NO_SDK), blockingReasons(check()));

    setUpProjectSdk();
    ElpPreflight.Result result = check();
    assertTrue(result.problems().toString(), result.canStart());
    assertEquals("3.28.0", ElpPreflight.getInstance(getProject()).cachedFacts().rebar3Version());

    RebarSettings.getInstance(getProject()).setRebarPath(script("rebar3", "echo 'rebar 3.20.0 on Erlang/OTP 26 Erts 14.2'"));
    assertEquals(EnumSet.of(Reason.REBAR3_TOO_OLD), blockingReasons(check()));

    RebarSettings.getInstance(getProject()).setRebarPath(script("rebar3", "echo 'escript: not found' >&2; exit 127"));
    ElpPreflight.Result notRunnable = check();
    assertEquals(EnumSet.of(Reason.REBAR3_NOT_RUNNABLE), blockingReasons(notRunnable));
    assertTrue(notRunnable.blocking().get(0).text(), notRunnable.blocking().get(0).text().contains("exit code 127, escript: not found"));
  }

  /**
   * The rebar3 path is a project setting that a cloned repository can ship: in a project the IDE does not trust, the check runs
   * nothing and says why; once the project is trusted, it runs rebar3 as before.
   */
  public void testNothingIsRunInAProjectThatIsNotTrusted() throws Exception {
    setUpProjectSdk();
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = script("elp", "exit 0");
    state.mode = ElpMode.ELP.name();
    ElpSettings.getInstance().loadState(state);
    myFixture.addFileToProject("src/a.erl", "-module(a).");
    myFixture.addFileToProject("rebar.config", "{erl_opts, []}.");
    Path ran = FileUtil.createTempDirectory("elp-preflight-ran", null, true).toPath().resolve("ran");
    RebarSettings.getInstance(getProject()).setRebarPath(
      script("rebar3", "touch '" + ran + "'\necho 'rebar 3.28.0 on Erlang/OTP 29 Erts 17.1'"));

    ElpPreflight.trustInTests(false, getTestRootDisposable());
    ElpPreflight.Result untrusted = check();
    assertFalse("rebar3 was started", Files.exists(ran));
    assertEquals(EnumSet.of(Reason.NOT_TRUSTED), untrusted.reasons());
    assertFalse(untrusted.canStart());
    assertFalse(untrusted.offerEligible());
    String text = assertOneElement(untrusted.blocking()).text();
    assertTrue(text, text.contains("not trusted"));
    assertFalse(ElpPreflight.getInstance(getProject()).cachedFacts().trusted());

    ElpPreflight.trustInTests(true, getTestRootDisposable());
    ElpPreflight.Result trusted = check();
    assertTrue("rebar3 was not started", Files.exists(ran));
    assertTrue(trusted.problems().toString(), trusted.canStart());
  }

  public void testFactsAreForgottenWhenTheRootsChange() throws Exception {
    PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(
      () -> ElpPreflight.getInstance(getProject()).gatherFacts()), 30_000);
    assertNotNull(ElpPreflight.getInstance(getProject()).cachedFacts());
    setUpProjectSdk();
    assertNull(ElpPreflight.getInstance(getProject()).cachedFacts());
  }

  public void testToldOncePerSetOfReasons() {
    ElpPreflight preflight = ElpPreflight.getInstance(getProject());
    ElpPreflight.Result noSdk = ElpPreflight.evaluate(facts(null, "/tools/rebar3", "3.28.0", "/opt/elp/bin/elp", true), REBAR);
    assertTrue(preflight.shouldTell(noSdk));
    assertFalse(preflight.shouldTell(noSdk));
    assertFalse("nothing to tell", preflight.shouldTell(ElpPreflight.evaluate(GOOD, REBAR)));
    assertTrue(preflight.shouldTell(ElpPreflight.evaluate(facts("27", null, null, "/opt/elp/bin/elp", true), REBAR)));
  }

  private ElpPreflight.Result check() {
    return PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(
      () -> ElpPreflight.check(getProject())), 60_000);
  }

  private static Set<Reason> blockingReasons(ElpPreflight.Result result) {
    Set<Reason> reasons = EnumSet.noneOf(Reason.class);
    for (ElpPreflight.Problem problem : result.blocking()) reasons.add(problem.reason());
    return reasons;
  }

  private static void assertReasons(Set<Reason> expected, Facts facts, Layout layout) {
    ElpPreflight.Result result = ElpPreflight.evaluate(facts, layout);
    assertEquals(expected, result.reasons());
    assertFalse(result.canStart());
    for (ElpPreflight.Problem problem : result.problems()) assertFalse(problem.text().isBlank());
  }

  private static Facts facts(String otp, String rebar3, String rebar3Version, String server, boolean runnable) {
    ElpPreflight.Problem serverProblem = runnable ? null : new ElpPreflight.Problem(Reason.NO_SERVER_BINARY, "The ELP binary " + server +
                                                                                                        " is not an executable file.");
    return new Facts(otp != null, otp, rebar3, rebar3Version, null, configuration(server), serverProblem, true, "");
  }

  private static ElpConfiguration configuration(String server) {
    return new ElpConfiguration(server, ElpMode.ELP);
  }

  /** An executable shell script named {@code name} in a new directory. */
  private static String script(String name, String body) throws Exception {
    Path dir = FileUtil.createTempDirectory("elp-preflight", null, true).toPath();
    Path script = Files.writeString(dir.resolve(name), "#!/bin/sh\n" + body + "\n");
    assertTrue(script.toFile().setExecutable(true));
    return script.toString();
  }
}
