package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Random;
import java.util.concurrent.Future;

/**
 * The base of the macro fuzz tests: a light project with a mock Erlang SDK (the OTP 29 release over the headers of the
 * default mock SDK, so that {@code -include_lib("eunit/include/eunit.hrl")} resolves), the case loop (a short fixed-seed
 * smoke run by default, a time-boxed run with {@code MACRO_FUZZ_SECONDS}, one case with {@code MACRO_FUZZ_CASE}) and the
 * recorder. A test fails when a failure has a signature that is not in the known list.
 */
public abstract class MacroFuzzTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  protected FuzzRecorder rec;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public interface CaseBody {
    void run(int index, @NotNull Random random, @NotNull String caseId) throws Throwable;
  }

  /** Runs {@code smokeCases} cases (or, with {@code MACRO_FUZZ_SECONDS}, cases until the time is over). */
  protected void runCases(@NotNull String family, int smokeCases, @NotNull CaseBody body) throws Throwable {
    rec = new FuzzRecorder(getClass().getName() + "#" + getName());
    long base = FuzzConfig.seed();
    int only = FuzzConfig.onlyCase();
    long seconds = FuzzConfig.seconds();
    long t0 = System.currentTimeMillis();
    long deadline = t0 + seconds * 1000;
    System.err.println("MACRO-FUZZ: " + getClass().getSimpleName() + "." + getName() + " arm=" + LayerHooks.arm() + " seed=" + base +
                       (only >= 0 ? " case=" + only : seconds > 0 ? " seconds=" + seconds : " smoke=" + smokeCases));
    rec.runAll(() -> {
      for (int i = only >= 0 ? only : 0; ; i++) {
        if (only >= 0 && i != only) break;
        if (only < 0 && seconds <= 0 && i >= smokeCases) break;
        if (only < 0 && seconds > 0 && System.currentTimeMillis() > deadline) break;
        long s = FuzzConfig.caseSeed(base, family, i);
        String caseId = family + ":" + i;
        rec.startCase(caseId, s, Map.of());
        int index = i;
        rec.guard("case " + caseId, () -> body.run(index, new Random(s), caseId));
        if (seconds > 0 && i % 20 == 0) {
          System.err.println("MACRO-FUZZ: progress " + caseId + " findings=" + rec.findings().size() + " actions=" + rec.actions());
        }
      }
    });
    String summary = rec.finish(System.currentTimeMillis() - t0);
    if (!rec.unknownSignatures().isEmpty() && !FuzzConfig.isLongRun()) fail(summary);
  }

  /** Builds the layer's expanded copy in the background, as the daemon does after a change (no-op on master). */
  protected void warm(@NotNull PsiFile file) {
    if (!LayerHooks.present()) return;
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    rec.guard("warm-copy " + file.getName(), () -> {
      Future<?> f = ApplicationManager.getApplication().executeOnPooledThread(
        () -> ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> LayerHooks.copyFile(file)), new EmptyProgressIndicator()));
      PlatformTestUtil.waitForFuture(f, FuzzConfig.actionMillis());
    });
  }

  protected FuzzWorld world(@NotNull Map<String, String> files, boolean extras) {
    FuzzWorld w = new FuzzWorld(myFixture, files, extras);
    rec.setCaseFiles(files);
    return w;
  }

  protected void open(@NotNull VirtualFile f) {
    myFixture.configureFromExistingVirtualFile(f);
  }
}
