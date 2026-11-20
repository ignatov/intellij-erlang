package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/** Spike 3: losses that the design accepts, measured on fixtures. Never fails on a finding; prints LAYER-LOSS lines. */
public class ErlangMacroLayerLossesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private static final String DEF = "-module(def).\n-export([f/0, hello/0]).\n-define(DEF(N), N() -> ok).\n?DEF(hello).\nf() -> hello().\n";

  public void testRemoteCallAndCompletion() {
    myFixture.addFileToProject("def.erl", DEF);
    for (boolean remote : new boolean[]{false, true}) {
      com.intellij.openapi.Disposable d = Disposer.newDisposable();
      ErlangMacroLayer.setRemoteForTests(remote, d);
      try {
        PsiFile user = myFixture.configureByText("user" + remote + ".erl", "-module(user" + remote + ").\ng() -> def:hello().\n");
        int built = ErlangMacroLayer.EXPANSIONS_BUILT.get();
        long t0 = System.nanoTime();
        PsiReference ref = user.findReferenceAt(user.getText().indexOf("hello"));
        PsiElement target = ref == null ? null : ref.resolve();
        long us = (System.nanoTime() - t0) / 1000;
        System.out.println("LAYER-LOSS remote=" + remote + " def:hello() -> " +
                           (target == null ? "null" : target.getContainingFile().getName() + "@" + target.getTextOffset() + " '" + target.getText() + "'") +
                           " first resolve us=" + us + " copies built=" + (ErlangMacroLayer.EXPANSIONS_BUILT.get() - built));
        myFixture.configureByText("comp" + remote + ".erl", "-module(comp" + remote + ").\ng() -> def:<caret>\n");
        LookupElement[] items = myFixture.completeBasic();
        List<String> names = new ArrayList<>();
        if (items != null) for (LookupElement e : items) names.add(e.getLookupString());
        System.out.println("LAYER-LOSS remote=" + remote + " completion after def: " + names);
      }
      finally {
        Disposer.dispose(d);
      }
    }
  }

  public void testLocalCompletionOfFunctionMadeByMacro() {
    myFixture.configureByText("loc.erl", DEF.replace("f() -> hello().", "f() -> hel<caret>"));
    LookupElement[] items = myFixture.completeBasic();
    List<String> names = new ArrayList<>();
    if (items != null) for (LookupElement e : items) names.add(e.getLookupString());
    else names.add("(single item inserted: " + myFixture.getEditor().getDocument().getText().contains("hello(") + ")");
    System.out.println("LAYER-LOSS local completion 'hel': " + names);
  }
}
