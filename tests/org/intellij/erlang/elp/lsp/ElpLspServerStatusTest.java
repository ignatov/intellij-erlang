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

import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpServerStatus;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.EnumSet;
import java.util.List;

public class ElpLspServerStatusTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testRegisteredFromTheOptionalDescriptor() {
    assertInstanceOf(getProject().getService(ElpServerStatus.class), ElpLspServerStatus.class);
  }

  public void testNothingIsServedWithoutARunningClient() {
    var file = myFixture.addFileToProject("src/a.erl", "-module(a).").getVirtualFile();
    for (ElpEngine.Feature feature : ElpEngine.Feature.values()) {
      assertFalse(feature.name(), new ElpLspServerStatus(getProject()).isServing(file, feature));
    }
  }

  public void testCapabilitiesOfElp() {
    // What ELP 1.1.0 announced in the initialize response of the owner's run.
    ServerCapabilities capabilities = new ServerCapabilities();
    capabilities.setCompletionProvider(new CompletionOptions(true, List.of(":", "#", "?", ".", "-", "\\")));
    capabilities.setInlayHintProvider(new InlayHintRegistrationOptions());
    capabilities.setHoverProvider(true);
    capabilities.setFoldingRangeProvider(true);
    capabilities.setCodeActionProvider(new CodeActionOptions(List.of(CodeActionKind.QuickFix)));
    capabilities.setSelectionRangeProvider(true);
    capabilities.setDefinitionProvider(true);
    capabilities.setReferencesProvider(true);
    capabilities.setRenameProvider(new RenameOptions(false));
    capabilities.setSignatureHelpProvider(new SignatureHelpOptions(List.of("(", ",")));
    capabilities.setDocumentHighlightProvider(true);
    capabilities.setWorkspaceSymbolProvider(true);
    capabilities.setCallHierarchyProvider(true);
    for (ElpEngine.Feature feature : ElpEngine.Feature.values()) {
      assertTrue(feature.name(), ElpLspServerStatus.supports(capabilities, feature));
    }
  }

  /** ELP is asked only for what it announced; under a served root the plugin stays silent all the same (ElpEngine). */
  public void testMissingCapabilitiesAreNotAskedFor() {
    ServerCapabilities capabilities = new ServerCapabilities();
    capabilities.setHoverProvider(false);
    capabilities.setFoldingRangeProvider(Either.forLeft(false));
    EnumSet<ElpEngine.Feature> supported = EnumSet.noneOf(ElpEngine.Feature.class);
    for (ElpEngine.Feature feature : ElpEngine.Feature.values()) {
      if (ElpLspServerStatus.supports(capabilities, feature)) supported.add(feature);
    }
    assertEquals(EnumSet.of(ElpEngine.Feature.DIAGNOSTICS), supported);
    assertFalse(ElpLspServerStatus.supports(null, ElpEngine.Feature.DIAGNOSTICS));
  }
}
