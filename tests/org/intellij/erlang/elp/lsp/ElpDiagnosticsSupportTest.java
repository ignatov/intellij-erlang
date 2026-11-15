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

import com.intellij.lang.annotation.HighlightSeverity;
import junit.framework.TestCase;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;

public class ElpDiagnosticsSupportTest extends TestCase {
  public void testEqwalizerDiagnosticsAreHiddenByDefault() {
    ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(false);
    assertNull(support.getHighlightSeverity(diagnostic("eqWAlizer", "incompatible_types", DiagnosticSeverity.Error)));
    assertNull(support.getHighlightSeverity(diagnostic("eqWAlizer", "expected_fun_type", DiagnosticSeverity.Warning)));
  }

  public void testOtherElpDiagnosticsAreShown() {
    ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(false);
    assertEquals(HighlightSeverity.ERROR, support.getHighlightSeverity(diagnostic("elp", "L1262", DiagnosticSeverity.Error)));
    assertEquals(HighlightSeverity.WARNING, support.getHighlightSeverity(diagnostic("elp", "W0017", DiagnosticSeverity.Warning)));
    assertNotNull(support.getHighlightSeverity(diagnostic(null, "W0066", DiagnosticSeverity.Hint)));
  }

  public void testEqwalizerDiagnosticsAreShownWhenSwitchedOn() {
    ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(true);
    assertEquals(HighlightSeverity.ERROR,
                 support.getHighlightSeverity(diagnostic("eqWAlizer", "incompatible_types", DiagnosticSeverity.Error)));
    assertEquals(HighlightSeverity.ERROR, support.getHighlightSeverity(diagnostic("elp", "L1262", DiagnosticSeverity.Error)));
  }

  public void testEqwalizerIsRecognizedBySourceOnly() {
    assertTrue(ElpDiagnosticsSupport.isEqwalizer(diagnostic("eqWAlizer", "incompatible_types", DiagnosticSeverity.Error)));
    assertTrue(ElpDiagnosticsSupport.isEqwalizer(diagnostic("eqwalizer", "anything", DiagnosticSeverity.Error)));
    assertFalse(ElpDiagnosticsSupport.isEqwalizer(diagnostic("elp", "incompatible_types", DiagnosticSeverity.Error)));
    assertFalse(ElpDiagnosticsSupport.isEqwalizer(diagnostic(null, "incompatible_types", DiagnosticSeverity.Error)));
  }

  private static Diagnostic diagnostic(String source, String code, DiagnosticSeverity severity) {
    return new Diagnostic(new Range(new Position(0, 0), new Position(0, 1)), "message", severity, source, code);
  }
}
