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
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * {@link ElpDiagnosticsSupport} on the diagnostics that the real ELP published for the fixture project, eqWAlizer's included.
 */
public class ElpRecordedDiagnosticsSupportTest extends TestCase {
  private static final List<String> COMPILING_MODULES = List.of("src/shop.erl", "src/shop_backend.erl", "src/shop_memory.erl",
                                                                "src/shop_typed.erl", "src/shop.hrl");

  private final Map<String, List<Diagnostic>> myDiagnostics = new ElpRecordedAnswers("file:///project").diagnostics();

  /** The record holds what the tests rely on: ELP's own and erlc's diagnostics, and eqWAlizer's type errors at error severity. */
  public void testRecordedDiagnostics() {
    List<Diagnostic> eqwalizer = eqwalizer();
    assertEquals(Set.of("src/shop.erl", "src/shop_typed.erl"), filesWith(eqwalizer));
    for (Diagnostic diagnostic : eqwalizer) {
      assertEquals(String.valueOf(diagnostic.getMessage()), DiagnosticSeverity.Error, diagnostic.getSeverity());
      assertEquals("incompatible_types", diagnostic.getCode().getLeft());
    }
    assertFalse(others().isEmpty());
  }

  public void testEqwalizerIsHiddenByDefault() {
    ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(false);
    for (Diagnostic diagnostic : eqwalizer()) assertNull(String.valueOf(diagnostic.getMessage()), support.getHighlightSeverity(diagnostic));
  }

  public void testEqwalizerIsAWarningWhenSwitchedOn() {
    ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(true);
    for (Diagnostic diagnostic : eqwalizer()) {
      assertEquals(String.valueOf(diagnostic.getMessage()), HighlightSeverity.WARNING, support.getHighlightSeverity(diagnostic));
    }
  }

  /** ELP's own and erlc's diagnostics keep their severity, whatever the eqWAlizer switch says. */
  public void testOtherDiagnosticsKeepTheirSeverity() {
    for (boolean showEqwalizer : new boolean[]{false, true}) {
      ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(showEqwalizer);
      for (Diagnostic diagnostic : others()) {
        HighlightSeverity expected = switch (diagnostic.getSeverity()) {
          case Error -> HighlightSeverity.ERROR;
          case Warning -> HighlightSeverity.WARNING;
          case Information -> HighlightSeverity.WEAK_WARNING;
          case Hint -> HighlightSeverity.INFORMATION;
        };
        assertEquals(diagnostic.getCode().getLeft() + " " + String.valueOf(diagnostic.getMessage()), expected, support.getHighlightSeverity(diagnostic));
      }
    }
  }

  /**
   * What the user sees in the modules that {@code erlc} compiles: nothing red, in either eqWAlizer setting. ELP reports eqWAlizer's
   * type errors as errors, one of them a false positive in {@code shop.erl}, and an EDoc style warning.
   */
  public void testNoErrorInModulesThatCompile() {
    for (boolean showEqwalizer : new boolean[]{false, true}) {
      ElpDiagnosticsSupport support = new ElpDiagnosticsSupport(showEqwalizer);
      for (String file : COMPILING_MODULES) {
        for (Diagnostic diagnostic : myDiagnostics.get(file)) {
          HighlightSeverity severity = support.getHighlightSeverity(diagnostic);
          assertTrue(file + ": " + String.valueOf(diagnostic.getMessage()), severity == null || severity.compareTo(HighlightSeverity.ERROR) < 0);
        }
      }
    }
  }

  @NotNull
  private List<Diagnostic> eqwalizer() {
    List<Diagnostic> result = new ArrayList<>();
    for (List<Diagnostic> diagnostics : myDiagnostics.values()) {
      for (Diagnostic diagnostic : diagnostics) {
        if (ElpDiagnosticsSupport.isEqwalizer(diagnostic)) result.add(diagnostic);
      }
    }
    return result;
  }

  @NotNull
  private List<Diagnostic> others() {
    List<Diagnostic> result = new ArrayList<>();
    for (List<Diagnostic> diagnostics : myDiagnostics.values()) {
      for (Diagnostic diagnostic : diagnostics) {
        if (!ElpDiagnosticsSupport.isEqwalizer(diagnostic)) result.add(diagnostic);
      }
    }
    return result;
  }

  @NotNull
  private Set<String> filesWith(@NotNull List<Diagnostic> diagnostics) {
    Set<String> files = new TreeSet<>();
    myDiagnostics.forEach((file, published) -> {
      for (Diagnostic diagnostic : published) {
        if (diagnostics.contains(diagnostic)) files.add(file);
      }
    });
    return files;
  }
}
