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

import org.intellij.erlang.inspection.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

/**
 * The plugin inspections whose problems ELP's diagnostics report too, with the ELP codes of the same problem. In the
 * {@link ElpMode#ELP} position ELP owns these problem classes, so the mapped inspections stay silent in the editor while ELP serves
 * the file ({@code ErlangInspectionBase}); the plugin inspections without an ELP counterpart keep reporting. The codes were taken
 * from ELP 1.1.0's own diagnostics on planted defects ({@code elp lint --include-erlc-diagnostics}): {@code L} are erl_lint's,
 * {@code E} epp's, {@code P} the parser's and {@code W} ELP's own.
 * <p>
 * Not mapped, because ELP reported nothing for the same defect: undefined behaviour, undefined or non-exported callbacks,
 * duplicate and conflicting behaviours. Not mapped, because the problem differs: the plugin's wrong arity of a {@code -spec Name/Arity}
 * (ELP reports a syntax error, P1711). The release-gated syntax inspections have no counterpart either.
 */
public final class ElpDiagnosticCounterparts {
  private ElpDiagnosticCounterparts() {
  }

  /** A plugin inspection, the ELP codes that report the same problem, and why they are the same. */
  public record Counterpart(@NotNull List<String> elpCodes, @NotNull String reason) {
  }

  public static final Map<Class<? extends ErlangInspectionBase>, Counterpart> TABLE = Map.ofEntries(
    Map.entry(ErlangUnboundVariableInspection.class, new Counterpart(List.of("L1262"), "variable 'X' is unbound")),
    Map.entry(ErlangUnresolvedFunctionInspection.class,
              new Counterpart(List.of("L1227", "W0017"),
                              "function f/1 undefined (local call), Function 'm:f/1' is undefined (remote call)")),
    Map.entry(ErlangUnresolvedExportFunctionInspection.class, new Counterpart(List.of("L1227"), "an exported function f/1 undefined")),
    Map.entry(ErlangUnusedFunctionInspection.class, new Counterpart(List.of("L1230"), "function f/1 is unused")),
    Map.entry(ErlangUnusedVariableInspection.class, new Counterpart(List.of("L1268"), "variable 'X' is unused")),
    Map.entry(ErlangUnusedTypeInspection.class, new Counterpart(List.of("L1296"), "type t() is unused")),
    Map.entry(ErlangUnresolvedRecordInspection.class, new Counterpart(List.of("L1252"), "record r undefined")),
    Map.entry(ErlangUnresolvedRecordFieldInspection.class, new Counterpart(List.of("L1256"), "field f undefined in record r")),
    Map.entry(ErlangUnresolvedMacrosInspection.class,
              new Counterpart(List.of("E1507", "E1508", "W0057"), "undefined macro 'M' (epp, with and without arguments; ELP's own)")),
    Map.entry(ErlangUnresolvedIncludeInspection.class, new Counterpart(List.of("E1516", "W0058"), "can't find include file")),
    Map.entry(ErlangUnresolvedIncludeLibInspection.class, new Counterpart(List.of("E1516", "W0058"), "can't find include lib")),
    Map.entry(ErlangSpecificationForUndefinedFunctionInspection.class,
              new Counterpart(List.of("L1308"), "spec for undefined function f/1")),
    Map.entry(ErlangHeadMismatchInspection.class, new Counterpart(List.of("P1700"), "head mismatch 'g' vs 'f'")),
    Map.entry(ErlangDuplicateFunctionInspection.class, new Counterpart(List.of("L1228"), "function f/0 already defined")),
    Map.entry(ErlangDefiningImportedFunctionInspection.class, new Counterpart(List.of("L1229"), "defining imported function f/2")),
    Map.entry(ErlangAmbiguousCallOfAutoImportedFunctionInspection.class,
              new Counterpart(List.of("L1231"), "ambiguous call of overridden auto-imported BIF f/1")),
    Map.entry(ErlangImportDirectiveOverridesAutoImportedBifInspection.class,
              new Counterpart(List.of("L1234"), "import directive overrides auto-imported BIF f/1")),
    Map.entry(ErlangFunctionAlreadyImportedInspection.class, new Counterpart(List.of("L1209"), "function f/2 already imported from m")),
    Map.entry(ErlangDuplicateFunctionExportInspection.class, new Counterpart(List.of("L1225"), "function f/0 already exported")),
    Map.entry(ErlangIllegalPatternInspection.class, new Counterpart(List.of("L1245"), "illegal pattern")),
    Map.entry(ErlangIllegalGuardInspection.class, new Counterpart(List.of("L1250"), "illegal guard expression")),
    Map.entry(ErlangIoFormatInspection.class,
              new Counterpart(List.of("L1317"), "the format string requires an argument list with N arguments")),
    Map.entry(ErlangIncorrectModuleNameInspection.class, new Counterpart(List.of("W0001"), "module name does not match file name"))
  );

  public static boolean hasElpCounterpart(@NotNull Class<?> inspection) {
    return TABLE.containsKey(inspection);
  }
}
