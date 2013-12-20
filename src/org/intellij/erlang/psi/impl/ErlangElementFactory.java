/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("ConstantConditions")
public class ErlangElementFactory {
  private ErlangElementFactory() {
  }

  @NotNull
  public static PsiElement createQAtomFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "-" + text + ".");
    return fileFromText.getAttributes().get(0).getAtomAttribute().getQAtom().getAtom();
  }

  @NotNull
  public static PsiElement createQVarFromText(@NotNull Project project, @NotNull String text) {
    return ((ErlangMaxExpression) createExpressionFromText(project, text)).getQVar();
  }

  @NotNull
  public static ErlangExpression createExpressionFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "f(" + text + ") -> " + text + ".");
    return fileFromText.getFunctions().get(0).getFunctionClauseList().get(0).getClauseBody().getExpressionList().get(0);
  }

  @NotNull
  public static ErlangFunction createFunctionFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, text);
    return fileFromText.getFunctions().get(0);
  }

  @NotNull
  public static PsiElement createMacrosFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "-define(" + text + ", 1).");
    return fileFromText.getMacroses().get(0).getMacrosName();
  }

  @NotNull
  public static PsiElement createStringFromText(@NotNull Project project, @NotNull String text) {
    return createIncludeString(project, text).getString();
  }

  @NotNull
  public static ErlangIncludeString createIncludeString(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "-include(\"" + text + "\").");
    return fileFromText.getIncludes().get(0).getIncludeStringSafe();
  }

  @NotNull
  public static PsiElement createExportFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "-export([" + text + "]).");
    return fileFromText.getAttributes().get(0);
  }

  @NotNull
  public static PsiElement createExportTypeFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "-export_type([" + text + "]).");
    return fileFromText.getAttributes().get(0);
  }

  @NotNull
  public static PsiElement createRecordFromText(@NotNull Project project, @NotNull String text, @NotNull String ... fields) {
    String fieldsText = StringUtil.join(fields, ",");
    ErlangFile fileFromText = createFileFromText(project, "-record(" + text + ", {" + fieldsText + "}).");
    return fileFromText.getRecords().get(0);
  }

  @NotNull
  public static PsiElement createRecordFieldsFromText(@NotNull Project project, @NotNull String text) {
    ErlangFile fileFromText = createFileFromText(project, "-record(text{" + text + "}).");
    return fileFromText.getRecords().get(0).getTypedRecordFields();
  }

  @NotNull
  public static PsiElement createLeafFromText(@NotNull Project project, @NotNull String text) {
    return createFileFromText(project, text).getFirstChild();
  }

  @NotNull
  private static ErlangFile createFileFromText(@NotNull Project project, @NotNull String text) {
    return (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, text);
  }
}
