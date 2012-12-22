/*
 * Copyright 2012 Sergey Ignatov
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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMaxExpression;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
@SuppressWarnings("ConstantConditions")
public class ErlangElementFactory {
  private ErlangElementFactory() {
  }

  @NotNull
  public static PsiElement createQAtomFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-" + text + ".");
    return fileFromText.getAttributes().get(0).getAtomAttribute().getQAtom().getAtom();
  }

  @NotNull
  public static PsiElement createQVarFromText(Project project, String text) {
    return ((ErlangMaxExpression) createExpressionFromText(project, text)).getQVar();
  }

  @NotNull
  public static ErlangExpression createExpressionFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "f(" + text + ") -> " + text + ".");
    return fileFromText.getFunctions().get(0).getFunctionClauseList().get(0).getClauseBody().getExpressionList().get(0);
  }

  @NotNull
  public static PsiElement createMacrosFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-define(" + text + ", 1).");
    return fileFromText.getMacroses().get(0).getMacrosName();
  }

  @NotNull
  public static PsiElement createStringFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-include(\"" + text + "\").");
    return fileFromText.getIncludes().get(0).getIncludeString().getString();
  }

  @NotNull
  public static PsiElement createExportFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-export([" + text + "]).");
    return fileFromText.getAttributes().get(0);
  }

  @NotNull
  public static PsiElement createExportTypeFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-export_type([" + text + "]).");
    return fileFromText.getAttributes().get(0);
  }

  @NotNull
  public static PsiElement createRecordFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-record(" + text + ", {}).");
    return fileFromText.getRecords().get(0);
  }

  @NotNull
  public static PsiElement createRecordFieldsFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-record(text{" + text + "}).");
    return fileFromText.getRecords().get(0).getTypedRecordFields();
  }

  @NotNull
  public static PsiElement createLeafFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, text);
    return fileFromText.getFirstChild();
  }
}
