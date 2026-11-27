/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.statistics.ErlangUsageCounters;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangIntroduceRecordFix extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Introduce new record";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangUsageCounters.countQuickFix(project, getClass());
    PsiElement element = descriptor.getPsiElement();

    List<String> fieldNames = element instanceof ErlangRecordRef ? getFieldNames((ErlangRecordRef) element) : ContainerUtil.emptyList();
    String name = element instanceof ErlangRecordRef ? getRecordName((ErlangRecordRef) element) : element.getText();
    PsiElement record = ErlangElementFactory.createRecordFromText(project, name, ArrayUtil.toStringArray(fieldNames));
    PsiFile file = element.getContainingFile();

    if (file instanceof ErlangFile) {
      ErlangCompositeElement elementBefore = getAnchorElement((ErlangFile) file);

      if (elementBefore != null) {
        file.addBefore(record, elementBefore);
        String newLines = elementBefore instanceof ErlangRecordDefinition ? "\n" : "\n\n";
        file.addBefore(ErlangElementFactory.createLeafFromText(project, newLines), elementBefore);
      }
    }
  }

  // OTP 29 takes a variable or a reserved word as a record name (#Foo{}, #when{}), -record( takes only an atom
  @NotNull
  private static String getRecordName(@NotNull ErlangRecordRef recordRef) {
    String text = recordRef.getText();
    ErlangAtom atom = recordRef.getQAtom().getAtom();
    if (atom == null || !text.equals(atom.getText()) || atom.getNode().findChildByType(ErlangTypes.ERL_SINGLE_QUOTE) != null) {
      return text;
    }
    ErlangLexer lexer = new ErlangLexer(recordRef.getProject());
    lexer.start(text);
    boolean plainAtom = lexer.getTokenType() == ErlangTypes.ERL_ATOM_NAME && lexer.getTokenEnd() == text.length();
    return plainAtom ? text : "'" + text + "'";
  }

  private static List<String> getFieldNames(@NotNull ErlangRecordRef recordRef) {
    ErlangRecordTuple tuple = PsiTreeUtil.getNextSiblingOfType(recordRef, ErlangRecordTuple.class);
    final List<String> fieldNames = new ArrayList<>();

    if (tuple != null) {
      tuple.accept(new ErlangRecursiveVisitor() {
        @Override
        public void visitRecordField(@NotNull ErlangRecordField o) {
          ErlangQAtom fieldNameAtom = o.getFieldNameAtom();
          String text = fieldNameAtom != null ? fieldNameAtom.getText() : null;
          ContainerUtil.addIfNotNull(fieldNames, text);
        }
      });
    }

    return fieldNames;
  }
}
