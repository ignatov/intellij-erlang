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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
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
    PsiElement element = descriptor.getPsiElement();

    List<String> fieldNames = element instanceof ErlangRecordRef ? getFieldNames((ErlangRecordRef) element) : ContainerUtil.<String>emptyList();
    PsiElement record = ErlangElementFactory.createRecordFromText(project, element.getText(), ArrayUtil.toStringArray(fieldNames));
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

  private static List<String> getFieldNames(@NotNull ErlangRecordRef recordRef) {
    ErlangRecordTuple tuple = PsiTreeUtil.getNextSiblingOfType(recordRef, ErlangRecordTuple.class);
    final List<String> fieldNames = new ArrayList<String>();

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
