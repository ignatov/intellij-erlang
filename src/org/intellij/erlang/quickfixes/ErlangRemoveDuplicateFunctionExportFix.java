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
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.inspection.ErlangDuplicateFunctionExportInspection;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.statistics.ErlangUsageCounters;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class ErlangRemoveDuplicateFunctionExportFix extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Remove duplicate export";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangUsageCounters.countQuickFix(project, getClass());
    ErlangExportFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangExportFunction.class, false);
    if (function == null) return;
    String signature = ErlangDuplicateFunctionExportInspection.getSignature(function);
    if (signature == null) return;
    PsiFile file = function.getContainingFile();
    if (!(file instanceof ErlangFile)) return;
    List<TextRange> ranges = new ArrayList<>();
    for (ErlangAttribute attribute : ((ErlangFile) file).getAttributes()) {
      ErlangExport export = attribute.getExport();
      ErlangExportFunctions exportFunctions = export != null ? export.getExportFunctions() : null;
      if (exportFunctions == null) continue;
      List<PsiElement> children = new ArrayList<>();
      for (PsiElement child = exportFunctions.getFirstChild(); child != null; child = child.getNextSibling()) {
        children.add(child);
      }
      List<PsiElement> removed = new ArrayList<>();
      for (ErlangExportFunction ef : exportFunctions.getExportFunctionList()) {
        if (ef != function && Objects.equals(ErlangDuplicateFunctionExportInspection.getSignature(ef), signature)) {
          removeEntry(children, children.indexOf(ef), removed);
        }
      }
      if (removed.isEmpty()) continue;
      if (!ContainerUtil.exists(children, c -> c instanceof ErlangExportFunction || c instanceof PsiComment)) {
        PsiElement next = attribute.getNextSibling();
        int end = next instanceof PsiWhiteSpace ? next.getTextRange().getEndOffset() : attribute.getTextRange().getEndOffset();
        ranges.add(new TextRange(attribute.getTextRange().getStartOffset(), end));
      }
      else {
        for (PsiElement e : removed) {
          ranges.add(e.getTextRange());
        }
      }
    }
    if (ranges.isEmpty()) return;
    PsiDocumentManager documentManager = PsiDocumentManager.getInstance(project);
    Document document = documentManager.getDocument(file);
    if (document == null) return;
    documentManager.doPostponedOperationsAndUnblockDocument(document);
    ranges.sort(Comparator.comparingInt(TextRange::getStartOffset).reversed());
    for (TextRange range : ranges) {
      document.deleteString(range.getStartOffset(), range.getEndOffset());
    }
    documentManager.commitDocument(document);
  }

  private static void removeEntry(@NotNull List<PsiElement> children, int index, @NotNull List<PsiElement> removed) {
    int next = skipWhiteSpacesAndComments(children, index, 1);
    if (isComma(children, next)) {
      int last = next + 1 < children.size() && children.get(next + 1) instanceof PsiWhiteSpace ? next + 1 : next;
      for (int i = last; i > index; i--) {
        if (i >= next || children.get(i) instanceof PsiWhiteSpace) {
          removed.add(children.remove(i));
        }
      }
    }
    else {
      int prev = skipWhiteSpacesAndComments(children, index, -1);
      if (isComma(children, prev)) {
        if (prev < index - 1 && children.get(index - 1) instanceof PsiWhiteSpace && !(children.get(index - 2) instanceof PsiComment)) {
          removed.add(children.remove(index - 1));
          index--;
        }
        removed.add(children.remove(prev));
        index--;
      }
    }
    removed.add(children.remove(index));
  }

  private static int skipWhiteSpacesAndComments(@NotNull List<PsiElement> children, int index, int step) {
    int i = index + step;
    while (i >= 0 && i < children.size() && (children.get(i) instanceof PsiWhiteSpace || children.get(i) instanceof PsiComment)) {
      i += step;
    }
    return i;
  }

  private static boolean isComma(@NotNull List<PsiElement> children, int index) {
    return index >= 0 && index < children.size() && children.get(index).getNode().getElementType() == ErlangTypes.ERL_COMMA;
  }
}
