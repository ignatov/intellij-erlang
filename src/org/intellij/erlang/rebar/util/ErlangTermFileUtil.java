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

package org.intellij.erlang.rebar.util;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.impl.source.tree.TreeUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Consumer;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.List;


//TODO use traversers - no need to build PSI here
public final class ErlangTermFileUtil {
  private ErlangTermFileUtil() {
  }

  @NotNull
  public static List<ErlangTupleExpression> findNamedTuples(@Nullable ErlangExpression listExpression) {
    return findNamedTuples(listExpression, null);
  }

  @NotNull
  private static List<ErlangTupleExpression> findNamedTuples(@Nullable ErlangExpression listExpression,
                                                             @Nullable String name) {
    ErlangListExpression propList = listExpression instanceof ErlangListExpression ? (ErlangListExpression) listExpression : null;
    return propList != null ? findNamedTuples(propList.getExpressionList(), name) : ContainerUtil.<ErlangTupleExpression>emptyList();
  }

  @NotNull
  private static List<ErlangTupleExpression> findNamedTuples(@NotNull List<ErlangExpression> configExpressions,
                                                             @Nullable final String name) {
    return ContainerUtil.mapNotNull(configExpressions, expression -> {
      String tupleName = getNameOfNamedTuple(expression);
      boolean isValidName = name == null && tupleName != null || name != null && name.equals(tupleName);
      return isValidName ? (ErlangTupleExpression) expression : null;
    });
  }

  @Nullable
  public static String getNameOfNamedTuple(@Nullable ErlangExpression expression) {
    ErlangTupleExpression tupleExpression = expression instanceof ErlangTupleExpression ? (ErlangTupleExpression) expression : null;
    List<ErlangExpression> expressions = tupleExpression != null ? tupleExpression.getExpressionList() : null;
    ErlangExpression configExpression = expressions != null && !expressions.isEmpty() ? expressions.get(0) : null;
    PsiElement nameQAtom = configExpression instanceof ErlangConfigExpression ? configExpression.getFirstChild() : null;
    ErlangAtom atom = nameQAtom instanceof ErlangQAtom ? ((ErlangQAtom) nameQAtom).getAtom() : null;
    return atom != null ? atom.getName() : null;
  }

  @Nullable
  public static ErlangFile createPsi(@NotNull VirtualFile file) {
    if (file.getFileType() != ErlangFileType.APP && file.getFileType() != ErlangFileType.TERMS) return null;
    try {
      String text = StringUtil.convertLineSeparators(VfsUtilCore.loadText(file));
      Project defaultProject = ProjectManager.getInstance().getDefaultProject();
      return (ErlangFile) PsiFileFactory.getInstance(defaultProject).createFileFromText(file.getName(), file.getFileType(), text);
    } catch (IOException e) {
      return null;
    }
  }

  public static void processConfigSection(@Nullable PsiElement configRoot, @NotNull String sectionName, @NotNull Consumer<ErlangExpression> sectionConsumer) {
    for (ErlangTupleExpression erlOptTuple : getConfigSections(configRoot, sectionName)) {
      List<ErlangExpression> expressions = erlOptTuple.getExpressionList();
      ErlangExpression optionsList = expressions.size() >= 2 ? expressions.get(1) : null;
      if (optionsList == null) continue;
      sectionConsumer.consume(optionsList);
    }
  }

  @NotNull
  public static List<ErlangTupleExpression> getConfigSections(@Nullable PsiElement termsFile, @NotNull String sectionName) {
    return findNamedTuples(PsiTreeUtil.getChildrenOfTypeAsList(termsFile, ErlangExpression.class), sectionName);
  }

  @NotNull
  public static ErlangExpression createForm(String formText) {
    Project defaultProject = ProjectManager.getInstance().getDefaultProject();
    PsiFile file = PsiFileFactory.getInstance(defaultProject).createFileFromText("a.config", ErlangFileType.TERMS, formText);
    ErlangExpression form = ContainerUtil.getFirstItem(PsiTreeUtil.getChildrenOfTypeAsList(file, ErlangExpression.class));
    return ObjectUtils.assertNotNull(form);
  }

  public static void addListExpressionItem(@NotNull ErlangListExpression list, @NotNull ErlangExpression what) {
    boolean shouldAddComma = list.getChildren().length != 0;
    PsiElement leftBracket = list.getBracketLeft();
    if (shouldAddComma) {
      PsiElement comma = ErlangElementFactory.createLeafFromText(ProjectManager.getInstance().getDefaultProject(), ",");
      list.addAfter(comma, leftBracket);
    }
    list.addAfter(what, leftBracket);
  }

  public static void deleteListExpressionItem(@NotNull PsiElement what) {
    ASTNode whatNode = what.getNode();
    ASTNode commaAfterElement = TreeUtil.findSibling(whatNode, ErlangTypes.ERL_COMMA);
    ASTNode commaBeforeElement = TreeUtil.findSiblingBackward(whatNode, ErlangTypes.ERL_COMMA);
    PsiElement firstElementToDelete = commaBeforeElement != null && shouldDeleteComma(commaBeforeElement, whatNode) ?
      commaBeforeElement.getPsi() : what;
    PsiElement lastElementToDelete = commaAfterElement != null && shouldDeleteComma(commaAfterElement, whatNode) ?
      commaAfterElement.getPsi() : what;
    what.getParent().deleteChildRange(firstElementToDelete, lastElementToDelete);
  }

  private static boolean shouldDeleteComma(@NotNull ASTNode comma, @NotNull ASTNode listElementNode) {
    ASTNode leftNode = comma.getStartOffset() < listElementNode.getStartOffset() ? comma : listElementNode;
    ASTNode rightNode = comma.getStartOffset() < listElementNode.getStartOffset() ? listElementNode : comma;
    return elementsBetweenAreWhitespaceOrComment(leftNode, rightNode);
  }

  private static boolean elementsBetweenAreWhitespaceOrComment(@NotNull ASTNode first, @NotNull ASTNode last) {
    for (ASTNode node = first.getTreeNext(); node != null && node.getStartOffset() < last.getStartOffset();
         node = node.getTreeNext()) {
      if (!ErlangPsiImplUtil.isWhitespaceOrComment(node)) {
        return false;
      }
    }
    return true;
  }
}
