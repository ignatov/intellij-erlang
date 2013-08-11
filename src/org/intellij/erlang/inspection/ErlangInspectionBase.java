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

package org.intellij.erlang.inspection;

import com.intellij.codeInsight.daemon.impl.actions.AbstractSuppressByNoInspectionCommentFix;
import com.intellij.codeInspection.*;
import com.intellij.lang.Commenter;
import com.intellij.lang.LanguageCommenters;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author ignatov
 */
abstract public class ErlangInspectionBase extends LocalInspectionTool implements CustomSuppressableInspectionTool {
  private static final Pattern SUPPRESS_PATTERN = Pattern.compile(SuppressionUtil.COMMON_SUPPRESS_REGEXP);

  @Override
  public ProblemDescriptor[] checkFile(@NotNull PsiFile file, @NotNull InspectionManager manager, boolean isOnTheFly) {
    ProblemsHolder problemsHolder = new ProblemsHolder(manager, file, isOnTheFly);
    try {
      checkFile(file, problemsHolder);
    } catch (PsiInvalidElementAccessException ignored) {
    }
    return problemsHolder.getResultsArray();
  }

  protected abstract void checkFile(PsiFile file, ProblemsHolder problemsHolder);

  @Nullable
  @Override
  public SuppressIntentionAction[] getSuppressActions(@Nullable PsiElement element) {
    return new SuppressIntentionAction[]{
      new ErlangSuppressInspectionFix(getSuppressId(), "Suppress for function", ErlangFunction.class),
      new ErlangSuppressInspectionFix(getSuppressId(), "Suppress for attribute", ErlangAttribute.class)
    };
  }

  @Override
  public boolean isSuppressedFor(@NotNull PsiElement element) {
    return isSuppressedForParent(element, ErlangFunction.class) || isSuppressedForParent(element, ErlangAttribute.class);
  }

  private boolean isSuppressedForParent(PsiElement element, final Class<? extends ErlangCompositeElement> parentClass) {
    PsiElement parent = PsiTreeUtil.getParentOfType(element, parentClass, false);
    if (parent == null) {
      return false;
    }
    return isSuppressedForElement(parent);
  }
  
  private boolean isSuppressedForElement(@NotNull PsiElement element) {
    Commenter commenter = LanguageCommenters.INSTANCE.forLanguage(ErlangLanguage.INSTANCE);
    String prefix = ObjectUtils.notNull(commenter == null ? null : commenter.getLineCommentPrefix(), "");
    
    PsiElement prevSibling = element.getPrevSibling();
    if (prevSibling == null) {
      final PsiElement parent = element.getParent();
      if (parent != null) {
        prevSibling = parent.getPrevSibling();
      }
    }
    while (prevSibling instanceof PsiComment || prevSibling instanceof PsiWhiteSpace) {
      if (prevSibling instanceof PsiComment) {
        int prefixLength = prefix.length();
        String text = prevSibling.getText();
        if (text.length() >= prefixLength && isSuppressedInComment(text.substring(prefixLength).trim())) {
          return true;
        }
      }
      prevSibling = prevSibling.getPrevSibling();
    }
    return false;
  }

  private boolean isSuppressedInComment(String commentText) {
    Matcher m = SUPPRESS_PATTERN.matcher(commentText);
    return m.matches() && SuppressionUtil.isInspectionToolIdMentioned(m.group(1), getSuppressId());
  }
  
  private String getSuppressId() {
    return getShortName().replace("Inspection", "");
  }

  public static class ErlangSuppressInspectionFix extends AbstractSuppressByNoInspectionCommentFix {
    private final Class<? extends ErlangCompositeElement> myContainerClass;
  
    public ErlangSuppressInspectionFix(final String ID, final String text, final Class<? extends ErlangCompositeElement> containerClass) {
      super(ID, false);
      setText(text);
      myContainerClass = containerClass;
    }
  
    @Override
    protected PsiElement getContainer(PsiElement context) {
      return PsiTreeUtil.getParentOfType(context, myContainerClass);
    }
    
    @Override
    protected void createSuppression(Project project, Editor editor, PsiElement element, PsiElement container) throws IncorrectOperationException {
      final PsiParserFacade parserFacade = PsiParserFacade.SERVICE.getInstance(project);
      final String text = SuppressionUtil.SUPPRESS_INSPECTIONS_TAG_NAME + " " + myID;
      PsiComment comment = parserFacade.createLineOrBlockCommentFromText(element.getContainingFile().getLanguage(), text);
      PsiElement where = container.getParent().addBefore(comment, container);
      PsiElement spaceFromText = PsiParserFacade.SERVICE.getInstance(project).createWhiteSpaceFromText("\n");
      where.getParent().addAfter(spaceFromText, where);
    }
  }
}
