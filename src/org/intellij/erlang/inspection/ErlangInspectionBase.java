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

package org.intellij.erlang.inspection;

import com.intellij.codeInsight.daemon.impl.actions.AbstractSuppressByNoInspectionCommentFix;
import com.intellij.codeInspection.*;
import com.intellij.lang.Commenter;
import com.intellij.lang.LanguageCommenters;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

abstract public class ErlangInspectionBase extends LocalInspectionTool implements CustomSuppressableInspectionTool {
  private static final PsiElementVisitor DUMMY_VISITOR = new PsiElementVisitor() { };
  private static final Pattern SUPPRESS_PATTERN = Pattern.compile(SuppressionUtil.COMMON_SUPPRESS_REGEXP);

  @NotNull
  @Override
  public final PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
    ErlangFile file = ObjectUtils.tryCast(session.getFile(), ErlangFile.class);
    return file != null && canRunOn(file) ? buildErlangVisitor(holder, session, isOnTheFly) : DUMMY_VISITOR;
  }

  @NotNull
  @Override
  public final PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
    throw new IllegalStateException();
  }

  @Nullable
  @Override
  public final ProblemDescriptor[] checkFile(@NotNull PsiFile file, @NotNull InspectionManager manager, boolean isOnTheFly) {
    throw new IllegalStateException();
  }

  protected boolean canRunOn(@NotNull ErlangFile file) {
    return true;
  }

  @NotNull
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder,
                                             @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFile(PsiFile file) {
        checkFile((ErlangFile)file, holder);
      }
    };
  }

  @NotNull
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder,
                                             @NotNull LocalInspectionToolSession session, boolean isOnTheFly) {
    return buildErlangVisitor(holder, session);
  }

  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
  }

  @Nullable
  @Override
  public SuppressIntentionAction[] getSuppressActions(@Nullable PsiElement element) {
    return new SuppressIntentionAction[]{
      new ErlangSuppressInspectionFix(getSuppressId(), "Suppress for function", ErlangFunction.class),
      new ErlangSuppressInspectionFix(getSuppressId(), "Suppress for attribute", ErlangAttribute.class),
      new ErlangSuppressInspectionFix(getSuppressId(), "Suppress for expression", ErlangExpression.class)
    };
  }

  @Override
  public boolean isSuppressedFor(@NotNull PsiElement element) {
    return isSuppressedForParent(element, ErlangFunction.class) ||
      isSuppressedForParent(element, ErlangAttribute.class) ||
      isSuppressedForExpression(element);
  }

  private boolean isSuppressedForExpression(@Nullable PsiElement element) {
    return isSuppressedForElement(getTopmostExpression(element));
  }

  @Nullable
  private static ErlangExpression getTopmostExpression(@Nullable PsiElement element) {
    ErlangExpression expression = PsiTreeUtil.getParentOfType(element, ErlangExpression.class);
    while (expression != null && !(expression.getParent() instanceof ErlangClauseBody)) {
      expression = PsiTreeUtil.getParentOfType(expression, ErlangExpression.class);
    }
    return expression;
  }

  private boolean isSuppressedForParent(PsiElement element, Class<? extends ErlangCompositeElement> parentClass) {
    PsiElement parent = PsiTreeUtil.getParentOfType(element, parentClass, false);
    if (parent == null) return false;
    return isSuppressedForElement(parent);
  }
  
  private boolean isSuppressedForElement(@Nullable PsiElement element) {
    if (element == null) return false;
    Commenter commenter = LanguageCommenters.INSTANCE.forLanguage(ErlangLanguage.INSTANCE);
    String prefix = ObjectUtils.notNull(commenter == null ? null : commenter.getLineCommentPrefix(), "");
    
    PsiElement prevSibling = element.getPrevSibling();
    if (prevSibling == null) {
      PsiElement parent = element.getParent();
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
  
  protected String getSuppressId() {
    return getShortName().replace("Inspection", "");
  }

  protected void registerProblem(@NotNull ProblemsHolder holder, @NotNull PsiElement target, @NotNull String text,
                                 LocalQuickFix... fixes) {
    registerProblem(holder, target, text, null, null, fixes);
  }

  protected void registerProblem(@NotNull ProblemsHolder holder,@NotNull PsiElement target, @NotNull String text,
                                 @Nullable TextRange range, @Nullable ProblemHighlightType severity, LocalQuickFix... fixes) {
    severity = ErlangPsiImplUtil.inMacroCallArguments(target) ? ProblemHighlightType.WEAK_WARNING :
      ObjectUtils.notNull(severity, ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
    holder.registerProblem(holder.getManager().createProblemDescriptor(target, range, text, severity, false, fixes));
  }

  public static class ErlangSuppressInspectionFix extends AbstractSuppressByNoInspectionCommentFix {
    private final Class<? extends ErlangCompositeElement> myContainerClass;
  
    public ErlangSuppressInspectionFix(String ID, String text, Class<? extends ErlangCompositeElement> containerClass) {
      super(ID, false);
      setText(text);
      myContainerClass = containerClass;
    }
  
    @Override
    @Nullable
    protected PsiElement getContainer(PsiElement context) {
      if (myContainerClass == ErlangExpression.class) return getTopmostExpression(context);
      return PsiTreeUtil.getParentOfType(context, myContainerClass);
    }

    @Override
    protected void createSuppression(@NotNull Project project, @NotNull PsiElement element, @NotNull PsiElement container) throws IncorrectOperationException {
      PsiParserFacade parserFacade = PsiParserFacade.SERVICE.getInstance(project);
      String text = SuppressionUtilCore.SUPPRESS_INSPECTIONS_TAG_NAME + " " + myID;
      PsiComment comment = parserFacade.createLineOrBlockCommentFromText(element.getContainingFile().getLanguage(), text);
      PsiElement where = container.getParent().addBefore(comment, container);
      PsiElement spaceFromText = PsiParserFacade.SERVICE.getInstance(project).createWhiteSpaceFromText("\n");
      where.getParent().addAfter(spaceFromText, where);
    }
  }
}
