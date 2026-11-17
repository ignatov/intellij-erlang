package org.intellij.erlang.editor;

import com.intellij.codeInsight.editorActions.smartEnter.SmartEnterProcessor;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.codeInsight.template.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.jetbrains.annotations.NotNull;

public class ErlangTerminatorSmartEnterProcessor extends SmartEnterProcessor {
  @Override
  public boolean process(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile psiFile) {
    int offset = editor.getCaretModel().getOffset();
    
    // Check file boundaries
    if (offset <= 0 || offset > psiFile.getTextLength()) {
      return false;
    }

    PsiElement element = psiFile.findElementAt(offset);

    // If element is null or whitespace, check the element at position -1
    if (element == null || element instanceof PsiWhiteSpace) {
      element = psiFile.findElementAt(offset - 1);
    }

    // If still null after checking previous element, return false
    if (element == null) {
      return false;
    }

    ErlangClauseBody clauseBody = PsiTreeUtil.getParentOfType(element, ErlangClauseBody.class, false);

    if (clauseBody == null) return false;

    TemplateManager templateManager = TemplateManager.getInstance(project);
    Template template = createTerminatorTemplate(templateManager);

    moveCaretToEndOfLine(editor);
    templateManager.startTemplate(editor, template);

    return true;
  }

  private static void moveCaretToEndOfLine(Editor editor) {
    LogicalPosition logicalPosition = editor.getCaretModel().getLogicalPosition();
    int lineNumber = logicalPosition.line;
    int lineEndOffset = editor.getDocument().getLineEndOffset(lineNumber);
    editor.getCaretModel().moveToOffset(lineEndOffset);
  }

  private static Template createTerminatorTemplate(TemplateManager templateManager) {
    Template template = templateManager.createTemplate("", "");
    template.addVariable("terminator", new TerminatorExpressionNode(), true);
    return template;
  }

  private static class TerminatorExpressionNode extends Expression {
    @Override
    public Result calculateResult(ExpressionContext context) {
      return new TextResult(",");
    }

    @Override
    public Result calculateQuickResult(ExpressionContext context) {
      return calculateResult(context);
    }

    @Override
    public LookupElement[] calculateLookupItems(ExpressionContext context) {
      return new LookupElement[]{
        LookupElementBuilder.create(","),
        LookupElementBuilder.create(";"),
        LookupElementBuilder.create("."),
      };
    }
  }
}