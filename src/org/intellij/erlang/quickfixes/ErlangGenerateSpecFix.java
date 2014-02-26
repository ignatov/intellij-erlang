package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.template.TemplateBuilder;
import com.intellij.codeInsight.template.TemplateBuilderFactory;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangGenerateSpecFix extends ErlangQuickFixBase {
  public static final String NAME = "Generate spec";

  @NotNull
  @Override
  public String getFamilyName() {
    return NAME;
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class, false);
    if (function != null) {
      Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
      if (editor != null) {
        generateSpec(editor, function);
      }
    }
  }

  public static void generateSpec(@NotNull Editor editor, @NotNull ErlangFunction function) {
    PsiFile containingFile = function.getContainingFile();
    if (!(containingFile instanceof ErlangFile)) return;
    ErlangFile file = (ErlangFile) containingFile;
    Project project = function.getProject();
    PsiElement spec = createSpecTemplate(function, file, project);
    runSpecTemplateEditor(editor, spec);
  }

  private static void runSpecTemplateEditor(final Editor editor, final PsiElement spec) {
    if (ApplicationManager.getApplication().isUnitTestMode()) return;
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            TemplateBuilder templateBuilder = TemplateBuilderFactory.getInstance().createTemplateBuilder(spec);
            Collection<ErlangTopType> types = PsiTreeUtil.findChildrenOfType(spec, ErlangTopType.class);
            for (ErlangTopType type : types) {
              templateBuilder.replaceElement(type, type.getText());
            }
            templateBuilder.run(editor, false);
          }
        });
      }
    });
  }

  private static PsiElement createSpecTemplate(final ErlangFunction function, final ErlangFile file, final Project project) {
    final StringBuilder specText = new StringBuilder();
    specText.append(function.getName())
            .append('(').append(computeArgsTypeSpecs(function)).append(')')
            .append(" -> ")
            .append(getTypeString(computeReturnType(function)))
            .append('.');
    final Ref<PsiElement> specRef = new Ref<PsiElement>();
    CommandProcessor.getInstance().executeCommand(project, new Runnable() {
      @Override
      public void run() {
        PsiElement spec = file.addBefore(ErlangElementFactory.createSpecFromText(project, specText.toString()), function);
        specRef.set(spec);
        file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n\n"), function);
      }
    }, null, null);
    return specRef.get();
  }

  private static String computeArgsTypeSpecs(ErlangFunction function) {
    StringBuilder argTypes = new StringBuilder();
    int arity = function.getArity();
    for (int i = 0; i < arity; i++) {
      ErlangExpressionType argType = computeArgumentType(function, i);
      argTypes.append(getTypeString(argType)).append(", ");
    }
    if (arity != 0) {
      argTypes.setLength(argTypes.length() - 2);
    }
    return argTypes.toString();
  }

  private static String getTypeString(ErlangExpressionType t) {
    return t == ErlangExpressionType.UNKNOWN ? "any()" : t.getName().toLowerCase() + "()";
  }

  private static ErlangExpressionType computeReturnType(ErlangFunction function) {
    List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
    List<ErlangExpression> lastExpressions = ContainerUtil.newArrayListWithCapacity(clauses.size());
    for (ErlangFunctionClause clause : clauses) {
      ErlangClauseBody clauseBody = clause.getClauseBody();
      if (clauseBody != null) {
        ErlangExpression lastExpressionInClause = ContainerUtil.getLastItem(clauseBody.getExpressionList());
        ContainerUtil.addIfNotNull(lastExpressions, lastExpressionInClause);
      }
    }
    return computeCommonType(lastExpressions);
  }

  private static ErlangExpressionType computeArgumentType(ErlangFunction function, int argumentIdx) {
    List<ErlangExpression> argumentPatterns = getArgumentPatterns(function, argumentIdx);
    return computeCommonType(argumentPatterns);
  }

  private static List<ErlangExpression> getArgumentPatterns(ErlangFunction function, int argumentIdx) {
    List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
    List<ErlangExpression> argumentPatterns = ContainerUtil.newArrayListWithCapacity(clauses.size());
    for (ErlangFunctionClause clause : clauses) {
      ErlangArgumentDefinitionList argDefList = clause.getArgumentDefinitionList();
      List<ErlangArgumentDefinition> clauseArgs = argDefList.getArgumentDefinitionList();
      ErlangArgumentDefinition argDef = argumentIdx < clauseArgs.size() ? clauseArgs.get(argumentIdx) : null;
      ContainerUtil.addIfNotNull(argumentPatterns, argDef != null ? argDef.getExpression() : null);
    }
    return argumentPatterns;
  }

  private static ErlangExpressionType computeCommonType(List<ErlangExpression> expressions) {
    List<ErlangExpressionType> types = ContainerUtil.map(expressions, new Function<ErlangExpression, ErlangExpressionType>() {
      @Override
      public ErlangExpressionType fun(ErlangExpression e) {
        return ErlangExpressionType.create(e);
      }
    });
    //TODO compute common type
    return types.isEmpty() ? ErlangExpressionType.UNKNOWN : types.get(0);
  }

}
