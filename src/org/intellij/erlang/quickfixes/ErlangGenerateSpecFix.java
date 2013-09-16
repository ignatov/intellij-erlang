package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.template.TemplateBuilder;
import com.intellij.codeInsight.template.TemplateBuilderFactory;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangTopType;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

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

  private static PsiElement createSpecTemplate(ErlangFunction function, ErlangFile file, Project project) {
    StringBuilder specText = new StringBuilder();
    specText.append(function.getName())
            .append('(').append(computeArgsTypeSpecs(function)).append(')')
            .append(" -> ")
              //TODO provide more specific return type
            .append("any()")
            .append('.');
    PsiElement spec = file.addBefore(ErlangElementFactory.createSpecFromText(project, specText.toString()), function);
    file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n\n"), function);
    return spec;
  }

  private static String computeArgsTypeSpecs(ErlangFunction function) {
    StringBuilder argTypes = new StringBuilder();
    int arity = function.getArity();
    for (int i = 0; i < arity; i++) {
      //TODO provide more specific type and/or argument names
      argTypes.append("any(), ");
    }
    if (arity != 0) {
      argTypes.setLength(argTypes.length() - 2);
    }
    return argTypes.toString();
  }

}
