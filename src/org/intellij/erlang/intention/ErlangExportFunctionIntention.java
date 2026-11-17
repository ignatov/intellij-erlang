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

package org.intellij.erlang.intention;

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class ErlangExportFunctionIntention extends ErlangBaseNamedElementIntention {
  private static final String EXPORT_FUNCTION = "Export function";
  private String myText = EXPORT_FUNCTION;

  public ErlangExportFunctionIntention() {
    super(EXPORT_FUNCTION, EXPORT_FUNCTION);
  }

  @NotNull
  @Override
  public String getText() {
    return myText;
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!file.getManager().isInProject(file)) return false;
    if (!(file instanceof ErlangFile erlangFile)) return false;
    int offset = editor.getCaretModel().getOffset();
    ErlangFunction function = findFunction(file, offset);
    if (function != null) {
      myText = EXPORT_FUNCTION;
      return !erlangFile.getExportedFunctions().contains(function);
    }
    if (findExport(file, offset) != null) {
      List<String> functions = getUnexportedFunctions(erlangFile);
      if (functions.isEmpty()) return false;
      myText = functions.size() == 1 ? EXPORT_FUNCTION + " " + functions.get(0) : "Export functions...";
      return true;
    }
    return false;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    int offset = editor.getCaretModel().getOffset();
    ErlangFunction function = findFunction(file, offset);
    if (function != null) {
      new ErlangExportFunctionFix(function).invoke(project, file, editor, function, null);
      return;
    }
    ErlangExport export = findExport(file, offset);
    if (export != null && file instanceof ErlangFile erlangFile) {
      exportFunctions(project, editor, erlangFile, export, getUnexportedFunctions(erlangFile));
    }
  }

  private static void exportFunctions(@NotNull Project project,
                                      @NotNull Editor editor,
                                      @NotNull ErlangFile file,
                                      @NotNull ErlangExport export,
                                      @NotNull List<String> functions) {
    if (functions.isEmpty()) return;
    if (functions.size() == 1 || ApplicationManager.getApplication().isUnitTestMode()) {
      ErlangExportFunctionFix.addToExport(project, export, functions);
      return;
    }

    var exportPointer = SmartPointerManager.createPointer(export);
    JBPopupFactory.getInstance().createPopupChooserBuilder(functions)
                  .setTitle("Choose Functions to Export")
                  .setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
                  .setMovable(false)
                  .setResizable(false)
                  .setRequestFocus(true)
                  .setItemsChosenCallback(chosen -> {
                    List<String> toExport = ContainerUtil.filter(functions, chosen::contains);
                    WriteCommandAction.writeCommandAction(project, file)
                      .withName("Export Functions")
                      .run(() -> {
                        PsiDocumentManager.getInstance(project).commitAllDocuments();
                        ErlangExport erlangExport = exportPointer.getElement();
                        if (erlangExport != null) {
                          ErlangExportFunctionFix.addToExport(project, erlangExport, toExport);
                        }
                      });
                  })
                  .createPopup()
                  .showInBestPositionFor(editor);
  }

  @NotNull
  private static List<String> getUnexportedFunctions(@NotNull ErlangFile file) {
    if (file.isExportedAll()) return List.of();
    Set<String> exportedThroughMacros = getExportedThroughMacros(file);
    Set<String> result = new LinkedHashSet<>();
    for (ErlangFunction function : file.getFunctions()) {
      if (function.isExported() || function.getName().startsWith("?")) continue;
      if (exportedThroughMacros.contains(function.getName() + "/" + function.getArity())) continue;
      result.add(ErlangExportFunctionFix.createExportPresentation(function));
    }
    return List.copyOf(result);
  }

  @NotNull
  private static Set<String> getExportedThroughMacros(@NotNull ErlangFile file) {
    Set<String> result = new HashSet<>();
    for (ErlangExport export : ErlangExportFunctionFix.getExportPsiElements(file)) {
      ErlangExportFunctions exportFunctions = export.getExportFunctions();
      if (exportFunctions == null) continue;
      for (ErlangExportFunction exportFunction : exportFunctions.getExportFunctionList()) {
        ErlangMacros macros = exportFunction.getQAtom().getMacros();
        PsiReference reference = macros != null ? macros.getReference() : null;
        PsiElement definition = reference != null ? reference.resolve() : null;
        PsiElement arity = exportFunction.getInteger();
        if (!(definition instanceof ErlangMacrosDefinition macrosDefinition) || arity == null ||
            macrosDefinition.getArgumentDefinitionList() != null || macrosDefinition.getMacrosBody() == null) {
          continue;
        }
        List<ErlangExpression> body = macrosDefinition.getMacrosBody().getExpressionList();
        if (body.size() == 1 && body.get(0) instanceof ErlangMaxExpression max &&
            max.getQAtom() != null && max.getQAtom().getAtom() != null) {
          String name = StringUtil.unquoteString(ErlangPsiImplUtil.getName(max.getQAtom().getAtom()), '\'');
          result.add(name + "/" + arity.getText());
        }
      }
    }
    return result;
  }

  @Nullable
  private static ErlangExport findExport(@NotNull PsiFile file, int offset) {
    ErlangExport export = PsiTreeUtil.getParentOfType(file.findElementAt(offset), ErlangExport.class);
    return export != null && export.getExportFunctions() != null ? export : null;
  }

  @Override
  public @NotNull IntentionPreviewInfo generatePreview(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    return IntentionPreviewInfo.EMPTY;
  }

  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    return findElement(file, offset, ErlangFunction.class);
  }
}
