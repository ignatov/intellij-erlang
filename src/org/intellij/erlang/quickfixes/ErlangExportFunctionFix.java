/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.codeInspection.LocalQuickFixAndIntentionActionOnPsiElement;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Consumer;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangExport;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class ErlangExportFunctionFix extends LocalQuickFixAndIntentionActionOnPsiElement {
  private static final int MAX_EXPORT_STRING_LENGTH = 80;

  public ErlangExportFunctionFix(ErlangFunction function) {
    super(function);
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return "Export function";
  }

  @NotNull
  @Override
  public String getText() {
    return getFamilyName();
  }

  @Override
  public void invoke(@NotNull Project project,
                     @NotNull PsiFile file,
                     @Nullable Editor editor,
                     @NotNull PsiElement startElement,
                     @Nullable PsiElement endElement) {
    if (startElement instanceof ErlangFunction) {
      processFunction(project, (ErlangFunction) startElement, editor);
    }
  }

  private static void processFunction(@NotNull final Project project,
                                      @NotNull final ErlangFunction function,
                                      @Nullable Editor editor) {
    if (!(function.getContainingFile() instanceof ErlangFile file)) return;

    var exports = getExportPsiElements(file);

    if (exports.isEmpty()) {
      createNewExport(project, file, function, null);
      return;
    }

    if (editor == null || ApplicationManager.getApplication().isUnitTestMode()) {
      var first = ContainerUtil.getFirstItem(exports);
      assert first != null;
      updateExport(project, function, first);
      return;
    }

    var notEmptyExports = getNotEmptyExports(exports);
    var exportsShow = new ArrayList<PsiElement>(exports);

    if (exports.size() == notEmptyExports.size()) {
      exportsShow.add(createExport(project, ""));
    }

    Consumer<PsiElement> onClick = erlangExport ->
      CommandProcessor.getInstance().executeCommand(
        project,
        () ->
          ApplicationManager.getApplication().runWriteAction(
            () -> {
              PsiDocumentManager.getInstance(project).commitAllDocuments();

              if (erlangExport instanceof ErlangExport erlangExportTyped) {
                updateExport(project, function, erlangExportTyped);
              }
              else {
                createNewExport(project, file, function, exports.isEmpty() ? null : exports.get(exports.size() - 1).getParent());
              }
            }), "Export Function", null);

    JBPopupFactory.getInstance().createPopupChooserBuilder(exportsShow)
                  .setTitle("Choose Export")
                  .setMovable(false)
                  .setResizable(false)
                  .setItemChosenCallback(onClick)
                  .setRequestFocus(true)
                  .setRenderer(getRenderer())
                  .createPopup()
                  .showInBestPositionFor(editor);
  }

  @NotNull
  private static DefaultListCellRenderer getRenderer() {
    return new DefaultListCellRenderer() {
      @NotNull
      @Override
      public Component getListCellRendererComponent(@NotNull JList list,
                                                    Object value,
                                                    int index,
                                                    boolean isSelected,
                                                    boolean cellHasFocus) {
        var rendererComponent = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

        if (value instanceof ErlangExport export) {
          if (export.getExportFunctions() != null) {
            var exportText = export.getExportFunctions().getText();
            // Replace consequent whitespaces and tabs with one space
            var trimmedText = exportText.trim().replaceAll(" +", " ");
            setText(getPrettyPrefix(trimmedText));
          }
        }
        else {
          setText("Add a new -export() attribute");
        }

        return rendererComponent;
      }

      @NotNull
      private static String getPrettyPrefix(@NotNull String s) {
        if (s.length() > MAX_EXPORT_STRING_LENGTH) {
          return s.substring(0, MAX_EXPORT_STRING_LENGTH - 2) + "...";
        }

        return s;
      }
    };
  }

  private static void updateExport(@NotNull Project project,
                                   @NotNull ErlangFunction function,
                                   @Nullable ErlangExport oldExport) {
    if (oldExport == null) return;

    var exportFunctions = oldExport.getExportFunctions();

    if (exportFunctions == null) return;

    var replace = exportFunctions.getText().replace("[", "").replace("]", "");
    var s = replace + (!StringUtil.isEmptyOrSpaces(replace) ? ", " : "") +
            ErlangPsiImplUtil.createFunctionPresentation(function);
    var attribute = PsiTreeUtil.getParentOfType(oldExport, ErlangAttribute.class);

    if (attribute == null) return;

    attribute.replace(ErlangElementFactory.createExportFromText(project, s));
  }

  private static void createNewExport(@NotNull Project project,
                                      @NotNull ErlangFile file,
                                      @NotNull ErlangFunction function,
                                      @Nullable PsiElement elementAfter) {
    if (elementAfter != null) {
      file.addAfter(createExport(project, function.getName() + "/" + function.getArity()),
                    elementAfter);
      file.addAfter(ErlangElementFactory.createLeafFromText(project, "\n"), elementAfter);
      return;
    }

    var elementBefore = ErlangQuickFixBase.getAnchorElement(file);

    if (elementBefore != null) {
      file.addBefore(createExport(project, function.getName() + "/" + function.getArity()),
                     elementBefore);
      file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n\n"), elementBefore);
    }
  }

  private static PsiElement createExport(Project project, String text) {
    return ErlangElementFactory.createExportFromText(project, text);
  }

  @NotNull
  public static List<ErlangExport> getExportPsiElements(@NotNull ErlangFile file) {
    var exports = new ArrayList<ErlangExport>(); // todo: move to erlang file

    for (ErlangAttribute attribute : file.getAttributes()) {
      if (attribute.getExport() != null) {
        exports.add(attribute.getExport());
      }
    }
    return exports;
  }

  @NotNull
  public static List<ErlangExport> getNotEmptyExports(@NotNull List<ErlangExport> exports) {
    return ContainerUtil.filter(
      exports,
      export -> {
        var functions = export.getExportFunctions();

        return functions != null && !functions.getExportFunctionList().isEmpty();
      });
  }
}
