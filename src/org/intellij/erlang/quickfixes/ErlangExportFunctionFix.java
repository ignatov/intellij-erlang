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
import com.intellij.openapi.editor.colors.EditorColors;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.markup.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupAdapter;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.LightweightWindowEvent;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.components.JBList;
import com.intellij.ui.popup.HintUpdateSupply;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ErlangExportFunctionFix extends LocalQuickFixAndIntentionActionOnPsiElement {
  private static final int MAX_EXPORT_STRING_LENGTH = 80;
  private final Set<RangeHighlighter> myExportHighlighters = new HashSet<>();

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

  private void processFunction(@NotNull final Project project,
                               @NotNull final ErlangFunction function,
                               @Nullable Editor editor) {
    if (!(function.getContainingFile() instanceof ErlangFile)) return;
    ErlangFile file = (ErlangFile) function.getContainingFile();
    List<ErlangExport> exports = getExportPsiElements(file);

    if (exports.isEmpty()) {
      createNewExport(project, file, function);
      return;
    }

    if (exports.size() == 1) {
      updateExport(project, function, exports.get(0));
      return;
    }

    if (editor == null || ApplicationManager.getApplication().isUnitTestMode()) {
      ErlangExport first = ContainerUtil.getFirstItem(exports);
      assert first != null;
      updateExport(project, function, first);
      return;
    }

    List<ErlangExport> notEmptyExports = getNotEmptyExports(exports);
    if (notEmptyExports.size() == 1) {
      updateExport(project, function, notEmptyExports.get(0));
      return;
    }

    final JBList exportPopupList = createExportJBList(editor, notEmptyExports);
    new HintUpdateSupply(exportPopupList) {
      @Override
      protected PsiElement getPsiElementForHint(Object selectedValue) {
        return (PsiElement) selectedValue;
      }
    };
    JBPopupFactory.getInstance().createListPopupBuilder(exportPopupList)
      .setTitle("Choose export")
      .setMovable(false)
      .setResizable(false)
      .addListener(new JBPopupAdapter() {
        @Override
        public void onClosed(LightweightWindowEvent event) {
          dropHighlighters();
        }
      })
      .setItemChoosenCallback(() -> CommandProcessor.getInstance().executeCommand(project, () -> ApplicationManager.getApplication().runWriteAction(() -> {
        PsiDocumentManager.getInstance(project).commitAllDocuments();
        updateExport(project, function, (ErlangExport) exportPopupList.getSelectedValue());
      }), "Export function", null))
      .setRequestFocus(true)
      .createPopup().showInBestPositionFor(editor);
  }

  private void dropHighlighters() {
    for (RangeHighlighter highlight : myExportHighlighters) {
      highlight.dispose();
    }
    myExportHighlighters.clear();
  }

  private static void updateExport(@NotNull Project project,
                                   @NotNull ErlangFunction function,
                                   @Nullable ErlangExport oldExport) {
    if (oldExport == null) return;
    ErlangExportFunctions exportFunctions = oldExport.getExportFunctions();
    if (exportFunctions == null) return;
    String replace = exportFunctions.getText().replace("[", "").replace("]", "");
    String s = replace + (!StringUtil.isEmptyOrSpaces(replace) ? ", " : "") +
      ErlangPsiImplUtil.createFunctionPresentation(function);
    ErlangAttribute attribute = PsiTreeUtil.getParentOfType(oldExport, ErlangAttribute.class);
    if (attribute == null) return;
    attribute.replace(ErlangElementFactory.createExportFromText(project, s));
  }

  private static void createNewExport(@NotNull Project project,
                                      @NotNull ErlangFile file,
                                      @NotNull ErlangFunction function) {
    ErlangCompositeElement elementBefore = ErlangQuickFixBase.getAnchorElement(file);

    if (elementBefore != null) {
      file.addBefore(ErlangElementFactory.createExportFromText(
          project,
          function.getName() + "/" + function.getArity()),
        elementBefore);
      file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n\n"), elementBefore);
    }
  }

  @NotNull
  public static List<ErlangExport> getExportPsiElements(@NotNull ErlangFile file) {
    List<ErlangExport> exports = new SmartList<>(); // todo: mode to erlang file
    for (ErlangAttribute attribute : file.getAttributes()) {
      if (attribute.getExport() != null) {
        exports.add(attribute.getExport());
      }
    }
    return exports;
  }

  @NotNull
  public static List<ErlangExport> getNotEmptyExports(@NotNull List<ErlangExport> exports) {
    return ContainerUtil.filter(exports, export -> {
      ErlangExportFunctions functions = export.getExportFunctions();
      return functions != null && !functions.getExportFunctionList().isEmpty();
    });
  }

  @NotNull
  private JBList createExportJBList(@NotNull final Editor editor, @NotNull List<ErlangExport> exportList) {
    DefaultListModel<ErlangExport> model = new DefaultListModel<>();
    for (ErlangExport export : exportList) {
      model.addElement(export);
    }
    JBList<ErlangExport> exportPopupList = new JBList<>(model);
    exportPopupList.setCellRenderer(new DefaultListCellRenderer() {
      @NotNull
      @Override
      public Component getListCellRendererComponent(@NotNull JList list,
                                                    Object value,
                                                    int index,
                                                    boolean isSelected,
                                                    boolean cellHasFocus) {
        Component rendererComponent = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        ErlangExport export = (ErlangExport) value;
        if (export != null && export.getExportFunctions() != null) {
          setText(getPrettyPrefix(export.getExportFunctions().getText().replace("[", "").replace("]", "")));
        }
        return rendererComponent;
      }

      @NotNull
      private String getPrettyPrefix(@NotNull String s) {
        if (s.length() > MAX_EXPORT_STRING_LENGTH) {
          return s.substring(0, MAX_EXPORT_STRING_LENGTH - 2) + "...";
        }
        return s;
      }
    });
    exportPopupList.addListSelectionListener(e -> {
      ErlangExport export = exportPopupList.getSelectedValue();
      if (export == null) return;
      dropHighlighters();
      MarkupModel markupModel = editor.getMarkupModel();
      TextAttributes attributes = EditorColorsManager.getInstance().getGlobalScheme().getAttributes(EditorColors.IDENTIFIER_UNDER_CARET_ATTRIBUTES);
      ErlangExport selectedExport = exportPopupList.getSelectedValue();
      myExportHighlighters.add(
        markupModel.addRangeHighlighter(
          selectedExport.getTextRange().getStartOffset(),
          selectedExport.getTextRange().getEndOffset(),
          HighlighterLayer.SELECTION - 1,
          attributes,
          HighlighterTargetArea.EXACT_RANGE));
    });
    return exportPopupList;
  }
}
