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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.colors.EditorColors;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.markup.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.*;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.ui.components.JBList;
import com.intellij.ui.popup.HintUpdateSupply;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ErlangExportFunctionFix extends ErlangQuickFixBase {

  private static final int MAX_EXPORT_STRING_LENGTH = 80;
  private final Set<RangeHighlighter> myExportHighlighters = new HashSet<RangeHighlighter>();
  private boolean myIsOnTheFly = true;

  public ErlangExportFunctionFix(boolean isOnTheFly) {
    myIsOnTheFly = isOnTheFly;
  }

  @NotNull
  @Override
  public String getFamilyName() {
    if (myIsOnTheFly)
      return "Export function...";
    return "Export function";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class);

    if (function != null) {
      processFunction(project, function);
    }
  }

  public void processFunction(final Project project, final ErlangFunction function) {
    if (!(function.getContainingFile() instanceof ErlangFile)) return;
    Editor editor = PsiUtilBase.findEditor(function);
    if (editor == null) return;
    final ErlangFile file = (ErlangFile) function.getContainingFile();
    List<ErlangExport> exports = getExportPsiElements(file);
    if (exports.isEmpty()) {
      createNewExport(project, file, function);
      return;
    }
    List<ErlangExport> notEmptyExports = getNotEmptyExports(exports);
    if (exports.size() == 1 || notEmptyExports.isEmpty()) {
      updateExport(project, function, exports.get(0));
      return;
    }
    if (notEmptyExports.size() == 1){
      updateExport(project, function, notEmptyExports.get(0));
      return;
    }
    exports = notEmptyExports;
    if (!myIsOnTheFly) {
      updateExport(project, function, exports.get(0));
      return;
    }
    final JBList exportPopupList = createExportJBList(editor, exports);
    new HintUpdateSupply(exportPopupList) {
      @Override
      protected PsiElement getPsiElementForHint(Object selectedValue) {
        return (PsiElement)selectedValue;
      }
    };
    JBPopupFactory.getInstance().createListPopupBuilder(exportPopupList)
      .setTitle("Choose export statement")
      .setMovable(false)
      .setResizable(false)
      .addListener(new JBPopupAdapter() {
        @Override
        public void onClosed(LightweightWindowEvent event) {
          dropHighlighters();
        }
      })
      .setItemChoosenCallback(new Runnable() {
        @Override
        public void run() {
          CommandProcessor.getInstance().executeCommand(project, new Runnable() {
            @Override
            public void run() {
              ApplicationManager.getApplication().runWriteAction(new Runnable() {
                @Override
                public void run() {
                  PsiDocumentManager.getInstance(project).commitAllDocuments();
                  updateExport(project, function, (ErlangExport) exportPopupList.getSelectedValue());
                }
              });
            }
          }, "Export function", null);
        }
      })
      .setRequestFocus(true)
      .createPopup().showInBestPositionFor(editor);

  }

  private void dropHighlighters() {
    for (RangeHighlighter highlight : myExportHighlighters) {
      highlight.dispose();
    }
    myExportHighlighters.clear();
  }

  private static void updateExport(Project project, ErlangFunction function, ErlangExport oldExport) {
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

  private static void createNewExport(Project project, ErlangFile file, ErlangFunction function) {
    ErlangCompositeElement elementBefore = getAnchorElement(file);

    if (elementBefore != null) {
      file.addBefore(ErlangElementFactory.createExportFromText(
        project,
        function.getName() + "/" + function.getArity()),
        elementBefore);
      file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n\n"), elementBefore);
    }
  }

  public static List<ErlangExport> getExportPsiElements(ErlangFile file) {
    List<ErlangExport> exports = new ArrayList<ErlangExport>();
    for (ErlangAttribute attribute : file.getAttributes()) {
      if (attribute.getExport() != null) {
        exports.add(attribute.getExport());
      }
    }
    return exports;
  }

  public static List<ErlangExport> getNotEmptyExports(List<ErlangExport> exports) {
    List<ErlangExport> notEmptyExports = new ArrayList<ErlangExport>();
    for (ErlangExport export : exports) {
      if (export.getExportFunctions() != null && export.getExportFunctions().getChildren().length != 0) {
        notEmptyExports.add(export);
      }
    }
    return notEmptyExports;
  }

  private JBList createExportJBList(final Editor editor, final List<ErlangExport> exportList) {
    final DefaultListModel model = new DefaultListModel();
    for (ErlangExport export : exportList) {
      model.addElement(export);
    }
    final JBList exportPopupList = new JBList(model);
    exportPopupList.setCellRenderer(new DefaultListCellRenderer() {
      @Override
      public Component getListCellRendererComponent(final JList list,
                                                    final Object value,
                                                    final int index,
                                                    final boolean isSelected,
                                                    final boolean cellHasFocus) {
        final Component rendererComponent = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        final ErlangExport export = (ErlangExport) value;
        if (export != null && export.getExportFunctions() != null) {
          setText(getPrettyPrefix(export.getExportFunctions().getText().replace("[", "").replace("]", "")));
        }
        return rendererComponent;
      }

      private String getPrettyPrefix(String s) {
        if (s.length() > MAX_EXPORT_STRING_LENGTH)
          return s.substring(0, MAX_EXPORT_STRING_LENGTH - 2) + "...";
        return s;
      }
    });
    exportPopupList.addListSelectionListener(new ListSelectionListener() {
      @Override
      public void valueChanged(final ListSelectionEvent e) {
        final ErlangExport export = (ErlangExport)exportPopupList.getSelectedValue();
        if (export == null) return;
        dropHighlighters();
        final MarkupModel markupModel = editor.getMarkupModel();
        TextAttributes attributes = EditorColorsManager.getInstance().getGlobalScheme().getAttributes(
          EditorColors.IDENTIFIER_UNDER_CARET_ATTRIBUTES);
        ErlangExport selectedExport = (ErlangExport) exportPopupList.getSelectedValue();
        final RangeHighlighter rangeHighlighter = markupModel.addRangeHighlighter(
          selectedExport.getTextRange().getStartOffset(),
          selectedExport.getTextRange().getEndOffset(),
          HighlighterLayer.SELECTION - 1,
          attributes,
          HighlighterTargetArea.EXACT_RANGE);
        myExportHighlighters.add(rangeHighlighter);
      }
    });
    return exportPopupList;
  }
}
