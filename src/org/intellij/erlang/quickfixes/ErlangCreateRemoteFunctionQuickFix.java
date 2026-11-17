/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.application.options.CodeStyle;
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.util.IntentionFamilyName;
import com.intellij.codeInspection.util.IntentionName;
import com.intellij.ide.util.PsiNavigationSupport;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttrVal;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangExport;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangListExpression;
import org.intellij.erlang.psi.ErlangMaxExpression;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix.FunctionTextProvider;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Creates a function called as {@code module:function(...)} at the end of the module, exports it and opens it.
 */
public class ErlangCreateRemoteFunctionQuickFix implements LocalQuickFix {
  private static final Pattern VARIABLE = Pattern.compile("[A-Z_][A-Za-z0-9_@]*");
  private static final Set<String> RESERVED_WORDS = Set.of(
    "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond", "div",
    "else", "end", "fun", "if", "let", "maybe", "not", "of", "or", "orelse", "receive", "rem", "try", "when", "xor");
  private final String myFixMessage;
  private final SmartPsiElementPointer<ErlangFile> myTargetFile;
  @SafeFieldForPreview
  private final FunctionTextProvider myFunctionText;

  private ErlangCreateRemoteFunctionQuickFix(@NotNull String fixMessage,
                                             @NotNull ErlangFile targetFile,
                                             @NotNull FunctionTextProvider functionText) {
    myFixMessage = fixMessage;
    myTargetFile = SmartPointerManager.getInstance(targetFile.getProject()).createSmartPsiElementPointer(targetFile);
    myFunctionText = functionText;
  }

  /**
   * Returns the fix, or null when the module is not a writable source of the project (for example a module of the SDK
   * or of a library).
   */
  @Nullable
  public static ErlangCreateRemoteFunctionQuickFix create(@NotNull String fixMessage,
                                                          @Nullable ErlangFile targetFile,
                                                          @NotNull FunctionTextProvider functionText) {
    VirtualFile virtualFile = targetFile != null ? targetFile.getVirtualFile() : null;
    if (virtualFile == null || targetFile.getFileType() != ErlangFileType.MODULE) return null;
    ProjectFileIndex index = ProjectFileIndex.getInstance(targetFile.getProject());
    if (!index.isInContent(virtualFile) || index.isInLibrary(virtualFile)) return null;
    return new ErlangCreateRemoteFunctionQuickFix(fixMessage, targetFile, functionText);
  }

  /**
   * Returns the function name as it must be written in source, quoted when needed, reserved words included.
   */
  @NotNull
  public static String toFunctionName(@NotNull String name) {
    String atomName = ObjectUtils.notNull(ErlangPsiImplUtil.toAtomName(name), name);
    return RESERVED_WORDS.contains(atomName) ? "'" + atomName + "'" : atomName;
  }

  @Override
  public @IntentionName @NotNull String getName() {
    return myFixMessage;
  }

  @Override
  public @IntentionFamilyName @NotNull String getFamilyName() {
    return "Create function in module";
  }

  @Override
  public @Nullable PsiElement getElementToMakeWritable(@NotNull PsiFile currentFile) {
    return myTargetFile.getElement();
  }

  @Override
  public @NotNull IntentionPreviewInfo generatePreview(@NotNull Project project,
                                                       @NotNull ProblemDescriptor previewDescriptor) {
    ErlangFile file = myTargetFile.getElement();
    if (file == null) return IntentionPreviewInfo.EMPTY;
    String indent = StringUtil.repeat(" ", CodeStyle.getIndentOptions(file).INDENT_SIZE);
    return new IntentionPreviewInfo.CustomDiff(ErlangFileType.MODULE, file.getName(), "",
                                               createFunctionText().trim().replace("\n", "\n" + indent));
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFile file = myTargetFile.getElement();
    PsiDocumentManager manager = PsiDocumentManager.getInstance(project);
    Document document = file != null ? manager.getDocument(file) : null;
    if (document == null) return;
    manager.doPostponedOperationsAndUnblockDocument(document);
    manager.commitDocument(document);
    String name = StringUtil.unquoteString(myFunctionText.getName(), '\'');
    if (file.getFunction(name, myFunctionText.getArguments().size()) != null) return;

    CharSequence text = document.getCharsSequence();
    int textOffset = text.length();
    while (textOffset > 0 && Character.isWhitespace(text.charAt(textOffset - 1))) textOffset--;
    String functionText = createFunctionText();
    document.insertString(textOffset, functionText);
    RangeMarker marker = document.createRangeMarker(textOffset, textOffset + functionText.length());
    manager.commitDocument(document);
    CodeStyleManager.getInstance(project).reformatText(file, marker.getStartOffset(), marker.getEndOffset());
    manager.doPostponedOperationsAndUnblockDocument(document);
    manager.commitDocument(document);

    PsiElement period = file.findElementAt(marker.getEndOffset() - 1);
    marker.dispose();
    ErlangFunction function = PsiTreeUtil.getParentOfType(period, ErlangFunction.class);
    if (function == null) return;
    export(project, file, function);
    manager.doPostponedOperationsAndUnblockDocument(document);

    ErlangClauseBody body = PsiTreeUtil.findChildOfType(function, ErlangClauseBody.class);
    PsiElement bodyStart = body != null ? PsiTreeUtil.skipWhitespacesAndCommentsForward(body.getFirstChild()) : null;
    VirtualFile virtualFile = file.getVirtualFile();
    if (bodyStart == null || virtualFile == null) return;
    int bodyOffset = bodyStart.getTextRange().getStartOffset();
    ApplicationManager.getApplication().invokeLater(
      () -> PsiNavigationSupport.getInstance().createNavigatable(project, virtualFile, bodyOffset).navigate(true),
      ModalityState.nonModal(), project.getDisposed());
  }

  /**
   * Adds the function to the first export attribute outside -ifdef, -ifndef and -if blocks. Without one, creates an
   * export attribute before the first such block or the first definition. Does nothing when the module has a
   * -compile(export_all) outside these blocks.
   */
  private static void export(@NotNull Project project, @NotNull ErlangFile file, @NotNull ErlangFunction function) {
    int depth = 0;
    ErlangExport export = null;
    PsiElement firstBlock = null;
    for (ErlangAttribute attribute : file.getAttributes()) {
      PsiElement keyword = PsiTreeUtil.nextVisibleLeaf(attribute.getFirstChild());
      String name = keyword != null ? keyword.getText() : null;
      if ("ifdef".equals(name) || "ifndef".equals(name) || "if".equals(name)) {
        if (firstBlock == null) firstBlock = attribute;
        depth++;
      }
      else if ("endif".equals(name)) depth = Math.max(0, depth - 1);
      else if (depth == 0 && isExportAll(attribute.getAtomAttribute())) return;
      else if (depth == 0 && export == null) export = attribute.getExport();
    }
    PsiElement anchor = ErlangQuickFixBase.getAnchorElement(file);
    if (firstBlock != null && (anchor == null || firstBlock.getTextOffset() < anchor.getTextOffset())) anchor = firstBlock;
    ErlangExportFunctionFix.exportFunction(project, function, export, anchor);
  }

  private static boolean isExportAll(@Nullable ErlangAtomAttribute attribute) {
    ErlangAttrVal value = attribute != null && "compile".equals(attribute.getName()) ? attribute.getAttrVal() : null;
    if (value == null) return false;
    for (ErlangExpression expression : value.getExpressionList()) {
      List<ErlangExpression> options = expression instanceof ErlangListExpression list ?
                                       list.getExpressionList() : List.of(expression);
      for (ErlangExpression option : options) {
        if (option instanceof ErlangMaxExpression max && "export_all".equals(ErlangPsiImplUtil.getAtomName(max))) {
          return true;
        }
      }
    }
    return false;
  }

  @NotNull
  private String createFunctionText() {
    return "\n\n" + myFunctionText.getName() + "(" + String.join(", ", createParameters()) + ") ->\n" +
           ErlangCreateFunctionQuickFix.FUNCTION_BODY_DEFAULT_TEXT;
  }

  /**
   * Keeps the argument names that are distinct variables and replaces the others (literals, atoms, empty names,
   * repeated names) with _ArgN, so the function has the arity of the call and matches any arguments.
   */
  @NotNull
  private List<String> createParameters() {
    List<String> arguments = myFunctionText.getArguments();
    Set<String> used = new HashSet<>();
    List<String> parameters = new ArrayList<>(arguments.size());
    for (int i = 0; i < arguments.size(); i++) {
      String name = arguments.get(i);
      if (!VARIABLE.matcher(name).matches() || "_".equals(name) || !used.add(name)) {
        String base = "_Arg" + i;
        name = base;
        for (int k = 1; !used.add(name); k++) name = base + "_" + k;
      }
      parameters.add(name);
    }
    return parameters;
  }
}
