
package org.intellij.erlang;

import com.intellij.find.findUsages.*;
import com.intellij.lang.findUsages.DescriptiveNameUtil;
import com.intellij.openapi.help.HelpManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.ui.SimpleColoredComponent;
import com.intellij.ui.SimpleTextAttributes;
import com.intellij.usageView.UsageViewUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class CommonFindUsagesDialog extends AbstractFindUsagesDialog {
  @NotNull protected final PsiElement myPsiElement;

  public CommonFindUsagesDialog(@NotNull PsiElement element,
                                @NotNull Project project,
                                @NotNull FindUsagesOptions findUsagesOptions,
                                boolean toShowInNewTab,
                                boolean mustOpenInNewTab,
                                boolean isSingleFile,
                                FindUsagesHandler handler) {
    super(project, findUsagesOptions, toShowInNewTab, mustOpenInNewTab, isSingleFile, isTextSearch(element, isSingleFile, handler),
          !isSingleFile);
    myPsiElement = element;
    init();
  }

  private static boolean isTextSearch(PsiElement element, boolean isSingleFile, FindUsagesHandler handler) {
    return FindUsagesUtil.isSearchForTextOccurrencesAvailable(element, isSingleFile, handler);
  }

  @Override
  protected boolean isInFileOnly() {
    return super.isInFileOnly() ||
           PsiSearchHelper.SERVICE.getInstance(myPsiElement.getProject()).getUseScope(myPsiElement) instanceof LocalSearchScope;
  }

  @Override
  protected JPanel createFindWhatPanel() {
    return null;
  }

  @Override
  public void configureLabelComponent(@NotNull SimpleColoredComponent coloredComponent) {
    coloredComponent.append(StringUtil.capitalize(UsageViewUtil.getType(myPsiElement)));
    coloredComponent.append(" ");
    coloredComponent.append(DescriptiveNameUtil.getDescriptiveName(myPsiElement), SimpleTextAttributes.REGULAR_BOLD_ATTRIBUTES);
  }

  @Override
  protected void doHelpAction() {
    HelpManager.getInstance().invokeHelp(FindUsagesManager.getHelpID(myPsiElement));
  }
}
