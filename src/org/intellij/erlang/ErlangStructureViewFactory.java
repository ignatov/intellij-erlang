/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang;

import com.intellij.ide.structureView.*;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.lang.PsiStructureViewFactory;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.pom.Navigatable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.ui.RowIcon;
import com.intellij.util.Function;
import com.intellij.util.PlatformIcons;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangStructureViewFactory implements PsiStructureViewFactory {
  @Override
  public StructureViewBuilder getStructureViewBuilder(@NotNull final PsiFile psiFile) {
    return new TreeBasedStructureViewBuilder() {
      @NotNull
      public StructureViewModel createStructureViewModel() {
        return new Model(psiFile);
      }

      @Override
      public boolean isRootNodeShown() {
        return false;
      }
    };
  }

  public static class Model extends StructureViewModelBase implements StructureViewModel.ElementInfoProvider {
    public Model(@NotNull PsiFile psiFile) {
      super(psiFile, new Element(psiFile));
      withSuitableClasses(ErlangFile.class, ErlangFunction.class, ErlangFunctionClause.class);
    }

    @Override
    public boolean isAlwaysShowsPlus(StructureViewTreeElement structureViewTreeElement) {
      return false;
    }

    @Override
    public boolean isAlwaysLeaf(StructureViewTreeElement structureViewTreeElement) {
      return false;
    }
  }

  public static class Element implements StructureViewTreeElement, ItemPresentation, NavigationItem {

    private final PsiElement myElement;

    public Element(PsiElement element) {
      this.myElement = element;
    }

    @Override
    public Object getValue() {
      return myElement;
    }

    @Override
    public void navigate(boolean requestFocus) {
      ((Navigatable) myElement).navigate(requestFocus);
    }

    @Override
    public boolean canNavigate() {
      return ((Navigatable) myElement).canNavigate();
    }

    @Override
    public boolean canNavigateToSource() {
      return ((Navigatable) myElement).canNavigateToSource();
    }

    @Nullable
    @Override
    public String getName() {
      return myElement instanceof ErlangNamedElement ? ((ErlangNamedElement) myElement).getName() : null;
    }

    @Override
    public ItemPresentation getPresentation() {
      return this;
    }

    @Override
    public TreeElement[] getChildren() {
      if (myElement instanceof ErlangFunctionClause) {
        return EMPTY_ARRAY;
      }
      final ArrayList<TreeElement> result = new ArrayList<TreeElement>();
      if (myElement instanceof ErlangFunction) {
        final List<ErlangFunctionClause> clauses = ((ErlangFunction) myElement).getFunctionClauseList();
        if (clauses.size() != 1) {
          for (ErlangFunctionClause o : clauses) {
            result.add(new Element(o));
          }
        }
      }
      else if (myElement instanceof ErlangFile) {
        Comparator<ErlangNamedElement> comparator = new Comparator<ErlangNamedElement>() {
          @Override
          public int compare(@NotNull ErlangNamedElement o1, @NotNull ErlangNamedElement o2) {
            String name = o1.getName();
            if (name == null) return -1;
            return name.compareToIgnoreCase(o2.getName());
          }
        };
        List<ErlangMacrosDefinition> macroses = ((ErlangFile) myElement).getMacroses();
        List<ErlangRecordDefinition> records = ((ErlangFile) myElement).getRecords();
        List<ErlangTypeDefinition> types = ((ErlangFile) myElement).getTypes();
        List<ErlangFunction> functions = ((ErlangFile) myElement).getFunctions();
        Collections.sort(macroses, comparator);
        Collections.sort(records, comparator);
        Collections.sort(functions, comparator);
        for (ErlangMacrosDefinition o : macroses) result.add(new Element(o));
        for (ErlangRecordDefinition o : records) result.add(new Element(o));
        for (ErlangTypeDefinition o : types) result.add(new Element(o));
        for (ErlangFunction o : functions) result.add(new Element(o));
      }

      return result.toArray(new TreeElement[result.size()]);
    }

    @Override
    public String getPresentableText() {
      if (myElement instanceof ErlangFunctionClause) {
        List<ErlangArgumentDefinition> exprs = ((ErlangFunctionClause) myElement).getArgumentDefinitionList().getArgumentDefinitionList();
        String name = ((ErlangFunctionClause) myElement).getQAtom().getText();
        List<String> expressionStrings = ContainerUtil.map(exprs, new Function<ErlangArgumentDefinition, String>() {
          @Override
          public String fun(@NotNull ErlangArgumentDefinition o) {
            return o.getText();
          }
        });

        ErlangClauseGuard guard = ((ErlangFunctionClause) myElement).getClauseGuard();
        String guardText = guard != null ? " " + guard.getText() : "";

        return name + "(" + StringUtil.join(expressionStrings, ", ") + ")" + guardText;
      }
      if (myElement instanceof ErlangFunction) {
        return ErlangPsiImplUtil.createFunctionPresentation((ErlangFunction) myElement);
      }
      else if (myElement instanceof ErlangFile) {
        return ((ErlangFile) myElement).getName();
      }
      else if (myElement instanceof ErlangRecordDefinition) {
        return ((ErlangRecordDefinition) myElement).getName();
      }
      else if (myElement instanceof ErlangMacrosDefinition) {
        return ((ErlangMacrosDefinition) myElement).getName();
      }
      else if (myElement instanceof ErlangTypeDefinition) {
        return ErlangPsiImplUtil.createTypePresentation((ErlangTypeDefinition) myElement);
      }
      throw new AssertionError(myElement.getClass().getName());
    }


    @Nullable
    @Override
    public String getLocationString() {
      return null;
    }

    @Override
    public Icon getIcon(boolean open) {
      if (myElement instanceof ErlangFunction) {
        PsiFile file = myElement.getContainingFile();
        boolean isPrivate = file instanceof ErlangFile && !((ErlangFile) file).getExportedFunctions().contains(myElement);
        return buildRowIcon(ErlangIcons.FUNCTION, isPrivate ? PlatformIcons.PRIVATE_ICON : PlatformIcons.PUBLIC_ICON);
      }
      else if (myElement instanceof ErlangFunctionClause) {
        return ErlangIcons.FUNCTION_CLAUSE;
      }
      else if (myElement instanceof ErlangAttribute) {
        return ErlangIcons.ATTRIBUTE;
      }
      else if (myElement instanceof ErlangRecordDefinition) {
        return ErlangIcons.RECORD;
      }
      else if (myElement instanceof ErlangMacrosDefinition) {
        return ErlangIcons.MACROS;
      }
      else if (myElement instanceof ErlangTypeDefinition) {
        return ErlangIcons.TYPE;
      }
      return myElement.getIcon(0);
    }
  }

  @NotNull
  private static Icon buildRowIcon(Icon first, Icon second) {
    RowIcon rowIcon = new RowIcon(2);
    rowIcon.setIcon(first, 0);
    rowIcon.setIcon(second, 1);
    return rowIcon;
  }
}
