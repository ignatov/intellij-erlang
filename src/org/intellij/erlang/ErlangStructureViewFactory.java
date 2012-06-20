package org.intellij.erlang;

import com.intellij.ide.structureView.*;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.lang.PsiStructureViewFactory;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.pom.Navigatable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;
import com.intellij.util.PlatformIcons;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangArgumentDefinition;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.intellij.erlang.psi.impl.ErlangFileImpl;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangStructureViewFactory implements PsiStructureViewFactory {
  @Override
  public StructureViewBuilder getStructureViewBuilder(final PsiFile psiFile) {
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
    public Model(PsiFile psiFile) {
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

  public static class Element implements StructureViewTreeElement, ItemPresentation {

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
      } else if (myElement instanceof ErlangFile) {
        for (ErlangFunction o : ((ErlangFile) myElement).getFunctions()) {
          result.add(new Element(o));
        }
      }

      return result.toArray(new TreeElement[result.size()]);
    }

    @Override
    public String getPresentableText() {
      if (myElement instanceof ErlangFunctionClause) {
        final List<ErlangArgumentDefinition> exprs = ((ErlangFunctionClause) myElement).getArgumentDefinitionList();
        String res = ((ErlangFunctionClause) myElement).getQAtom().getText();
        final List<String> strings = ContainerUtil.map(exprs, new Function<ErlangArgumentDefinition, String>() {
          @Override
          public String fun(ErlangArgumentDefinition o) {
            return o.getText();
          }
        });

        return res + "(" + StringUtil.join(strings, ", ") + ")";
      }
      if (myElement instanceof ErlangFunction) {
        final int argCount = ((ErlangFunction) myElement).getFunctionClauseList().get(0).getArgumentDefinitionList().size();
        return ((ErlangFunction) myElement).getAtomName().getText() + "/" + argCount;
      } else if (myElement instanceof ErlangFileImpl) {
        return ((ErlangFileImpl) myElement).getName();
      }
      throw new AssertionError(myElement.getClass().getName());
    }


    @Override
    public String getLocationString() {
      return null;
    }

    @Override
    public Icon getIcon(boolean open) {
      if (myElement instanceof ErlangFunction) {
        return PlatformIcons.FUNCTION_ICON;
      } else if (myElement instanceof ErlangFunctionClause) {
        return PlatformIcons.PACKAGE_LOCAL_ICON;
      }
      return myElement.getIcon(0);
    }
  }
}
