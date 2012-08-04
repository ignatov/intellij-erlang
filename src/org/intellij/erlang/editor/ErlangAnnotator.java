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

package org.intellij.erlang.editor;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Query;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

/**
 * @author ignatov
 */
public class ErlangAnnotator implements Annotator, DumbAware {
  @Override
  public void annotate(@NotNull PsiElement psiElement, @NotNull final AnnotationHolder annotationHolder) {
    psiElement.accept(new ErlangVisitor() {
      @Override
      public void visitRecordExpression(@NotNull ErlangRecordExpression o) {
        ErlangQAtom atomName = o.getAtomName();
        if (atomName != null) {
          PsiElement prevSibling = atomName.getPrevSibling();
          if (prevSibling != null && "#".equals(prevSibling.getText())) {
            o.getReference(); // todo: rewrite
            markIfUnresolved(atomName, atomName, annotationHolder, "Unresolved record " + atomName.getText());
          }
        }
      }

      @Override
      public void visitAtomAttribute(@NotNull ErlangAtomAttribute o) {
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
      }

      @Override
      public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitSpecification(@NotNull ErlangSpecification o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitAttribute(@NotNull ErlangAttribute o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitExport(@NotNull ErlangExport o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitInclude(@NotNull ErlangInclude o) {
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "include");
        markAttributeNameAsKeyword(o, annotationHolder, "include_lib");
      }

      @Override
      public void visitModule(@NotNull ErlangModule o) {
        markFirstChildAsKeyword(o, annotationHolder);
        String ext = FileUtil.getExtension(o.getContainingFile().getName());
        String withoutExtension = FileUtil.getNameWithoutExtension(o.getContainingFile().getName());
        String moduleName = o.getName();
        ErlangCompositeElement atom = o.getQAtom();
        if (atom != null && !moduleName.equals(withoutExtension)) {
          annotationHolder.createErrorAnnotation(atom, "Module with name '" + moduleName + "' should be declared in a file named '" +
            moduleName + "." + ext + "'.");
        }
      }

      @Override
      public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "record");
      }

      @Override
      public void visitExportFunction(@NotNull ErlangExportFunction o) {
        markIfUnresolved(o, o, annotationHolder, "Unresolved function " + o.getText());
      }

      @Override
      public void visitFunction(@NotNull final ErlangFunction function) {
//        List<ErlangFunction> funs = PsiTreeUtil.getChildrenOfTypeAsList(function.getContainingFile(), ErlangFunction.class);
//
//        for (ErlangFunction fun : funs) {
//          if (!function.equals(fun) && function.getName().equals(fun.getName()) && function.getArity() == fun.getArity()) {
//            annotationHolder.createErrorAnnotation(fun.getNameIdentifier(), "Duplicate function " + fun.getName() + "/" + fun.getArity());
//            return;
//          }
//        }

        final Ref<Object> usage = new Ref<Object>();

        function.getContainingFile().accept(
          new ErlangRecursiveVisitor() {
            @Override
            public void visitFile(PsiFile file) {
              for (PsiElement psiElement : file.getChildren()) {
                if (psiElement instanceof ErlangCompositeElement) {
                  psiElement.accept(this);
                }
              }
            }

            @Override
            public void visitCompositeElement(@NotNull ErlangCompositeElement o) {
              if (!usage.isNull()) return;
              super.visitCompositeElement(o);
            }

            @Override
            public void visitExportFunction(@NotNull ErlangExportFunction o) {
              PsiReference reference = o.getReference();
              if (reference != null && function.equals(reference.resolve())) {
                usage.set(o);
              }
            }

            @Override
            public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
              PsiReference reference = o.getReference();
              if (reference != null && function.equals(reference.resolve())) {
                usage.set(o);
              }
            }
          });


        if (usage.get() == null) {
          Query<PsiReference> search = ReferencesSearch.search(function, new LocalSearchScope(function.getContainingFile()));
          if (search.findFirst() == null) {
            annotationHolder.createWarningAnnotation(function.getNameIdentifier(), "Unused function " + function.getName());
          }
        }
      }
      // todo: add export, import and other bundled attributes
    });
  }

  private static void markAttributeNameAsKeyword(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder, @NotNull String name) {
    PsiElement rec = o.getFirstChild();
    while (rec != null) {
      if (rec instanceof LeafPsiElement && name.equals(rec.getText())) break;
      rec = rec.getNextSibling();
    }
    if (rec != null) {
      setHighlighting(rec, annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
    }
  }

  private static void markVariableIfUnused(@NotNull final ErlangQVar var, @NotNull AnnotationHolder annotationHolder, @NotNull String text) {
    ErlangFunctionClause functionClause = PsiTreeUtil.getTopmostParentOfType(var, ErlangFunctionClause.class);
    if (functionClause == null) return;
    Query<PsiReference> search = ReferencesSearch.search(var, new LocalSearchScope(functionClause));
    if (search.findFirst() == null) {
      annotationHolder.createWarningAnnotation(var, text);
    }
  }

  private static void markIfUnresolved(@NotNull ErlangCompositeElement o, @NotNull ErlangCompositeElement errorElement, @NotNull AnnotationHolder annotationHolder, @NotNull String text) {
    PsiReference reference = o.getReference();
    if (reference != null && reference.resolve() == null) {
      annotationHolder.createErrorAnnotation(errorElement, text);
    }
  }

  private static void markFirstChildAsKeyword(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder) {
    final PsiElement firstChild = o.getFirstChild();
    if (firstChild != null) {
      setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
    }
  }

  private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder, final TextAttributesKey key) {
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(TextAttributes.ERASE_MARKER);
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
  }
}
