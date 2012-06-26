package org.intellij.erlang.editor;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangAnnotator implements Annotator, DumbAware {
  @Override
  public void annotate(@NotNull PsiElement psiElement, @NotNull final AnnotationHolder annotationHolder) {
    psiElement.accept(new ErlangVisitor() {
      @Override
      public void visitQVar(@NotNull ErlangQVar o) {
        setHighlighting(o, annotationHolder, ErlangSyntaxHighlighter.VARIABLES);
      }

      @Override
      public void visitAtomAttribute(@NotNull ErlangAtomAttribute o) {
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
      }

      @Override
      public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
        firstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitSpecification(@NotNull ErlangSpecification o) {
        firstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitAttribute(@NotNull ErlangAttribute o) {
        firstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitExport(@NotNull ErlangExport o) {
        firstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
        firstChildAsKeyword(o, annotationHolder);
        PsiElement rec = o.getFirstChild();
        while (rec != null) {
          if (rec instanceof LeafPsiElement && "record".equals(rec.getText())) break;
          rec = rec.getNextSibling();
        }
        if (rec != null) {
          setHighlighting(rec, annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
        }
      }

      // todo: add export, import and other bundled attributes
    });
  }

  private static void firstChildAsKeyword(ErlangCompositeElement o, AnnotationHolder annotationHolder) {
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
