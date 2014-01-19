// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.psi.*;

public class ErlangTypeImpl extends ErlangCompositeElementImpl implements ErlangType {

  public ErlangTypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitType(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangModuleRef getModuleRef() {
    return findChildByClass(ErlangModuleRef.class);
  }

  @Override
  @Nullable
  public ErlangQVar getQVar() {
    return findChildByClass(ErlangQVar.class);
  }

  @Override
  @Nullable
  public ErlangRecordRef getRecordRef() {
    return findChildByClass(ErlangRecordRef.class);
  }

  @Override
  @Nullable
  public ErlangTopType getTopType() {
    return findChildByClass(ErlangTopType.class);
  }

  @Override
  @Nullable
  public ErlangTypeRef getTypeRef() {
    return findChildByClass(ErlangTypeRef.class);
  }

  @Override
  @Nullable
  public PsiElement getBracketLeft() {
    return findChildByType(ERL_BRACKET_LEFT);
  }

  @Override
  @Nullable
  public PsiElement getBracketRight() {
    return findChildByType(ERL_BRACKET_RIGHT);
  }

  @Override
  @Nullable
  public PsiElement getColon() {
    return findChildByType(ERL_COLON);
  }

  @Override
  @Nullable
  public PsiElement getColonColon() {
    return findChildByType(ERL_COLON_COLON);
  }

  @Override
  @Nullable
  public PsiElement getCurlyLeft() {
    return findChildByType(ERL_CURLY_LEFT);
  }

  @Override
  @Nullable
  public PsiElement getCurlyRight() {
    return findChildByType(ERL_CURLY_RIGHT);
  }

  @Override
  @Nullable
  public PsiElement getDotDot() {
    return findChildByType(ERL_DOT_DOT);
  }

  @Override
  @Nullable
  public PsiElement getDotDotDot() {
    return findChildByType(ERL_DOT_DOT_DOT);
  }

  @Override
  @Nullable
  public PsiElement getParLeft() {
    return findChildByType(ERL_PAR_LEFT);
  }

  @Override
  @Nullable
  public PsiElement getParRight() {
    return findChildByType(ERL_PAR_RIGHT);
  }

  @Override
  @Nullable
  public PsiElement getRadix() {
    return findChildByType(ERL_RADIX);
  }

  @Override
  @Nullable
  public PsiElement getFun() {
    return findChildByType(ERL_FUN);
  }

}
