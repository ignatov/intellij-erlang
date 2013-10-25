// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiLanguageInjectionHost;

public class ErlangVisitor extends PsiElementVisitor {

  public void visitAdditiveExpression(@NotNull ErlangAdditiveExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitAfterClause(@NotNull ErlangAfterClause o) {
    visitCompositeElement(o);
  }

  public void visitAfterClauseBody(@NotNull ErlangAfterClauseBody o) {
    visitCompositeElement(o);
  }

  public void visitAndalsoExpression(@NotNull ErlangAndalsoExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitAnonymousCallExpression(@NotNull ErlangAnonymousCallExpression o) {
    visitExpression(o);
  }

  public void visitArgumentDefinition(@NotNull ErlangArgumentDefinition o) {
    visitCompositeElement(o);
  }

  public void visitArgumentDefinitionList(@NotNull ErlangArgumentDefinitionList o) {
    visitCompositeElement(o);
  }

  public void visitArgumentList(@NotNull ErlangArgumentList o) {
    visitCompositeElement(o);
  }

  public void visitAssignmentExpression(@NotNull ErlangAssignmentExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitAtomAttribute(@NotNull ErlangAtomAttribute o) {
    visitCompositeElement(o);
  }

  public void visitAttrVal(@NotNull ErlangAttrVal o) {
    visitCompositeElement(o);
  }

  public void visitAttribute(@NotNull ErlangAttribute o) {
    visitCompositeElement(o);
  }

  public void visitBeginEndBody(@NotNull ErlangBeginEndBody o) {
    visitCompositeElement(o);
  }

  public void visitBeginEndExpression(@NotNull ErlangBeginEndExpression o) {
    visitExpression(o);
  }

  public void visitBehaviour(@NotNull ErlangBehaviour o) {
    visitCompositeElement(o);
  }

  public void visitBinBaseType(@NotNull ErlangBinBaseType o) {
    visitType(o);
  }

  public void visitBinElement(@NotNull ErlangBinElement o) {
    visitCompositeElement(o);
  }

  public void visitBinUnitType(@NotNull ErlangBinUnitType o) {
    visitType(o);
  }

  public void visitBinaryExpression(@NotNull ErlangBinaryExpression o) {
    visitExpression(o);
  }

  public void visitBinaryType(@NotNull ErlangBinaryType o) {
    visitType(o);
  }

  public void visitBitType(@NotNull ErlangBitType o) {
    visitCompositeElement(o);
  }

  public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
    visitCompositeElement(o);
  }

  public void visitCaseExpression(@NotNull ErlangCaseExpression o) {
    visitExpression(o);
    // visitClauseOwner(o);
  }

  public void visitCatchExpression(@NotNull ErlangCatchExpression o) {
    visitExpression(o);
  }

  public void visitClauseBody(@NotNull ErlangClauseBody o) {
    visitCompositeElement(o);
  }

  public void visitClauseGuard(@NotNull ErlangClauseGuard o) {
    visitCompositeElement(o);
  }

  public void visitColonQualifiedExpression(@NotNull ErlangColonQualifiedExpression o) {
    visitExpression(o);
  }

  public void visitCompOpExpression(@NotNull ErlangCompOpExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitConfigCallExpression(@NotNull ErlangConfigCallExpression o) {
    visitExpression(o);
  }

  public void visitConfigExpression(@NotNull ErlangConfigExpression o) {
    visitExpression(o);
  }

  public void visitCrClause(@NotNull ErlangCrClause o) {
    visitCompositeElement(o);
  }

  public void visitExport(@NotNull ErlangExport o) {
    visitCompositeElement(o);
  }

  public void visitExportFunction(@NotNull ErlangExportFunction o) {
    visitCompositeElement(o);
  }

  public void visitExportFunctions(@NotNull ErlangExportFunctions o) {
    visitCompositeElement(o);
  }

  public void visitExportType(@NotNull ErlangExportType o) {
    visitCompositeElement(o);
  }

  public void visitExportTypeAttribute(@NotNull ErlangExportTypeAttribute o) {
    visitCompositeElement(o);
  }

  public void visitExportTypes(@NotNull ErlangExportTypes o) {
    visitCompositeElement(o);
  }

  public void visitExpression(@NotNull ErlangExpression o) {
    visitCompositeElement(o);
  }

  public void visitFakeBinaryExpression(@NotNull ErlangFakeBinaryExpression o) {
    visitExpression(o);
  }

  public void visitFieldType(@NotNull ErlangFieldType o) {
    visitType(o);
  }

  public void visitFunClause(@NotNull ErlangFunClause o) {
    visitCompositeElement(o);
  }

  public void visitFunClauses(@NotNull ErlangFunClauses o) {
    visitCompositeElement(o);
  }

  public void visitFunExpression(@NotNull ErlangFunExpression o) {
    visitExpression(o);
  }

  public void visitFunType(@NotNull ErlangFunType o) {
    visitType(o);
  }

  public void visitFunType100T(@NotNull ErlangFunType100T o) {
    visitType(o);
  }

  public void visitFunTypeArguments(@NotNull ErlangFunTypeArguments o) {
    visitCompositeElement(o);
  }

  public void visitFunTypeSigs(@NotNull ErlangFunTypeSigs o) {
    visitCompositeElement(o);
  }

  public void visitFunTypeSigsBraces(@NotNull ErlangFunTypeSigsBraces o) {
    visitCompositeElement(o);
  }

  public void visitFunction(@NotNull ErlangFunction o) {
    visitNamedElement(o);
  }

  public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
    visitExpression(o);
  }

  public void visitFunctionClause(@NotNull ErlangFunctionClause o) {
    visitCompositeElement(o);
  }

  public void visitFunctionWithArity(@NotNull ErlangFunctionWithArity o) {
    visitCompositeElement(o);
  }

  public void visitFunctionWithArityVariables(@NotNull ErlangFunctionWithArityVariables o) {
    visitCompositeElement(o);
  }

  public void visitGenericFunctionCallExpression(@NotNull ErlangGenericFunctionCallExpression o) {
    visitExpression(o);
  }

  public void visitGlobalFunctionCallExpression(@NotNull ErlangGlobalFunctionCallExpression o) {
    visitExpression(o);
  }

  public void visitGuard(@NotNull ErlangGuard o) {
    visitCompositeElement(o);
  }

  public void visitIfClause(@NotNull ErlangIfClause o) {
    visitCompositeElement(o);
  }

  public void visitIfExpression(@NotNull ErlangIfExpression o) {
    visitExpression(o);
  }

  public void visitImportDirective(@NotNull ErlangImportDirective o) {
    visitCompositeElement(o);
  }

  public void visitImportFunction(@NotNull ErlangImportFunction o) {
    visitCompositeElement(o);
  }

  public void visitImportFunctions(@NotNull ErlangImportFunctions o) {
    visitCompositeElement(o);
  }

  public void visitInclude(@NotNull ErlangInclude o) {
    visitCompositeElement(o);
  }

  public void visitIncludeLib(@NotNull ErlangIncludeLib o) {
    visitCompositeElement(o);
  }

  public void visitIncludeString(@NotNull ErlangIncludeString o) {
    visitCompositeElement(o);
  }

  public void visitIntType(@NotNull ErlangIntType o) {
    visitType(o);
  }

  public void visitLcExpression(@NotNull ErlangLcExpression o) {
    visitExpression(o);
  }

  public void visitLcExprs(@NotNull ErlangLcExprs o) {
    visitCompositeElement(o);
  }

  public void visitListComprehension(@NotNull ErlangListComprehension o) {
    visitExpression(o);
  }

  public void visitListExpression(@NotNull ErlangListExpression o) {
    visitExpression(o);
  }

  public void visitListOpExpression(@NotNull ErlangListOpExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitMacros(@NotNull ErlangMacros o) {
    visitCompositeElement(o);
  }

  public void visitMacrosArg(@NotNull ErlangMacrosArg o) {
    visitCompositeElement(o);
  }

  public void visitMacrosBody(@NotNull ErlangMacrosBody o) {
    visitCompositeElement(o);
  }

  public void visitMacrosCall(@NotNull ErlangMacrosCall o) {
    visitCompositeElement(o);
  }

  public void visitMacrosDefinition(@NotNull ErlangMacrosDefinition o) {
    visitNamedElement(o);
  }

  public void visitMacrosName(@NotNull ErlangMacrosName o) {
    visitCompositeElement(o);
  }

  public void visitMaxExpression(@NotNull ErlangMaxExpression o) {
    visitExpression(o);
  }

  public void visitModelFieldList(@NotNull ErlangModelFieldList o) {
    visitCompositeElement(o);
  }

  public void visitModule(@NotNull ErlangModule o) {
    visitNamedElement(o);
  }

  public void visitModuleRef(@NotNull ErlangModuleRef o) {
    visitCompositeElement(o);
  }

  public void visitMultiplicativeExpression(@NotNull ErlangMultiplicativeExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitOptBitTypeList(@NotNull ErlangOptBitTypeList o) {
    visitCompositeElement(o);
  }

  public void visitOrelseExpression(@NotNull ErlangOrelseExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitParenthesizedExpression(@NotNull ErlangParenthesizedExpression o) {
    visitExpression(o);
  }

  public void visitPrefixExpression(@NotNull ErlangPrefixExpression o) {
    visitExpression(o);
  }

  public void visitQAtom(@NotNull ErlangQAtom o) {
    visitCompositeElement(o);
  }

  public void visitQVar(@NotNull ErlangQVar o) {
    visitNamedElement(o);
  }

  public void visitQualifiedExpression(@NotNull ErlangQualifiedExpression o) {
    visitExpression(o);
  }

  public void visitReceiveExpression(@NotNull ErlangReceiveExpression o) {
    visitExpression(o);
    // visitClauseOwner(o);
  }

  public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
    visitNamedElement(o);
  }

  public void visitRecordExpression(@NotNull ErlangRecordExpression o) {
    visitExpression(o);
  }

  public void visitRecordField(@NotNull ErlangRecordField o) {
    visitCompositeElement(o);
  }

  public void visitRecordFields(@NotNull ErlangRecordFields o) {
    visitCompositeElement(o);
  }

  public void visitRecordLikeType(@NotNull ErlangRecordLikeType o) {
    visitType(o);
  }

  public void visitRecordRef(@NotNull ErlangRecordRef o) {
    visitCompositeElement(o);
  }

  public void visitRecordTuple(@NotNull ErlangRecordTuple o) {
    visitCompositeElement(o);
  }

  public void visitRule(@NotNull ErlangRule o) {
    visitCompositeElement(o);
  }

  public void visitRuleBody(@NotNull ErlangRuleBody o) {
    visitCompositeElement(o);
  }

  public void visitRuleClause(@NotNull ErlangRuleClause o) {
    visitCompositeElement(o);
  }

  public void visitSendExpression(@NotNull ErlangSendExpression o) {
    visitFakeBinaryExpression(o);
  }

  public void visitSpecFun(@NotNull ErlangSpecFun o) {
    visitCompositeElement(o);
  }

  public void visitSpecification(@NotNull ErlangSpecification o) {
    visitCompositeElement(o);
  }

  public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
    visitExpression(o);
    // visitPsiLanguageInjectionHost(o);
  }

  public void visitTopType(@NotNull ErlangTopType o) {
    visitCompositeElement(o);
  }

  public void visitTopType100T(@NotNull ErlangTopType100T o) {
    visitType(o);
  }

  public void visitTopTypeClause(@NotNull ErlangTopTypeClause o) {
    visitCompositeElement(o);
  }

  public void visitTryCatch(@NotNull ErlangTryCatch o) {
    visitCompositeElement(o);
  }

  public void visitTryClause(@NotNull ErlangTryClause o) {
    visitCompositeElement(o);
  }

  public void visitTryClauses(@NotNull ErlangTryClauses o) {
    visitCompositeElement(o);
  }

  public void visitTryExpression(@NotNull ErlangTryExpression o) {
    visitExpression(o);
    // visitClauseOwner(o);
  }

  public void visitTryExpressionsClause(@NotNull ErlangTryExpressionsClause o) {
    visitCompositeElement(o);
  }

  public void visitTupleExpression(@NotNull ErlangTupleExpression o) {
    visitExpression(o);
  }

  public void visitType(@NotNull ErlangType o) {
    visitCompositeElement(o);
  }

  public void visitTypeDefinition(@NotNull ErlangTypeDefinition o) {
    visitNamedElement(o);
  }

  public void visitTypeGuard(@NotNull ErlangTypeGuard o) {
    visitCompositeElement(o);
  }

  public void visitTypeRef(@NotNull ErlangTypeRef o) {
    visitCompositeElement(o);
  }

  public void visitTypeSig(@NotNull ErlangTypeSig o) {
    visitCompositeElement(o);
  }

  public void visitTypeSigGuard(@NotNull ErlangTypeSigGuard o) {
    visitCompositeElement(o);
  }

  public void visitTypedAttrVal(@NotNull ErlangTypedAttrVal o) {
    visitCompositeElement(o);
  }

  public void visitTypedExpr(@NotNull ErlangTypedExpr o) {
    visitNamedElement(o);
  }

  public void visitTypedRecordFields(@NotNull ErlangTypedRecordFields o) {
    visitCompositeElement(o);
  }

  public void visitNamedElement(@NotNull ErlangNamedElement o) {
    visitCompositeElement(o);
  }

  public void visitCompositeElement(@NotNull ErlangCompositeElement o) {
    visitElement(o);
  }

}
