package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.SmartList;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacros;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Round 6: a macro use {@code ?NAME} that the parser left as bare tokens (a use where the grammar expects no
 * expression, such as {@code foo?PAR}, a separator between clauses, a body consumed as raw tokens, or tokens skipped
 * by error recovery). The tree stays as it is; the nearest Erlang composite element (or the file) that holds the two
 * tokens, directly or through error elements and dummy blocks of the error recovery, hosts this reference. Its range
 * is the name token. The platform's reference lookup walks up from a leaf, so go to declaration, identifier
 * highlighting, find usages, rename and completion see it with no special path.
 */
public final class ErlangBareMacroReference extends PsiReferenceBase<PsiElement> implements PsiPolyVariantReference {
  private static final TokenSet SKIPPED = TokenSet.orSet(ErlangParserDefinition.COMMENTS, TokenSet.create(TokenType.WHITE_SPACE));
  private static final TokenSet NAMES = TokenSet.create(ErlangTypes.ERL_VAR, ErlangTypes.ERL_ATOM_NAME);

  private final @NotNull PsiElement myName;

  private ErlangBareMacroReference(@NotNull PsiElement host, @NotNull PsiElement name) {
    super(host, TextRange.from(name.getTextRange().getStartOffset() - host.getTextRange().getStartOffset(), name.getTextLength()));
    myName = name;
  }

  /** The host's own references followed by the references of the bare macro uses it holds. */
  public static PsiReference @NotNull [] append(@NotNull PsiElement host, PsiReference @NotNull [] own) {
    PsiReference[] bare = find(host);
    return bare.length == 0 ? own : ArrayUtil.mergeArrays(own, bare);
  }

  /**
   * The bare macro uses held by the host. Cheap when there are none: one pass over the host's direct children,
   * looking for a {@code ?} token, an error element or a dummy block.
   */
  public static PsiReference @NotNull [] find(@NotNull PsiElement host) {
    // round 6 (next), N3: no read access (a platform listener outside a read action): master's answer, no exception
    if (org.intellij.erlang.macrolayer.ErlangMacroLayer.noReadAccess()) return PsiReference.EMPTY_ARRAY;
    // M2: with the layer off, master's references only
    if (!org.intellij.erlang.macrolayer.ErlangMacroLayer.isEnabled()) return PsiReference.EMPTY_ARRAY;
    if (host instanceof StubBasedPsiElement<?> s && s.getStub() != null) return PsiReference.EMPTY_ARRAY; // no AST: no text search here
    ASTNode node = host.getNode();
    if (node == null) return PsiReference.EMPTY_ARRAY;
    List<PsiReference> result = new SmartList<>();
    if (host instanceof PsiFile) addSplitUses(host, result);
    if (!mayHold(node)) return result.isEmpty() ? PsiReference.EMPTY_ARRAY : result.toArray(PsiReference.EMPTY_ARRAY);
    List<ASTNode> seq = new ArrayList<>();
    collect(node, seq);
    for (int i = 0; i + 1 < seq.size(); i++) {
      ASTNode q = seq.get(i);
      if (q == null || q.getElementType() != ErlangTypes.ERL_QMARK) continue;
      ASTNode prev = i > 0 ? seq.get(i - 1) : null;
      if (prev != null && prev.getElementType() == ErlangTypes.ERL_QMARK) continue; // ??X
      ASTNode n = seq.get(i + 1);
      if (n == null) continue;
      if (n.getElementType() == ErlangTypes.ERL_SINGLE_QUOTE && i + 3 < seq.size() && seq.get(i + 2) != null &&
          seq.get(i + 2).getElementType() == ErlangTypes.ERL_ATOM_NAME && seq.get(i + 3) != null &&
          seq.get(i + 3).getElementType() == ErlangTypes.ERL_SINGLE_QUOTE) {
        n = seq.get(i + 2);
      }
      if (!NAMES.contains(n.getElementType())) continue;
      result.add(new ErlangBareMacroReference(host, n.getPsi()));
    }
    return result.isEmpty() ? PsiReference.EMPTY_ARRAY : result.toArray(PsiReference.EMPTY_ARRAY);
  }

  /**
   * A use split between two top-level forms: error recovery stops before {@code atom (}, so {@code foo?mac(a)} ends
   * one form with the bare {@code ?} and starts the next with {@code mac(a)}. Only the file holds both tokens.
   */
  private static void addSplitUses(@NotNull PsiElement file, @NotNull List<PsiReference> result) {
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      PsiElement last = PsiTreeUtil.lastChild(c);
      while (last != null && (last instanceof PsiWhiteSpace || last instanceof PsiComment || last.getTextLength() == 0)) last = PsiTreeUtil.prevLeaf(last);
      if (last == null || last.getNode().getElementType() != ErlangTypes.ERL_QMARK || last.getParent() instanceof ErlangMacros) continue;
      PsiElement prev = PsiTreeUtil.prevVisibleLeaf(last);
      if (prev != null && prev.getNode().getElementType() == ErlangTypes.ERL_QMARK) continue;
      PsiElement name = PsiTreeUtil.nextVisibleLeaf(last);
      if (name == null || !NAMES.contains(name.getNode().getElementType()) || PsiTreeUtil.isAncestor(c, name, false)) continue;
      if (PsiTreeUtil.getParentOfType(name, ErlangMacrosName.class) != null) continue;
      result.add(new ErlangBareMacroReference(file, name));
    }
  }

  private static boolean mayHold(@NotNull ASTNode node) {
    for (ASTNode c = node.getFirstChildNode(); c != null; c = c.getTreeNext()) {
      IElementType t = c.getElementType();
      if (t == ErlangTypes.ERL_QMARK || isForeign(t)) return true;
    }
    return false;
  }

  private static boolean isForeign(@NotNull IElementType t) {
    return t == TokenType.ERROR_ELEMENT || t == GeneratedParserUtilBase.DUMMY_BLOCK;
  }

  /** Significant leaves of the host and of its error elements and dummy blocks; null for an Erlang element between them. */
  private static void collect(@NotNull ASTNode node, @NotNull List<ASTNode> out) {
    for (ASTNode c = node.getFirstChildNode(); c != null; c = c.getTreeNext()) {
      IElementType t = c.getElementType();
      if (c.getFirstChildNode() == null && !isForeign(t)) {
        if (!SKIPPED.contains(t) && c.getTextLength() > 0) out.add(c);
      }
      else if (isForeign(t)) collect(c, out);
      else out.add(null);
    }
  }

  /** The name token of the use. */
  public @NotNull PsiElement getNameElement() {
    return myName;
  }

  @Override
  public @NotNull String getCanonicalText() {
    return myName.getText();
  }

  @Override
  public @Nullable PsiElement resolve() {
    // round 6 (next), N2: several possible definitions (conditional branches): no single target, go to declaration offers all
    ResolveResult[] r = multiResolve(false);
    return r.length == 1 ? r[0].getElement() : null;
  }

  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    if (!(myElement.getContainingFile() instanceof ErlangFile file)) return ResolveResult.EMPTY_ARRAY;
    int arity = ErlangMacroResolve.arityAfter(myName);
    List<ErlangMacrosDefinition> all = ErlangMacroResolve.resolveAll(file, myName.getText(), arity);
    if (!all.isEmpty()) return PsiElementResolveResult.createResults(all); // round 6 (next), item 4
    ErlangMacrosDefinition d = ErlangMacroResolve.resolve(file, myName.getText(), arity);
    return d == null ? ResolveResult.EMPTY_ARRAY : new ResolveResult[]{new PsiElementResolveResult(d)};
  }

  @Override
  public boolean isReferenceTo(@NotNull PsiElement element) {
    if (!(element instanceof ErlangMacrosDefinition d) || !myName.getText().equals(d.getName())) return false;
    if (!(myElement.getContainingFile() instanceof ErlangFile file)) return true;
    // round 6 (next), item 3b: a definition that epp may choose for this use's arity (any conditional branch)
    List<ErlangMacrosDefinition> m = ErlangMacroResolve.matching(file, myName.getText(), ErlangMacroResolve.arityAfter(myName));
    if (!m.isEmpty()) return m.contains(d);
    // a header may use a macro that only its includers define: then any definition of the name may be meant
    return ErlangMacroResolve.isVisibleOrUnknown(file, d);
  }

  @Override
  public Object @NotNull [] getVariants() {
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getMacrosLookupElements(myElement.getContainingFile()));
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    PsiElement replacement = ErlangMacroResolve.nameToken(myName.getProject(), newElementName);
    boolean quoted = PsiTreeUtil.prevLeaf(myName) instanceof LeafPsiElement l && l.getElementType() == ErlangTypes.ERL_SINGLE_QUOTE;
    if (replacement == null || quoted && replacement.getNode().getElementType() != ErlangTypes.ERL_ATOM_NAME) {
      throw new IncorrectOperationException("Cannot rename the macro use to " + newElementName);
    }
    myName.replace(replacement);
    return myElement;
  }

  @Override
  public String toString() {
    return "ErlangBareMacroReference(" + myName.getText() + ")";
  }
}
