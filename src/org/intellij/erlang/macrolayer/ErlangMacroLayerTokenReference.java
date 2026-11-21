package org.intellij.erlang.macrolayer;

import com.intellij.lang.ASTNode;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.SmartList;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Round 6: a name token of the edited file that the main tree gives no reference, but that has one in the expanded
 * copy: {@code foo} in {@code foo?PAR} (with {@code -define(PAR, ())}) is a plain atom in the main tree and the name
 * of a call of {@code foo/0} in the copy; a record field name in the arguments of a macro that builds record fields;
 * a token that error recovery left bare. The reference stands on the token in the main tree and resolves through the
 * copy's reference at the same token, mapped back to real elements.
 * <p>
 * Candidates (structure only, no copy): the name of a standalone atom (the main tree's atom reference resolves to
 * nothing), and a bare atom or variable token (no element of its own) that is not the name of a bare macro use. Only
 * in a top-level form with a use of a visible macro, outside {@code -define}. The copy is asked only when the reference
 * is resolved and the round-5 gate sends the form to the copy, through the same entry point and refusal rules as the
 * other questions of the layer.
 */
public final class ErlangMacroLayerTokenReference extends PsiPolyVariantReferenceBase<PsiElement> {
  private static final TokenSet SKIPPED = TokenSet.orSet(ErlangParserDefinition.COMMENTS, TokenSet.create(TokenType.WHITE_SPACE));
  private static final TokenSet NAMES = TokenSet.create(ErlangTypes.ERL_VAR, ErlangTypes.ERL_ATOM_NAME);

  private final @NotNull PsiElement myLeaf;

  private ErlangMacroLayerTokenReference(@NotNull PsiElement host, @NotNull PsiElement leaf) {
    super(host, TextRange.from(leaf.getTextRange().getStartOffset() - host.getTextRange().getStartOffset(), leaf.getTextLength()));
    myLeaf = leaf;
  }

  /** The references of the host's candidate tokens (empty when the layer is off or the token's form needs no copy). */
  public static PsiReference @NotNull [] find(@NotNull PsiElement host) {
    if (!ErlangMacroLayer.isEnabled()) return PsiReference.EMPTY_ARRAY;
    List<PsiElement> leaves = host instanceof PsiFile file
                              ? CachedValuesManager.getCachedValue(file, () -> CachedValueProvider.Result.create(candidates(file), file))
                              : candidates(host);
    if (leaves.isEmpty()) return PsiReference.EMPTY_ARRAY;
    List<PsiReference> r = new SmartList<>();
    PsiElement lastForm = null;
    boolean lastAnswer = false, first = true;
    for (PsiElement leaf : leaves) {
      PsiElement form = ErlangMacroLayer.topForm(leaf);
      if (first || form != lastForm) {
        first = false;
        lastForm = form;
        lastAnswer = inFormWithUse(leaf, form);
      }
      if (lastAnswer) r.add(new ErlangMacroLayerTokenReference(host, leaf));
    }
    return r.isEmpty() ? PsiReference.EMPTY_ARRAY : r.toArray(PsiReference.EMPTY_ARRAY);
  }

  /** The host's own references followed by the token references it holds. */
  public static PsiReference @NotNull [] append(@NotNull PsiElement host, PsiReference @NotNull [] own) {
    PsiReference[] tokens = find(host);
    return tokens.length == 0 ? own : com.intellij.util.ArrayUtil.mergeArrays(own, tokens);
  }

  private static @NotNull List<PsiElement> candidates(@NotNull PsiElement host) {
    if (host instanceof ErlangAtom atom) {
      if (!(atom.getParent() instanceof ErlangQAtom q) || !ErlangPsiImplUtil.standaloneAtom(q)) return List.of();
      PsiElement name = atom.getNameIdentifier();
      return name.getNode().getElementType() == ErlangTypes.ERL_ATOM_NAME ? List.of(name) : List.of();
    }
    ASTNode node = host.getNode();
    if (node == null || !hasForeignChild(node)) return List.of();
    List<ASTNode> seq = new ArrayList<>();
    collect(node, seq, false);
    List<PsiElement> r = new SmartList<>();
    for (int i = 0; i < seq.size(); i++) {
      ASTNode n = seq.get(i);
      if (n == null || !NAMES.contains(n.getElementType())) continue;
      ASTNode prev = i > 0 ? seq.get(i - 1) : null;
      if (prev != null && (prev.getElementType() == ErlangTypes.ERL_QMARK ||
                           prev.getElementType() == ErlangTypes.ERL_SINGLE_QUOTE && i > 1 && seq.get(i - 2) != null &&
                           seq.get(i - 2).getElementType() == ErlangTypes.ERL_QMARK)) {
        continue; // the name of a bare macro use: ErlangBareMacroReference
      }
      if (isAttributeName(n.getPsi())) continue;
      r.add(n.getPsi());
    }
    return r;
  }

  /** {@code export} in {@code -export(...)}: the name of an attribute, a keyword of the form. */
  private static boolean isAttributeName(@NotNull PsiElement leaf) {
    PsiElement prev = PsiTreeUtil.prevVisibleLeaf(leaf);
    if (prev == null || prev.getNode().getElementType() != ErlangTypes.ERL_OP_MINUS) return false;
    PsiElement form = ErlangMacroLayer.topForm(prev);
    return form != null && PsiTreeUtil.firstChild(form) == prev;
  }

  private static boolean hasForeignChild(@NotNull ASTNode node) {
    for (ASTNode c = node.getFirstChildNode(); c != null; c = c.getTreeNext()) {
      IElementType t = c.getElementType();
      if (isForeign(t) || c.getFirstChildNode() == null && NAMES.contains(t) && !(c.getTreeParent().getPsi() instanceof ErlangAtom) &&
                          !(c.getTreeParent().getPsi() instanceof ErlangQVar) && !(c.getTreeParent().getPsi() instanceof ErlangMacrosName)) {
        return true;
      }
    }
    return false;
  }

  private static boolean isForeign(@NotNull IElementType t) {
    return t == TokenType.ERROR_ELEMENT || t == GeneratedParserUtilBase.DUMMY_BLOCK;
  }

  /** Bare significant leaves of the host and of its error elements and dummy blocks; null for an element between them. */
  private static void collect(@NotNull ASTNode node, @NotNull List<ASTNode> out, boolean foreign) {
    boolean ownNames = !(node.getPsi() instanceof ErlangAtom) && !(node.getPsi() instanceof ErlangQVar) &&
                       !(node.getPsi() instanceof ErlangMacrosName);
    for (ASTNode c = node.getFirstChildNode(); c != null; c = c.getTreeNext()) {
      IElementType t = c.getElementType();
      if (c.getFirstChildNode() == null && !isForeign(t)) {
        if (!SKIPPED.contains(t) && c.getTextLength() > 0) out.add(ownNames || foreign ? c : null);
      }
      else if (isForeign(t)) collect(c, out, true);
      else out.add(null);
    }
  }

  /**
   * The token lies in a top-level form, outside -define, with a use of a macro the file sees: the reference exists
   * there whatever the gate says (so that the narrow and the wide gate give the same references), and resolves through
   * the copy only when the gate sends the form to it ({@link #answer}). Never builds a copy.
   */
  private static boolean inFormWithUse(@NotNull PsiElement leaf, @Nullable PsiElement form) {
    if (form == null) return false;
    ErlangFile file = ErlangMacroLayer.layerFile(leaf);
    if (file == null) return false;
    if (PsiTreeUtil.getParentOfType(leaf, ErlangMacrosDefinition.class, false) != null) return false;
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) return false;
    return ErlangMacroLayer.formUsesMacro(t, form);
  }

  /** The round-5 gate sends the token's form to the copy. Never builds a copy. */
  private static boolean formNeedsCopy(@NotNull ErlangFile file, @NotNull PsiElement leaf) {
    ErlangMacroLayer.Table t = ErlangMacroLayer.table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) return false;
    if (!ErlangMacroLayer.isGateWide() && !ErlangMacroLayer.anyFormNeedsCopy(file, t)) return false;
    return ErlangMacroLayer.formNeedsCopy(t, ErlangMacroLayer.topForm(leaf));
  }

  public @NotNull PsiElement getToken() {
    return myLeaf;
  }

  @Override
  public @NotNull String getCanonicalText() {
    return myLeaf.getText();
  }

  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    Answer a = answer(myLeaf, "token");
    return a == null ? ResolveResult.EMPTY_ARRAY : a.results;
  }

  @Override
  public boolean isReferenceTo(@NotNull PsiElement element) {
    PsiManager manager = getElement().getManager();
    for (ResolveResult r : multiResolve(false)) {
      PsiElement e = r.getElement();
      if (manager.areElementsEquivalent(e, element)) return true;
      if (e instanceof PsiNameIdentifierOwner o && manager.areElementsEquivalent(o.getNameIdentifier(), element)) return true;
    }
    return false;
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    if (myElement instanceof ErlangAtom atom) return ErlangPsiImplUtil.setName(atom, newElementName);
    PsiElement replacement = tokenOf(newElementName);
    if (replacement == null || replacement.getNode().getElementType() != myLeaf.getNode().getElementType()) {
      throw new IncorrectOperationException("Cannot rename " + myLeaf.getText() + " to " + newElementName + " here");
    }
    myLeaf.replace(replacement);
    return myElement;
  }

  private @Nullable PsiElement tokenOf(@NotNull String name) {
    try {
      PsiElement e = org.intellij.erlang.psi.impl.ErlangElementFactory.createMacrosFromText(myLeaf.getProject(), name);
      if (!(e instanceof ErlangMacrosName m)) return null;
      if (m.getVar() != null) return m.getVar();
      if (m.getAtom() != null && m.getAtom().getFirstChild() == m.getAtom().getLastChild()) return m.getAtom().getFirstChild();
    }
    catch (Exception ignore) {
    }
    return null;
  }

  @Override
  public Object @NotNull [] getVariants() {
    return EMPTY_ARRAY;
  }

  @Override
  public String toString() {
    return "ErlangMacroLayerTokenReference(" + myLeaf.getText() + ")";
  }

  // ------------------------------------------------------------------------------------------------ the copy's answer

  /** The copy's reference at a token and its real targets. {@code copyReference} lives in the copy: never let it out. */
  record Answer(@NotNull PsiReference copyReference, @NotNull ResolveResult @NotNull [] results) {
  }

  private static final Object NONE = new Object();

  /**
   * The reference of the copy at the token (same start, same text) in a clean expanded form, with its targets
   * mapped to real elements; null when the copy is not there, the form is not clean, or no reference stands there.
   */
  static @Nullable Answer answer(@NotNull PsiElement leaf, @NotNull String trigger) {
    ErlangFile file = ErlangMacroLayer.layerFile(leaf);
    if (file == null || !formNeedsCopy(file, leaf)) return null;
    ErlangMacroLayerLog.query(trigger, file.getName());
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(file, trigger);
    if (copy == null || copy.copyFile == null) return null;
    String key = "t" + leaf.getTextRange().getStartOffset();
    Object cached = copy.answers.get(key);
    if (cached != null) return cached == NONE ? null : (Answer)cached;
    Answer a = compute(copy, leaf, file);
    copy.answers.put(key, a == null ? NONE : a);
    return a;
  }

  private static @Nullable Answer compute(@NotNull ErlangMacroLayer.Copy copy, @NotNull PsiElement leaf, @NotNull ErlangFile file) {
    TextRange abs = leaf.getTextRange();
    String text = leaf.getText();
    PsiReference first = null;
    LinkedHashSet<PsiElement> targets = new LinkedHashSet<>();
    for (int e : copy.toCopyOffsets(abs)) {
      Form form = copy.expansion.formAtExpanded(e);
      if (!copy.isClean(form)) continue;
      PsiReference c = copy.copyFile.findReferenceAt(e);
      if (c == null) continue;
      TextRange cabs = c.getRangeInElement().shiftRight(c.getElement().getTextRange().getStartOffset());
      if (cabs.getStartOffset() != e || !text.contentEquals(cabs.subSequence(copy.expansion.text))) continue;
      if (first == null) first = c;
      // resolve() first: the multiResolve of a function reference is not the same answer (round 5)
      PsiElement single = c.resolve();
      if (single != null) {
        PsiElement real = ErlangMacroLayer.exit(copy.toReal(single), file);
        if (real != null) targets.add(real);
      }
      else if (c instanceof PsiPolyVariantReference p) {
        for (ResolveResult r : p.multiResolve(false)) {
          PsiElement real = ErlangMacroLayer.exit(copy.toReal(r.getElement()), file);
          if (real != null) targets.add(real);
        }
      }
    }
    if (first == null) return null;
    return new Answer(first, PsiElementResolveResult.createResults(targets));
  }

  /**
   * "name/arity" when the copy's reference at the token is a local function reference that resolves to nothing (erlc
   * reports {@code function name/arity undefined}); null otherwise, also when the copy is not there.
   */
  public static @Nullable String unresolvedFunctionAt(@NotNull PsiElement leaf) {
    if (!ErlangMacroLayer.isEnabled()) return null;
    PsiElement host = leaf.getParent() instanceof ErlangAtom atom ? atom : leaf.getParent();
    if (host == null) return null;
    boolean candidate = false;
    for (PsiReference r : find(host)) if (r instanceof ErlangMacroLayerTokenReference t && t.myLeaf == leaf) candidate = true;
    if (!candidate) return null;
    Answer a = answer(leaf, "token-inspection");
    if (a == null || a.results.length > 0) return null;
    PsiReference c = a.copyReference;
    if (c instanceof PsiMultiReference m) {
      for (PsiReference r : m.getReferences()) if (r instanceof ErlangFunctionReference) c = r;
    }
    if (!(c instanceof ErlangFunctionReference fr)) return null;
    PsiElement e = c.getElement();
    if (e.getParent() instanceof ErlangGlobalFunctionCallExpression || c.resolve() != null) return null;
    return fr.getName() + "/" + fr.getArity();
  }
}
