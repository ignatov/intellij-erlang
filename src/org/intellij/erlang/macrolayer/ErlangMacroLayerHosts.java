package org.intellij.erlang.macrolayer;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.StubBasedPsiElement;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import org.intellij.erlang.psi.ErlangAtom;
import org.intellij.erlang.psi.impl.ErlangBareMacroReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Which elements of a file can host the layer's references: an element with a direct child that is a {@code ?} token,
 * an error element, a dummy block or a bare name token other than the name of an attribute (the conditions under which
 * {@link ErlangBareMacroReference} and {@link ErlangMacroLayerTokenReference} look at the children at all). The platform asks every ancestor of every
 * word occurrence for its references; without this answer each of them scanned its children (quadratic in a long
 * list or a large function).
 * <p>
 * The set is computed in one pass over the file's tree and cached on the file; it reads only that tree, so the file's
 * own PSI changes are its only dependency. It does not depend on the switch, the table or other files (those decide,
 * later, whether a candidate gets a reference). Threading: under a read action.
 */
public final class ErlangMacroLayerHosts {
  /** Scans of an element's children by the hosted references' find (tests: a file with nothing to host needs none). */
  public static final AtomicLong CHILD_SCANS = new AtomicLong();
  /** Leaves walked by the hosted references' find (tests: the work of one search grows with the uses, not with their square). */
  public static final AtomicLong LEAF_VISITS = new AtomicLong();

  private static final Key<CachedValue<Hosts>> HOSTS = Key.create("erlang.macro.layer.hosts");

  /** The elements that may host a bare macro use, and those that may host a token reference. */
  private record Hosts(@NotNull Set<PsiElement> bareUses, @NotNull Set<PsiElement> tokens) {
  }

  private ErlangMacroLayerHosts() {
  }

  /**
   * The element may host a reference of the layer: false when the layer is off or there is no read access (both
   * finds give none then), and for an element that is in neither set and is not an atom (an atom has its own path in
   * {@link ErlangMacroLayerTokenReference}). One look-up per asked element, before either find.
   */
  public static boolean mayHostAny(@NotNull PsiElement host) {
    if (!ErlangMacroLayer.isEnabled() || ErlangMacroLayer.noReadAccess()) return false;
    if (host instanceof ErlangAtom) return true;
    Hosts hosts = hostsOf(host);
    return hosts == null || hosts.bareUses().contains(host) || hosts.tokens().contains(host);
  }

  /** The element may host a bare macro use (false: it hosts none, its children need no scan). */
  public static boolean mayHostBareUse(@NotNull PsiElement host) {
    Hosts hosts = hostsOf(host);
    return hosts == null || hosts.bareUses().contains(host);
  }

  /** The element may host a token reference (false: it hosts none, its children need no scan). */
  public static boolean mayHostToken(@NotNull PsiElement host) {
    Hosts hosts = hostsOf(host);
    return hosts == null || hosts.tokens().contains(host);
  }

  /** The hosts of the element's file; null when there is no answer and the caller keeps the old path. */
  private static @Nullable Hosts hostsOf(@NotNull PsiElement host) {
    // an element read from a stub has no tree here; it keeps the old path, which loads none either
    if (host instanceof StubBasedPsiElement<?> s && s.getStub() != null) return null;
    PsiFile file = host.getContainingFile();
    if (file == null) return null;
    return CachedValuesManager.getManager(file.getProject()).getCachedValue(file, HOSTS,
      () -> CachedValueProvider.Result.create(compute(file), file), false);
  }

  private static @NotNull Hosts compute(@NotNull PsiFile file) {
    ASTNode root = file.getNode();
    if (root == null) return new Hosts(Collections.emptySet(), Collections.emptySet());
    Set<PsiElement> bareUses = new HashSet<>();
    Set<PsiElement> tokens = new HashSet<>();
    Deque<ASTNode> stack = new ArrayDeque<>();
    stack.push(root);
    while (!stack.isEmpty()) {
      ASTNode node = stack.pop();
      if (node.getFirstChildNode() == null) continue;
      if (ErlangBareMacroReference.mayHoldBareUse(node)) bareUses.add(node.getPsi());
      if (ErlangMacroLayerTokenReference.mayHoldToken(node, true)) tokens.add(node.getPsi());
      for (ASTNode c = node.getFirstChildNode(); c != null; c = c.getTreeNext()) stack.push(c);
    }
    return new Hosts(bareUses, tokens);
  }
}
