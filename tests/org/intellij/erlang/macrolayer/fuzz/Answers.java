package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * What the plugin answers about a file, in a form that does not depend on the case folder: every reference with what it
 * resolves to (file, offset and class of each target), and the highlighting of warning level and above. Two answers for
 * the same text in the same setting must be equal.
 */
public final class Answers {
  private Answers() {
  }

  public static @NotNull List<String> references(@NotNull PsiFile file, @NotNull String caseRoot) {
    List<String> r = new ArrayList<>();
    for (PsiElement e : PsiTreeUtil.collectElements(file, x -> true)) {
      for (PsiReference ref : e.getReferences()) {
        int offset = e.getTextRange().getStartOffset() + ref.getRangeInElement().getStartOffset();
        StringBuilder sb = new StringBuilder().append(offset).append(' ').append(ref.getClass().getSimpleName()).append(" -> ");
        List<String> targets = new ArrayList<>();
        if (ref instanceof PsiPolyVariantReference pv) {
          for (ResolveResult rr : pv.multiResolve(false)) targets.add(target(rr.getElement(), caseRoot));
        }
        else {
          targets.add(target(ref.resolve(), caseRoot));
        }
        Collections.sort(targets);
        sb.append(targets);
        r.add(sb.toString());
      }
    }
    Collections.sort(r);
    return r;
  }

  private static String target(@Nullable PsiElement t, String caseRoot) {
    if (t == null) return "null";
    if (!t.isValid()) return "INVALID:" + t.getClass().getSimpleName();
    PsiFile f = t.getContainingFile();
    VirtualFile vf = f == null ? null : f.getOriginalFile().getVirtualFile();
    String path = vf == null ? (f == null ? "-" : f.getName()) : vf.getPath();
    int i = path.indexOf(caseRoot + "/");
    if (i >= 0) path = path.substring(i + caseRoot.length() + 1);
    else if (path.contains("/lib/")) path = path.substring(path.indexOf("/lib/") + 1);
    return path + ":" + t.getTextOffset() + ":" + t.getClass().getSimpleName();
  }

  public static @NotNull List<String> highlights(@NotNull List<HighlightInfo> infos) {
    List<String> r = new ArrayList<>();
    for (HighlightInfo h : infos) {
      if (h.getSeverity().compareTo(HighlightSeverity.WARNING) < 0) continue;
      r.add(h.getStartOffset() + "-" + h.getEndOffset() + " " + h.getSeverity() + " " + h.getDescription());
    }
    Collections.sort(r);
    return r;
  }

  /** The first lines that differ, for a failure message. */
  public static @NotNull String diff(@NotNull List<String> a, @NotNull List<String> b) {
    Set<String> sa = new LinkedHashSet<>(a);
    Set<String> sb = new LinkedHashSet<>(b);
    List<String> onlyA = new ArrayList<>();
    for (String s : a) if (!sb.contains(s)) onlyA.add(s);
    List<String> onlyB = new ArrayList<>();
    for (String s : b) if (!sa.contains(s)) onlyB.add(s);
    return "only first: " + onlyA.subList(0, Math.min(5, onlyA.size())) + " only second: " + onlyB.subList(0, Math.min(5, onlyB.size()));
  }
}
