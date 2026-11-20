package org.intellij.erlang.spikeprobe;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.atomic.AtomicInteger;

/** Verifier 3: reflection access to the layer, so that the probes compile on master too. */
final class Verifier3Support {
  private Verifier3Support() {
  }

  static @Nullable Class<?> layer() {
    try {
      return Class.forName("org.intellij.erlang.macrolayer.ErlangMacroLayer");
    }
    catch (ClassNotFoundException e) {
      return null;
    }
  }

  static int counter(String name) {
    Class<?> c = layer();
    if (c == null) return -1;
    try {
      return ((AtomicInteger)c.getField(name).get(null)).get();
    }
    catch (Exception e) {
      return -2;
    }
  }

  static @Nullable Object copy(PsiFile f) {
    Class<?> c = layer();
    if (c == null) return null;
    try {
      return c.getMethod("getCopy", PsiFile.class).invoke(null, f);
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  static @Nullable Object build(PsiFile f) {
    Class<?> c = layer();
    if (c == null) return null;
    try {
      return c.getMethod("build", PsiFile.class).invoke(null, f);
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  static @Nullable Object field(@Nullable Object o, String name) {
    if (o == null) return null;
    try {
      return o.getClass().getField(name).get(o);
    }
    catch (Exception e) {
      try {
        java.lang.reflect.Field f = o.getClass().getDeclaredField(name);
        f.setAccessible(true);
        return f.get(o);
      }
      catch (Exception e2) {
        return null;
      }
    }
  }

  static boolean setRemote(boolean on, Disposable d) {
    Class<?> c = layer();
    if (c == null) return false;
    try {
      c.getMethod("setRemoteForTests", boolean.class, Disposable.class).invoke(null, on, d);
      return true;
    }
    catch (Exception e) {
      return false;
    }
  }

  static String where(@Nullable PsiElement e) {
    if (e == null) return "null";
    PsiFile f = e.getContainingFile();
    String pos = "?";
    if (f != null && e.getTextRange() != null) {
      Document d = PsiDocumentManager.getInstance(f.getProject()).getDocument(f);
      int off = e.getTextRange().getStartOffset();
      if (d != null && off <= d.getTextLength()) {
        int line = d.getLineNumber(off);
        pos = (line + 1) + ":" + (off - d.getLineStartOffset(line) + 1);
      }
      else pos = "@" + off;
    }
    String text = e.getText();
    if (text != null && text.length() > 40) text = text.substring(0, 40) + "...";
    return (f == null ? "nofile" : f.getName()) + (f != null && !f.isPhysical() ? "[NONPHYSICAL]" : "") + ":" + pos + " " +
           e.getClass().getSimpleName() + " '" + (text == null ? "" : text.replace('\n', ' ')) + "'";
  }
}
