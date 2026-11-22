package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.lang.Language;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.fileTypes.PlainTextLanguage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubTree;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Elements of every kind the platform can hand to plugin code: directories (with files, empty, holding only an empty
 * directory, the source root), files of each kind (Erlang module and header, text, binary, XML), elements of another
 * language, non-physical Erlang files and their elements, a copy of a file, invalidated elements, stub-based elements
 * of a file whose tree is not loaded, and elements of the layer's expanded copy.
 */
public final class ElementZoo {
  private ElementZoo() {
  }

  public static @NotNull List<Pair<String, PsiElement>> nonErlang(@NotNull FuzzWorld w) {
    List<Pair<String, PsiElement>> r = new ArrayList<>();
    add(r, "dir-case", w.dir(""));
    add(r, "dir-empty-parent", w.dir("empty"));
    add(r, "dir-empty", w.dir("empty/pkg"));
    add(r, "dir-empty2", w.dir("zz_empty"));
    add(r, "dir-ebin", w.dir("ebin"));
    add(r, "dir-root", w.sourceRoot());
    for (String p : List.of("notes.txt", "ebin/x.beam", "img/logo.png", "conf/pom.xml", "rebar.config")) add(r, "file " + p, w.psi(p));
    PsiFile xml = w.psi("conf/pom.xml");
    if (xml != null) {
      PsiElement leaf = xml.findElementAt(2);
      add(r, "xml-leaf", leaf);
      add(r, "xml-parent", leaf == null ? null : leaf.getParent());
    }
    PsiFile txt = w.psi("notes.txt");
    if (txt != null) add(r, "text-leaf", txt.findElementAt(3));
    Project p = w.project;
    PsiFile plain = PsiFileFactory.getInstance(p).createFileFromText("x.txt", PlainTextLanguage.INSTANCE, "?MACRO plain");
    add(r, "nonphysical-text", plain);
    add(r, "nonphysical-text-leaf", plain.findElementAt(1));
    Language xmlLang = Language.findLanguageByID("XML");
    if (xmlLang != null) {
      PsiFile x = PsiFileFactory.getInstance(p).createFileFromText("x.xml", xmlLang, "<a b=\"1\">?M</a>");
      add(r, "nonphysical-xml-leaf", x.findElementAt(1));
      add(r, "nonphysical-xml-tag", x.findElementAt(1) == null ? null : x.findElementAt(1).getParent());
    }
    return r;
  }

  /** Erlang elements of a file: the file, a sample of its elements, and every element whose class names a macro. */
  public static @NotNull List<Pair<String, PsiElement>> erlang(@NotNull String prefix, @Nullable PsiFile file, @NotNull Random rnd, int sample) {
    List<Pair<String, PsiElement>> r = new ArrayList<>();
    if (file == null) return r;
    add(r, prefix + " file", file);
    Collection<PsiElement> all = PsiTreeUtil.findChildrenOfType(file, PsiElement.class);
    List<PsiElement> macroish = new ArrayList<>();
    List<PsiElement> other = new ArrayList<>();
    for (PsiElement e : all) (e.getClass().getSimpleName().contains("Macro") ? macroish : other).add(e);
    Collections.shuffle(other, rnd);
    for (PsiElement e : macroish) add(r, prefix + " " + e.getClass().getSimpleName() + "@" + e.getTextOffset(), e);
    for (int i = 0; i < Math.min(sample, other.size()); i++) {
      PsiElement e = other.get(i);
      add(r, prefix + " " + e.getClass().getSimpleName() + "@" + e.getTextOffset(), e);
    }
    return r;
  }

  public static @Nullable PsiFile nonPhysicalErlang(@NotNull Project p, @NotNull String name, @NotNull String text) {
    return PsiFileFactory.getInstance(p).createFileFromText(name, FileTypeManager.getInstance().getFileTypeByExtension("erl"), text);
  }

  /** Elements of {@code file} taken before an edit of it, so that they are invalid afterwards. */
  public static @NotNull List<Pair<String, PsiElement>> invalidated(@NotNull PsiFile file, @NotNull Random rnd) {
    List<Pair<String, PsiElement>> r = erlang("invalid", file, rnd, 15);
    Document doc = PsiDocumentManager.getInstance(file.getProject()).getDocument(file);
    if (doc == null) return List.of();
    WriteAction.run(() -> {
      doc.setText("-module(gone).\n");
      PsiDocumentManager.getInstance(file.getProject()).commitDocument(doc);
    });
    List<Pair<String, PsiElement>> out = new ArrayList<>();
    for (Pair<String, PsiElement> pe : r) if (!pe.second.isValid()) out.add(pe);
    return out;
  }

  /** Stub-based elements of a file whose tree is not loaded (empty when it is). */
  public static @NotNull List<Pair<String, PsiElement>> stubbed(@Nullable PsiFile file) {
    List<Pair<String, PsiElement>> r = new ArrayList<>();
    if (!(file instanceof PsiFileImpl impl) || impl.getTreeElement() != null) return r;
    StubTree tree = impl.getStubTree();
    if (tree == null) return r;
    for (StubElement<?> s : tree.getPlainList()) {
      PsiElement e = s.getPsi();
      if (e != null && !(e instanceof PsiFile)) add(r, "stub " + e.getClass().getSimpleName(), e);
    }
    return r;
  }

  /** Whether the zoo name is of an element that is not an Erlang element (a directory, a file, another language). */
  public static boolean isOtherKind(@NotNull String name) {
    return name.startsWith("dir") || name.startsWith("file ") || name.startsWith("xml") || name.startsWith("text") ||
           name.startsWith("nonphysical-") || name.endsWith(" file");
  }

  private static void add(List<Pair<String, PsiElement>> r, String name, @Nullable PsiElement e) {
    if (e != null) r.add(Pair.create(name, e));
  }
}
