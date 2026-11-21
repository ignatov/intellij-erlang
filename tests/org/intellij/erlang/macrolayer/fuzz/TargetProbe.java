package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.lang.documentation.ide.IdeDocumentationTargetProvider;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.platform.backend.documentation.DocumentationTarget;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * Finds what the platform takes as the target or the reference at an offset, the way the editor actions do (target
 * element search with each flag set, the targets of Go to Declaration and of its handlers, the documentation targets of
 * Quick Documentation, the reference and what it resolves to), on the UI thread and in a background read action, and
 * pushes each new target through {@link ElementProbe}.
 */
public final class TargetProbe {
  private final FuzzRecorder myRec;
  private final Project myProject;
  private final Editor myEditor;
  private final PsiFile myFile;
  private final ElementProbe myProbe;
  private final Set<PsiElement> mySeen = Collections.newSetFromMap(new IdentityHashMap<>());
  private final Set<String> mySeenKinds = new HashSet<>();
  /** How many targets get the full element probe with searches; the rest get it without searches. */
  public int searchedTargets = 6;

  public TargetProbe(@NotNull FuzzRecorder rec, @NotNull Editor editor, @NotNull PsiFile file) {
    myRec = rec;
    myProject = file.getProject();
    myEditor = editor;
    myFile = file;
    myProbe = new ElementProbe(rec, myProject, editor);
  }

  /** Offsets worth probing: every start of a leaf in or next to a macro use, and a random sample of the others. */
  public static @NotNull List<Integer> offsets(@NotNull PsiFile file, @NotNull Random rnd, int others) {
    List<Integer> macro = new ArrayList<>();
    List<Integer> rest = new ArrayList<>();
    String text = file.getText();
    for (PsiElement leaf : PsiTreeUtil.collectElements(file, e -> e.getFirstChild() == null)) {
      int s = leaf.getTextRange().getStartOffset();
      boolean near = s > 0 && text.charAt(s - 1) == '?' || leaf.getText().startsWith("?") ||
                     PsiTreeUtil.findFirstParent(leaf, e -> e.getClass().getSimpleName().contains("Macro")) != null;
      (near ? macro : rest).add(s);
      if (near && leaf.getTextLength() > 1) macro.add(s + 1);
    }
    Collections.shuffle(rest, rnd);
    List<Integer> r = new ArrayList<>(macro);
    r.addAll(rest.subList(0, Math.min(others, rest.size())));
    r.add(text.length());
    return r;
  }

  public void probeOffset(int offset, boolean background) {
    String p = (background ? "bg " : "edt ") + "@" + offset + " ";
    List<Pair<String, PsiElement>> found = new ArrayList<>();
    myEditor.getCaretModel().moveToOffset(offset);
    run(p + "targets", background, () -> {
      TargetElementUtil u = TargetElementUtil.getInstance();
      int[] flags = {u.getAllAccepted(), u.getReferenceSearchFlags(), u.getDefinitionSearchFlags(),
        TargetElementUtil.ELEMENT_NAME_ACCEPTED | TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED};
      for (int f : flags) {
        PsiElement t = u.findTargetElement(myEditor, f, offset);
        if (t != null) found.add(Pair.create("target", t));
      }
      PsiReference r = TargetElementUtil.findReference(myEditor, offset);
      if (r != null) {
        for (PsiElement c : u.getTargetCandidates(r)) found.add(Pair.create("candidate", c));
        PsiElement adj = u.adjustReference(r);
        if (adj != null) found.add(Pair.create("adjusted", adj));
      }
    });
    run(p + "goto-declaration", background, () -> {
      PsiElement[] all = GotoDeclarationAction.findAllTargetElements(myProject, myEditor, offset);
      for (PsiElement t : all) found.add(Pair.create("goto", t));
      PsiElement src = myFile.findElementAt(offset);
      if (src != null) {
        for (GotoDeclarationHandler h : GotoDeclarationHandler.EP_NAME.getExtensionList()) {
          PsiElement[] ts = h.getGotoDeclarationTargets(src, offset, myEditor);
          if (ts != null) for (PsiElement t : ts) if (t != null) found.add(Pair.create("goto-handler", t));
        }
      }
    });
    run(p + "documentation", background, () -> {
      for (DocumentationTarget t : IdeDocumentationTargetProvider.getInstance(myProject).documentationTargets(myEditor, myFile, offset)) {
        t.computePresentation();
        t.computeDocumentationHint();
        t.getNavigatable();
        t.createPointer().dereference();
      }
    });
    run(p + "reference", background, () -> {
      PsiReference r = myFile.findReferenceAt(offset);
      if (r == null) return;
      r.getRangeInElement();
      r.getCanonicalText();
      r.isSoft();
      PsiElement t = r.resolve();
      if (t != null) {
        found.add(Pair.create("resolve", t));
        r.isReferenceTo(t);
      }
      if (r instanceof PsiPolyVariantReference pv) {
        for (ResolveResult rr : pv.multiResolve(false)) if (rr.getElement() != null) found.add(Pair.create("multi", rr.getElement()));
        pv.multiResolve(true);
      }
      if (!background) r.getVariants();
      r.isReferenceTo(myFile);
      PsiElement host = r.getElement();
      found.add(Pair.create("host", host));
    });
    if (!background) {
      myRec.guard(p + "rename-reference-on-copy", () -> {
        PsiFile copy = (PsiFile)myFile.copy();
        PsiReference r = copy.findReferenceAt(offset);
        if (r == null) return;
        WriteCommandAction.runWriteCommandAction(myProject, () -> {
          try {
            // a name of the same shape as the old one, as the rename refactoring validates it first
            String old = r.getCanonicalText();
            boolean upper = !old.isEmpty() && (Character.isUpperCase(old.charAt(0)) || old.charAt(0) == '_' || old.charAt(0) == '?');
            r.handleElementRename(upper ? "FuzzNew" : "fuzz_new");
          }
          catch (com.intellij.util.IncorrectOperationException ignored) {
            // a reference may refuse a name; that is its contract
          }
        });
      });
    }
    int n = 0;
    for (Pair<String, PsiElement> t : found) {
      if (!mySeen.add(t.second)) continue;
      String kind = t.first + ":" + t.second.getClass().getSimpleName();
      boolean firstOfKind = mySeenKinds.add(kind);
      myProbe.searches = firstOfKind && n++ < searchedTargets;
      myProbe.probe(kind + "@" + offset, t.second, !t.first.equals("host"));
    }
  }

  private void run(String label, boolean background, FuzzRecorder.Body body) {
    myRec.guard(label, () -> {
      if (background) {
        myProbe.background(() -> {
          try {
            body.run();
          }
          catch (RuntimeException | Error e) {
            throw e;
          }
          catch (Throwable t) {
            throw new RuntimeException(t);
          }
          return null;
        });
      }
      else {
        body.run();
      }
    });
  }
}
