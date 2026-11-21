package org.intellij.erlang.macrolayer;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Round 6: the expansion errors ({@link ErlangMacroLayerExpansionErrors}) as a pass of their own, after the main
 * highlighting: the platform runs an external annotator when the daemon has finished the other passes, so the normal
 * errors and inspections never wait for it.
 * <ul>
 *   <li>collect (a read action): the file and its modification stamp;</li>
 *   <li>annotate (a background thread, no lock held): reads the file in a non-blocking read action, which a write
 *   action cancels and restarts; it uses the layer's copy, building it only when no valid one is cached, and the
 *   errors of each form are cached on the copy, so a rerun after an unrelated change expands and parses nothing;</li>
 *   <li>apply: only when the file's stamp is still the one the result was computed for.</li>
 * </ul>
 * The daemon cancels a running pass on a write action (a change of a document or of PSI); moving the caret changes no
 * document and restarts nothing.
 */
public class ErlangMacroLayerExpansionErrorAnnotator
  extends ExternalAnnotator<ErlangMacroLayerExpansionErrorAnnotator.Info, ErlangMacroLayerExpansionErrorAnnotator.Result> {

  record Info(@NotNull ErlangFile file, long stamp) {
  }

  record Result(long stamp, @NotNull List<ErlangMacroLayerExpansionErrors.Problem> problems) {
  }

  @Override
  public @Nullable Info collectInformation(@NotNull PsiFile file, @NotNull Editor editor, boolean hasErrors) {
    return collectInformation(file);
  }

  @Override
  public @Nullable Info collectInformation(@NotNull PsiFile file) {
    if (!ErlangMacroLayer.isEnabled()) return null;
    ErlangFile f = ErlangMacroLayer.layerFile(file);
    return f == null ? null : new Info(f, f.getModificationStamp());
  }

  @Override
  public @Nullable Result doAnnotate(Info info) {
    if (info == null) return null;
    boolean edt = ApplicationManager.getApplication().isDispatchThread(); // tests run the pass in place
    for (int attempt = 0; ; attempt++) {
      ProgressManager.checkCanceled();
      List<ErlangMacroLayerExpansionErrors.Problem> problems = edt
        ? ReadAction.compute(() -> compute(info))
        : ReadAction.nonBlocking(() -> compute(info)).executeSynchronously();
      if (problems != null) return new Result(info.stamp(), problems);
      // the layer gave no copy: refused while the file was typed in; the pass runs after the typing, so wait once
      if (edt || attempt >= 2) return null;
      sleepCancellably(ErlangMacroLayer.quietMillis());
    }
  }

  private static @Nullable List<ErlangMacroLayerExpansionErrors.Problem> compute(@NotNull Info info) {
    ErlangFile file = info.file();
    if (!file.isValid() || file.getModificationStamp() != info.stamp()) return List.of();
    return ErlangMacroLayerExpansionErrors.problems(file);
  }

  private static void sleepCancellably(long millis) {
    long end = System.currentTimeMillis() + millis;
    while (System.currentTimeMillis() < end) {
      ProgressManager.checkCanceled();
      try {
        Thread.sleep(20);
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw new ProcessCanceledException(e);
      }
    }
  }

  @Override
  public void apply(@NotNull PsiFile file, Result result, @NotNull AnnotationHolder holder) {
    if (result == null || file.getModificationStamp() != result.stamp()) return;
    for (ErlangMacroLayerExpansionErrors.Problem p : result.problems()) {
      if (p.range().getEndOffset() > file.getTextLength()) continue;
      holder.newAnnotation(HighlightSeverity.ERROR, p.message()).range(p.range()).create();
    }
  }
}
