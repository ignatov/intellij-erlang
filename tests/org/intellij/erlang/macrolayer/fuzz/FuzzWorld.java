package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * The files of one case in the light project: the case's Erlang files plus a fixed set of other things a user can
 * select (directories with and without files, an empty directory nested in another, a text file, binary files, an XML
 * file), all under one directory {@code c<N>/} of the temporary source root, which {@link #dispose()} deletes again (in
 * the in-memory file system of the light fixture).
 */
public final class FuzzWorld {
  private static final byte[] PNG = {(byte)0x89, 'P', 'N', 'G', 13, 10, 26, 10, 0, 0, 0, 13, 'I', 'H', 'D', 'R', 0, 0, 0, 1, 0, 0, 0, 1,
    8, 6, 0, 0, 0, 0x1f, 0x15, (byte)0xc4, (byte)0x89, 0, 0, 0, 0, 'I', 'E', 'N', 'D', (byte)0xae, 'B', 0x60, (byte)0x82};
  private static final byte[] BEAM = {'F', 'O', 'R', '1', 0, 0, 0, 20, 'B', 'E', 'A', 'M', 'A', 't', 'U', '8', 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0};

  private static int ourCounter;

  public final CodeInsightTestFixture fixture;
  public final Project project;
  public final String root;
  public final VirtualFile rootDir;
  public final Map<String, VirtualFile> files = new LinkedHashMap<>();
  private final Map<String, String> myTexts = new LinkedHashMap<>();

  public FuzzWorld(@NotNull CodeInsightTestFixture fixture, @NotNull Map<String, String> erlangFiles, boolean extras) {
    this.fixture = fixture;
    this.project = fixture.getProject();
    this.root = "c" + (ourCounter++);
    try {
      VirtualFile[] holder = new VirtualFile[1];
      WriteAction.runAndWait(() -> {
        VirtualFile src = fixture.getTempDirFixture().getFile("");
        if (src == null) src = fixture.getTempDirFixture().findOrCreateDir("");
        VirtualFile dir = src.createChildDirectory(this, root);
        holder[0] = dir;
        for (Map.Entry<String, String> e : erlangFiles.entrySet()) put(dir, e.getKey(), e.getValue().getBytes(StandardCharsets.UTF_8));
        if (extras) {
          put(dir, "notes.txt", "a text file ?MACRO -define(X, 1).\n".getBytes(StandardCharsets.UTF_8));
          put(dir, "ebin/x.beam", BEAM);
          put(dir, "img/logo.png", PNG);
          put(dir, "conf/pom.xml", "<project><a>?X</a></project>\n".getBytes(StandardCharsets.UTF_8));
          put(dir, "rebar.config", "{erl_opts, [debug_info, {d, 'FROM_REBAR', 1}]}.\n".getBytes(StandardCharsets.UTF_8));
          VfsUtil.createDirectoryIfMissing(dir, "empty/pkg");     // a directory whose only child is an empty directory
          VfsUtil.createDirectoryIfMissing(dir, "zz_empty");      // an empty directory
        }
      });
      rootDir = holder[0];
    }
    catch (Throwable e) {
      throw new IllegalStateException(e);
    }
    myTexts.putAll(erlangFiles);
    PsiDocumentManager.getInstance(project).commitAllDocuments();
  }

  private void put(VirtualFile dir, String path, byte[] content) throws IOException {
    int slash = path.lastIndexOf('/');
    VirtualFile parent = slash < 0 ? dir : VfsUtil.createDirectoryIfMissing(dir, path.substring(0, slash));
    VirtualFile f = parent.createChildData(this, slash < 0 ? path : path.substring(slash + 1));
    f.setBinaryContent(content);
    files.put(path, f);
  }

  public @NotNull Map<String, String> texts() {
    return myTexts;
  }

  public @Nullable PsiFile psi(@NotNull String path) {
    VirtualFile f = files.get(path);
    return f == null ? null : PsiManager.getInstance(project).findFile(f);
  }

  public @Nullable PsiDirectory dir(@NotNull String path) {
    VirtualFile d = path.isEmpty() ? rootDir : rootDir.findFileByRelativePath(path);
    return d == null ? null : PsiManager.getInstance(project).findDirectory(d);
  }

  public @NotNull PsiDirectory sourceRoot() {
    return Objects.requireNonNull(PsiManager.getInstance(project).findDirectory(rootDir.getParent()));
  }

  public @NotNull List<String> erlangPaths() {
    List<String> r = new ArrayList<>();
    for (String p : files.keySet()) if (p.endsWith(".erl") || p.endsWith(".hrl")) r.add(p);
    return r;
  }

  public void dispose() {
    try {
      WriteAction.runAndWait(() -> {
        if (rootDir.isValid()) rootDir.delete(this);
      });
    }
    catch (Throwable ignored) {
    }
  }
}
