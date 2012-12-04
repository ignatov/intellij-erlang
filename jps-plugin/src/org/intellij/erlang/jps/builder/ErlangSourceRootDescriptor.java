package org.intellij.erlang.jps.builder;

import com.intellij.openapi.util.io.FileUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.builders.BuildRootDescriptor;
import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.cmdline.ProjectDescriptor;

import java.io.File;
import java.io.FileFilter;

/**
 * @author @nik
 */
public class ErlangSourceRootDescriptor extends BuildRootDescriptor {
  private File myRoot;
  private final ErlangTarget myErlangTarget;

  public ErlangSourceRootDescriptor(File root, ErlangTarget erlangTarget) {
    myRoot = root;
    myErlangTarget = erlangTarget;
  }

  @Override
  public String getRootId() {
    return myRoot.getAbsolutePath();
  }

  @Override
  public File getRootFile() {
    return myRoot;
  }

  @Override
  public BuildTarget<?> getTarget() {
    return myErlangTarget;
  }

  @Override
  public FileFilter createFileFilter(@NotNull ProjectDescriptor descriptor) {
    return new FileFilter() {
      @Override
      public boolean accept(File file) {
        return FileUtil.getExtension(file.getName()).equals("erl");
      }
    };
  }
}
