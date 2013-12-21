package org.intellij.erlang.jps.builder;

import com.intellij.openapi.util.io.FileUtilRt;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.builders.BuildRootDescriptor;
import org.jetbrains.jps.builders.BuildTarget;

import java.io.File;
import java.io.FileFilter;

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

  @NotNull
  @Override
  public FileFilter createFileFilter() {
    return new FileFilter() {
      @Override
      public boolean accept(File file) {
        return FileUtilRt.extensionEquals(file.getName(), "erl");
      }
    };
  }
}
