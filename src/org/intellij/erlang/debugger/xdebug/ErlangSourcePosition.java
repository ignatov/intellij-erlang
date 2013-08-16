package org.intellij.erlang.debugger.xdebug;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.xdebugger.XSourcePosition;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author savenko
 */
public class ErlangSourcePosition {
  private final ErlangFile myErlangFile;
  private final int myLine;
  private final ErlangFunction myFunction;

  public ErlangSourcePosition(ErlangFile erlangFile, int line) {
    myErlangFile = erlangFile;
    myLine = line;
    myFunction = null;
  }

  public ErlangSourcePosition(Project project, XSourcePosition sourcePosition) {
    PsiFile psiFile = PsiManager.getInstance(project).findFile(sourcePosition.getFile());
    if (!(psiFile instanceof ErlangFile)) throw new IllegalArgumentException("Invalid source position.");
    myErlangFile = (ErlangFile) psiFile;
    myLine = sourcePosition.getLine();
    myFunction = null;
  }

  public ErlangSourcePosition(ErlangFile module, String functionName, int arity) {
    myFunction = module.getFunction(functionName, arity);
    if (myFunction == null) throw  new IllegalArgumentException("Function not found.");
    myErlangFile = module;
    myLine = StringUtil.offsetToLineNumber(module.getText(), myFunction.getTextOffset());
  }

  @NotNull
  public String getErlangModuleName() {
    return FileUtil.getNameWithoutExtension(myErlangFile.getName());
  }

  @NotNull
  public ErlangFile getErlangFile() {
    return myErlangFile;
  }

  public int getLine() {
    return myLine;
  }

  @Nullable
  public ErlangFunction getFunction() {
    return myFunction;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    ErlangSourcePosition that = (ErlangSourcePosition) o;

    if (myLine != that.myLine) return false;
    if (myErlangFile != that.myErlangFile) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = 0;
    if (myErlangFile != null) {
      VirtualFile virtualFile = myErlangFile.getVirtualFile();
      if (virtualFile != null) {
        result = virtualFile.getPath().hashCode();
      }
    }
    result = 31 * result + myLine;
    return result;
  }
}
