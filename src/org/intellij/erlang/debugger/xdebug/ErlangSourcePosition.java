package org.intellij.erlang.debugger.xdebug;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.xdebugger.XSourcePosition;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunExpression;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangSourcePosition {
  private static final Pattern FUN_PATTERN = Pattern.compile("^-(.*)/(\\d+)-fun-(\\d+)-$");
  private final ErlangFile myErlangFile;
  private final int myLine;
  private final ErlangFunction myFunction;
  private final ErlangFunExpression myFunExpression;

  public ErlangSourcePosition(ErlangFile erlangFile, int line) {
    myErlangFile = erlangFile;
    myLine = line;
    myFunction = null;
    myFunExpression = null;
  }

  public ErlangSourcePosition(Project project, XSourcePosition sourcePosition) {
    PsiFile psiFile = PsiManager.getInstance(project).findFile(sourcePosition.getFile());
    if (!(psiFile instanceof ErlangFile)) throw new IllegalArgumentException("Invalid source position.");
    myErlangFile = (ErlangFile) psiFile;
    myLine = sourcePosition.getLine();
    myFunction = null;
    myFunExpression = null;
  }

  public ErlangSourcePosition(ErlangFile module, String functionOrFunExpression, int arity) {
    Matcher matcher = FUN_PATTERN.matcher(functionOrFunExpression);
    boolean inFunExpression = matcher.matches();
    String functionName = functionOrFunExpression;
    int functionArity = arity;
    int funExpressionArity = -1;
    if (inFunExpression) {
      functionName = matcher.group(1);
      functionArity = Integer.parseInt(matcher.group(2));
      funExpressionArity = Integer.parseInt(matcher.group(3));
    }
    myErlangFile = module;
    myFunction = module.getFunction(functionName, functionArity);
    if (myFunction != null) {
      myFunExpression = inFunExpression ? ErlangPsiImplUtil.findFunExpression(myFunction, funExpressionArity) : null;
      int offset = myFunExpression != null ? myFunExpression.getTextOffset() : myFunction.getTextOffset();
      myLine = StringUtil.offsetToLineNumber(module.getText(), offset);
    }
    else {
      myLine = 0;
      myFunExpression = null;
    }
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

  @Nullable
  public ErlangFunExpression getFunExpression() {
    return myFunExpression;
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
