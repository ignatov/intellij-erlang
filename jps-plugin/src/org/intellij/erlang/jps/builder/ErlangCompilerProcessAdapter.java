package org.intellij.erlang.jps.builder;

import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFileManager;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;

/**
 * @author savenko
 */
public class ErlangCompilerProcessAdapter extends ProcessAdapter {
  private final CompileContext myContext;
  private final String myBuilderName;
  private final String myCompileTargetRootPath;

  public ErlangCompilerProcessAdapter(CompileContext context, String builderName, String compileTargetRootPath) {
    myContext = context;
    myBuilderName = builderName;
    myCompileTargetRootPath = compileTargetRootPath;
  }

  @Override
  public void onTextAvailable(ProcessEvent event, Key outputType) {
    ErlangCompilerError error = ErlangCompilerError.create(myCompileTargetRootPath, event.getText());
    if (error != null) {
      boolean isError = error.getCategory() == CompilerMessageCategory.ERROR;
      BuildMessage.Kind kind = isError ? BuildMessage.Kind.ERROR : BuildMessage.Kind.WARNING;
      CompilerMessage msg = new CompilerMessage(myBuilderName, kind, error.getErrorMessage(),
        VirtualFileManager.extractPath(error.getUrl()), -1, -1, -1, error.getLine(), -1);
      myContext.processMessage(msg);
    }
  }
}
