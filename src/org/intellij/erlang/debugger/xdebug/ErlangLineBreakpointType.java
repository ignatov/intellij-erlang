package org.intellij.erlang.debugger.xdebug;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Processor;
import com.intellij.xdebugger.XDebuggerUtil;
import com.intellij.xdebugger.breakpoints.XLineBreakpointType;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangLineBreakpointType extends XLineBreakpointType<ErlangLineBreakpointProperties> {
  public static final String ID = "ErlangLineBreakpoint";
  public static final String NAME = "Line breakpoint";

  protected ErlangLineBreakpointType() {
    super(ID, NAME);
  }

  @Nullable
  @Override
  public ErlangLineBreakpointProperties createBreakpointProperties(@NotNull VirtualFile file, int line) {
    return new ErlangLineBreakpointProperties();
  }

  @Override
  public boolean canPutAt(@NotNull VirtualFile file, int line, @NotNull Project project) {
    if (file.getFileType() != ErlangFileType.MODULE) return false;
    return isLineBreakpointAvailable(file, line, project);
  }

  // it should return true for lines matching "Executable Lines"
  // description at http://www.erlang.org/doc/apps/debugger/debugger_chapter.html
  // and, ideally, it should return false otherwise
  private static boolean isLineBreakpointAvailable(VirtualFile file, int line, @NotNull Project project) {
    Document document = FileDocumentManager.getInstance().getDocument(file);
    if (document == null) return false;
    LineBreakpointAvailabilityProcessor canPutAtChecker = new LineBreakpointAvailabilityProcessor();
    XDebuggerUtil.getInstance().iterateLine(project, document, line, canPutAtChecker);
    return canPutAtChecker.isLineBreakpointAvailable();
  }

  private static final class LineBreakpointAvailabilityProcessor implements Processor<PsiElement> {
    private boolean myIsLineBreakpointAvailable;

    @Override
    public boolean process(PsiElement psiElement) {
      if (ErlangPsiImplUtil.isWhitespaceOrComment(psiElement) ||
        psiElement.getNode().getElementType() == ErlangTypes.ERL_DOT ||
        psiElement.getNode().getElementType() == ErlangTypes.ERL_ARROW) return true;
      @SuppressWarnings("unchecked")
      ErlangCompositeElement nonExecutableParent = PsiTreeUtil.getParentOfType(psiElement,
          ErlangGuard.class,
          ErlangArgumentDefinition.class,
          ErlangAttribute.class,
          ErlangRecordDefinition.class,
          ErlangIncludeLib.class,
          ErlangInclude.class,
          ErlangMacrosDefinition.class,
          ErlangTypeDefinition.class);
      if (nonExecutableParent != null) return true;
      ErlangClauseBody executableParent = PsiTreeUtil.getParentOfType(psiElement, ErlangClauseBody.class);
      if (executableParent != null) {
        myIsLineBreakpointAvailable = true;
        return false;
      }
      return true;
    }

    public boolean isLineBreakpointAvailable() {
      return myIsLineBreakpointAvailable;
    }
  }
}
