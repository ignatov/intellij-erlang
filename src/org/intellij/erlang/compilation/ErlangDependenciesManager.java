package org.intellij.erlang.compilation;

import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.components.AbstractProjectComponent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupManager;

/**
 * @author savenko
 */
public class ErlangDependenciesManager extends AbstractProjectComponent {
  protected ErlangDependenciesManager(Project project) {
    super(project);
  }

  @Override
  public void initComponent() {
    StartupManager.getInstance(myProject).registerPostStartupActivity(new Runnable() {
      @Override
      public void run() {
        CompilerManager.getInstance(myProject).addBeforeTask(new ErlangPrepareDependenciesCompileTask());
      }
    });
  }
}