package org.intellij.erlang.configuration;

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangCompilerOptionsSerializer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
  name = JpsErlangCompilerOptionsSerializer.COMPILER_OPTIONS_COMPONENT_NAME,
  storages = {
    @Storage(file = StoragePathMacros.PROJECT_FILE),
    @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/compiler.xml", scheme = StorageScheme.DIRECTORY_BASED)
  }
)
public class ErlangCompilerSettings implements PersistentStateComponent<ErlangCompilerOptions> {
  private ErlangCompilerOptions myCompilerOptions = new ErlangCompilerOptions();

  @Nullable
  @Override
  public ErlangCompilerOptions getState() {
    return myCompilerOptions;
  }

  @Override
  public void loadState(ErlangCompilerOptions state) {
    myCompilerOptions = state;
  }

  public boolean isUseRebarCompilerEnabled() {
    return myCompilerOptions.myUseRebarCompiler;
  }

  public void setUseRebarCompilerEnabled(boolean useRebarCompiler) {
    myCompilerOptions.myUseRebarCompiler = useRebarCompiler;
  }

  public boolean isAddDebugInfoEnabled() {
    return myCompilerOptions.myAddDebugInfoEnabled;
  }

  public void setAddDebugInfoEnabled(boolean useDebugInfo) {
    myCompilerOptions.myAddDebugInfoEnabled = useDebugInfo;
  }

  @NotNull
  public static ErlangCompilerSettings getInstance(@NotNull Project project) {
    ErlangCompilerSettings persisted = ServiceManager.getService(project, ErlangCompilerSettings.class);
    return persisted != null ? persisted : new ErlangCompilerSettings();
  }
}
