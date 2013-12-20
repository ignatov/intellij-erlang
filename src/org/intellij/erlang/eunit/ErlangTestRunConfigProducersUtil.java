package org.intellij.erlang.eunit;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.search.FilenameIndex;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class ErlangTestRunConfigProducersUtil {
  private ErlangTestRunConfigProducersUtil() {
  }

  public static boolean shouldProduceEunitTestRunConfiguration(@Nullable Project project, @Nullable Module module) {
    return !shouldProduceRebarTestRunConfiguration(project, module);
  }

  //TODO think of a more elegant way to learn if rebar is used in a module.
  public static boolean shouldProduceRebarTestRunConfiguration(@Nullable Project project, @Nullable Module module) {
    if (project == null || module == null) return false;
    if (StringUtil.isEmpty(RebarSettings.getInstance(project).getRebarPath())) return false;
    Collection<VirtualFile> configs = FilenameIndex.getVirtualFilesByName(project, "rebar.config", module.getModuleContentScope());
    return !configs.isEmpty();
  }
}
