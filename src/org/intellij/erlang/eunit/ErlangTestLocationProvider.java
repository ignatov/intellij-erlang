package org.intellij.erlang.eunit;

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testIntegration.TestLocationProvider;
import com.intellij.util.SmartList;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author ignatov
 */
public class ErlangTestLocationProvider implements TestLocationProvider {
  private static final Pattern METHOD_PATTERN = Pattern.compile("(\\w+):(\\w+)");

  @NotNull
  @Override
  public List<Location> getLocation(@NotNull String protocolId, @NotNull String locationData, Project project) {
    if (!ErlangUnitRunConfigurationType.PROTOCOL.equals(protocolId)) return Collections.emptyList();

    SmartList<Location> list = new SmartList<Location>();

    Matcher matcher = METHOD_PATTERN.matcher(locationData);
    Collection<ErlangFunction> result = new ArrayList<ErlangFunction>();
    if (matcher.matches()) {
      String module = matcher.group(1);
      String function = matcher.group(2);

      PsiFile[] files = FilenameIndex.getFilesByName(project, module + "." + ErlangFileType.MODULE.getDefaultExtension(),
        GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.allScope(project), ErlangFileType.MODULE));
      for (PsiFile file : files) {
        if (file instanceof ErlangFile) {
          result.addAll(((ErlangFile) file).getFunctionsByName(function));
        }
      }

      for (ErlangFunction each : result) {
        list.add(new PsiLocation<PsiElement>(project, each));
      }
    }
    return list;
  }
}