/*
 * Copyright 2012-2013 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.dialyzer;

import com.intellij.codeInsight.daemon.HighlightDisplayKey;
import com.intellij.codeInspection.InspectionProfile;
import com.intellij.codeInspection.ex.DisableInspectionToolAction;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.profile.codeInspection.InspectionProjectProfileManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangDialyzerExternalAnnotator extends ExternalAnnotator<ErlangDialyzerExternalAnnotator.State, ErlangDialyzerExternalAnnotator.State> {
  private final static Logger LOG = Logger.getInstance(ErlangDialyzerExternalAnnotator.class);
  
  @Nullable
  private static Problem parseProblem(String input) {
    List<String> split = StringUtil.split(input, ":");
    if (split.size() < 3) return null;
    int line = StringUtil.parseInt(split.get(1), 0);
    return new Problem(line, StringUtil.join(split.subList(2, split.size()), ":"));
  }

  @Nullable
  @Override
  public State collectionInformation(@NotNull PsiFile file) {
    VirtualFile vFile = file.getVirtualFile();
    if (vFile == null || vFile.getFileType() != ErlangFileType.MODULE) return null;
    String canonicalPath = vFile.getCanonicalPath();
    if (canonicalPath == null) return null;
    Module module = ModuleUtilCore.findModuleForPsiElement(file);
    if (module == null) return null;
    final Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
    if (sdk == null) return null;
    final String homePath = sdk.getHomePath();
    if (homePath == null) return null;

    final InspectionProfile profile = InspectionProjectProfileManager.getInstance(file.getProject()).getInspectionProfile();
    final HighlightDisplayKey key = HighlightDisplayKey.find(ErlangDialyzerInspection.INSPECTION_SHORT_NAME);
    if (!profile.isToolEnabled(key)) return null;

    String workingDir = file.getProject().getBasePath();
    String dialyzerPath = homePath + "/bin/dialyzer";

    String currentPltPath = DialyzerSettings.getInstance(file.getProject()).getCurrentPltPath();

    return new State(dialyzerPath, currentPltPath, canonicalPath, workingDir);
  }

  @Nullable
  @Override
  public State doAnnotate(State state) {
    if (state == null) return null;

    ProcessOutput output = null;
    try {
      String[] params = StringUtil.isEmptyOrSpaces(state.myCurrentPltPath) ? new String[]{state.myFilePath} : new String[]{"--plt", state.myCurrentPltPath, state.myFilePath};
      output = ErlangSystemUtil.getProcessOutput(state.myWorkingDir, state.myDialyzerPath, params);
    } catch (ExecutionException e) {
      LOG.debug(e);
    }
    if (output != null) {
      if (output.getStderrLines().isEmpty()) {
        for (String line : output.getStdoutLines()) {
          final Problem problem = parseProblem(line);
          LOG.debug(line);
          LOG.debug(problem != null ? problem.toString() : null);
          ContainerUtil.addAllNotNull(state.problems, problem);
        }
      }
    }
    return state;
  }

  @Override
  public void apply(@NotNull PsiFile file, State annotationResult, @NotNull AnnotationHolder holder) {
    if (annotationResult == null || !file.isValid()) return;
    final String text = file.getText();
    for (Problem problem : annotationResult.problems) {
      int offset = StringUtil.lineColToOffset(text, problem.myLine - 1, 0);

      if (offset == -1) continue;

      int width = 0;
      while (offset + width < text.length() && !StringUtil.isLineBreak(text.charAt(offset + width))) width++;

      TextRange problemRange = TextRange.create(offset, offset + width);
      final String message = "Dialyzer: " + problem.myDescription;
      final Annotation annotation = holder.createWarningAnnotation(problemRange, message);
      HighlightDisplayKey key = HighlightDisplayKey.find(ErlangDialyzerInspection.INSPECTION_SHORT_NAME);
      annotation.registerFix(new DisableInspectionToolAction(key) {
        @NotNull
        @Override
        public String getText() {
          return "Disable 'Dialyzer-based inspections'";
        }
      });
    }
  }

  public static class Problem {
    private final int myLine;
    private final String myDescription;

    public Problem(int line, String description) {
      myLine = line;
      myDescription = description;
    }

    @Override
    public String toString() {
      return "Problem{" +
        "myLine=" + myLine +
        ", myDescription='" + myDescription + '\'' +
        '}';
    }
  }

  public static class State {
    public final List<Problem> problems = new ArrayList<Problem>();
    private final String myDialyzerPath;
    private final String myCurrentPltPath;
    private final String myFilePath;
    private final String myWorkingDir;

    public State(String dialyzerPath, String currentPltPath, String filePath, String workingDir) {
      myDialyzerPath = dialyzerPath;
      myCurrentPltPath = currentPltPath;
      myFilePath = filePath;
      myWorkingDir = workingDir;
    }
  }
}
