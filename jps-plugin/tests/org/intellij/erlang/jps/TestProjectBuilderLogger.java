/*
 * Copyright 2012-2014 Sergey Ignatov
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
package org.intellij.erlang.jps;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.UsefulTestCase;
import com.intellij.util.containers.CollectionFactory;
import com.intellij.util.containers.MultiMap;
import org.jetbrains.jps.builders.impl.logging.ProjectBuilderLoggerBase;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class TestProjectBuilderLogger extends ProjectBuilderLoggerBase {
  private final MultiMap<String, File> myCompiledFiles = new MultiMap<>();
  private final Set<String> myDeletedFiles = CollectionFactory.createFilePathSet();
  @Override
  public void logDeletedFiles(Collection<String> paths) {
    myDeletedFiles.addAll(paths);
  }

  @Override
  public void logCompiledFiles(Collection<File> files, String builderName, String description) {
    myCompiledFiles.putValues(builderName, files);
  }

  public void clear() {
    myCompiledFiles.clear();
    myDeletedFiles.clear();
  }

  public void assertCompiled(String builderName, File[] baseDirs, String... paths) {
    List<String> filePaths = new ArrayList<>();
    myCompiledFiles.get(builderName).stream()
      .map(File::getAbsolutePath)
      .forEach(filePaths::add);
    assertRelativePaths(baseDirs, filePaths, paths);
  }

  public void assertDeleted(File[] baseDirs, String... paths) {
    assertRelativePaths(baseDirs, myDeletedFiles, paths);
  }

  private static void assertRelativePaths(File[] baseDirs, Collection<String> files, String[] expected) {
    List<String> relativePaths = new ArrayList<>();
    for (String filePath : files) {
      File file = new File(filePath);
      String path = file.getAbsolutePath();
      for (File baseDir : baseDirs) {
        if (baseDir != null && FileUtil.isAncestor(baseDir, file, false)) {
          path = FileUtil.getRelativePath(baseDir, file);
          break;
        }
      }
      relativePaths.add(FileUtil.toSystemIndependentName(path));
    }
    UsefulTestCase.assertSameElements(relativePaths, expected);
  }

  @Override
  protected void logLine(String message) {
  }

  @Override
  public boolean isEnabled() {
    return true;
  }
}
