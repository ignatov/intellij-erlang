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

package org.intellij.erlang;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Processor;
import com.intellij.util.indexing.*;
import com.intellij.util.io.DataExternalizer;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.KeyDescriptor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author savenko
 */
public class ErlangApplicationIndex extends FileBasedIndexExtension<String, String> {
  public static final ID<String, String> ERLANG_APPLICAION_INDEX = ID.create("ErlangApplicationIndex");

  private static final FileBasedIndex.InputFilter INPUT_FILTER = new ErlangApplicationInputFilter();
  private static final int INDEX_VERSION = 0;
  private static final KeyDescriptor<String> KEY_DESCRIPTOR = new EnumeratorStringDescriptor();
  private static final DataIndexer<String, String, FileContent> DATA_INDEXER = new ErlangApplicationDataIndexer();

  @NotNull
  @Override
  public ID<String, String> getName() {
    return ERLANG_APPLICAION_INDEX;
  }

  @NotNull
  @Override
  public DataIndexer<String, String, FileContent> getIndexer() {
    return DATA_INDEXER;
  }

  @Override
  public KeyDescriptor<String> getKeyDescriptor() {
    return KEY_DESCRIPTOR;
  }

  @Override
  public DataExternalizer<String> getValueExternalizer() {
    return KEY_DESCRIPTOR;
  }

  @Override
  public FileBasedIndex.InputFilter getInputFilter() {
    return INPUT_FILTER;
  }

  @Override
  public boolean dependsOnFileContent() {
    return false;
  }

  @Override
  public int getVersion() {
    return INDEX_VERSION;
  }

  @NotNull
  public static List<String> getApplicationPathsByName(@NotNull String appName, @NotNull GlobalSearchScope searchScope) {
    return FileBasedIndex.getInstance().getValues(ERLANG_APPLICAION_INDEX, appName, searchScope);
  }

  public static List<String> getAllApplicationPaths(@NotNull final Project project, @NotNull final GlobalSearchScope searchScope) {
    final ArrayList<String> result = new ArrayList<String>();

    FileBasedIndex.getInstance().processAllKeys(ERLANG_APPLICAION_INDEX, new Processor<String>() {
      @Override
      public boolean process(String s) {
        FileBasedIndex.getInstance().processValues(ERLANG_APPLICAION_INDEX, s, null, new FileBasedIndex.ValueProcessor<String>() {
          @Override
          public boolean process(VirtualFile file, String value) {
            result.add(value);
            return true;
          }
        }, searchScope);
        return true;
      }
    }, project);

    return result;
  }

  private static class ErlangApplicationInputFilter implements FileBasedIndex.InputFilter {
    @Override
    public boolean acceptInput(VirtualFile file) {
      return file != null && file.getFileType() == ErlangFileType.APP;
    }
  }

  private static class ErlangApplicationDataIndexer implements DataIndexer<String, String, FileContent> {
    @NotNull
    @Override
    public Map<String, String> map(FileContent inputData) {
      String libDirPath = getLibraryDirectoryPath(inputData.getFile());

      return libDirPath == null ? Collections.<String, String>emptyMap() : Collections.singletonMap(inputData.getFile().getNameWithoutExtension(), libDirPath);
    }

    @Nullable
    private static String getLibraryDirectoryPath(VirtualFile appFile) {
      String libName = appFile.getNameWithoutExtension();
      VirtualFile parent = appFile.getParent();
      VirtualFile libDir = parent != null ? parent.getParent() : null;
      String libDirName = libDir != null ? libDir.getName() : null;

      if (parent == null || !"ebin".equals(parent.getName())) return null;
      if (libDirName == null || !(libDirName.length() == libName.length() ? libDirName.equals(libName) : libDirName.startsWith(libName + "-"))) return null;

      return libDir.getPath();
    }
  }
}
