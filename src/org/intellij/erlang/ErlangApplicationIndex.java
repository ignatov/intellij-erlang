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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Processor;
import com.intellij.util.indexing.*;
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
public class ErlangApplicationIndex extends ScalarIndexExtension<String> {
  public static final ID<String, Void> ERLANG_APPLICAION_INDEX = ID.create("ErlangApplicationIndex");

  private static final FileBasedIndex.InputFilter INPUT_FILTER = new ErlangApplicationInputFilter();
  private static final int INDEX_VERSION = 0;
  private static final KeyDescriptor<String> KEY_DESCRIPTOR = new EnumeratorStringDescriptor();
  private static final DataIndexer<String, Void, FileContent> DATA_INDEXER = new ErlangApplicationDataIndexer();
  private static final String APP_SRC = ".app.src";

  @NotNull
  @Override
  public ID<String, Void> getName() {
    return ERLANG_APPLICAION_INDEX;
  }

  @NotNull
  @Override
  public DataIndexer<String, Void, FileContent> getIndexer() {
    return DATA_INDEXER;
  }

  @Override
  public KeyDescriptor<String> getKeyDescriptor() {
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

  @Nullable
  public static VirtualFile getApplicationDirectoryByName(@NotNull String appName, @NotNull GlobalSearchScope searchScope) {
    ApplicationPathExtractingProcessor processor = new ApplicationPathExtractingProcessor();
    FileBasedIndex.getInstance().processValues(ERLANG_APPLICAION_INDEX, appName, null, processor, searchScope);
    return processor.getApplicationPath();
  }

  public static List<VirtualFile> getAllApplicationDirectories(@NotNull Project project, @NotNull final GlobalSearchScope searchScope) {
    final ArrayList<VirtualFile> result = new ArrayList<VirtualFile>();
    final FileBasedIndex index = FileBasedIndex.getInstance();

    index.processAllKeys(ERLANG_APPLICAION_INDEX, new Processor<String>() {
      @Override
      public boolean process(String appName) {
        ApplicationPathExtractingProcessor processor = new ApplicationPathExtractingProcessor();
        index.processValues(ERLANG_APPLICAION_INDEX, appName, null, processor, searchScope);
        result.add(processor.getApplicationPath());
        return true;
      }
    }, project);

    return result;
  }

  @Nullable
  private static VirtualFile getLibraryDirectory(VirtualFile appFile) {
    String libName = getApplicationName(appFile);
    VirtualFile parent = appFile.getParent();
    VirtualFile grandParent = parent != null ? parent.getParent() : null;

    if (isAppDirectory(parent, libName)) return parent;
    if (isAppDirectory(grandParent, libName)) return grandParent;
    return null;
  }

  private static boolean isAppDirectory(@Nullable VirtualFile directory, String appName) {
    String dirName = directory != null ? directory.getName() : null;
    return directory != null && (dirName.equals(appName) || dirName.startsWith(appName + "-"));
  }

  private static class ApplicationPathExtractingProcessor implements FileBasedIndex.ValueProcessor<Void> {
    private VirtualFile myPath = null;

    @Override
    public boolean process(VirtualFile appFile, Void value) {
      VirtualFile libDir = getLibraryDirectory(appFile);
      if (libDir == null) return true;
      String appName = getApplicationName(appFile);
      //applications with no version specification have higher priority
      if (myPath == null || appName.equals(libDir.getName())) {
        myPath = libDir;
        return true;
      }
      if (appName.equals(myPath.getName())) return true;
      myPath = myPath.getName().compareTo(libDir.getName()) < 0 ? libDir : myPath;
      return true;
    }

    public VirtualFile getApplicationPath() {
      return myPath;
    }
  }

  private static class ErlangApplicationInputFilter implements FileBasedIndex.InputFilter {
    @Override
    public boolean acceptInput(VirtualFile file) {
      return file != null && file.getFileType() == ErlangFileType.APP;
    }
  }

  private static class ErlangApplicationDataIndexer implements DataIndexer<String, Void, FileContent> {
    @NotNull
    @Override
    public Map<String, Void> map(FileContent inputData) {
      return Collections.singletonMap(getApplicationName(inputData.getFile()), null);
    }
  }

  @NotNull
  private static String getApplicationName(VirtualFile appFile) {
    String filename = appFile.getName();
    return filename.endsWith(APP_SRC) ? StringUtil.trimEnd(filename, APP_SRC) : appFile.getNameWithoutExtension();
  }
}
