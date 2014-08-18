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

package org.intellij.erlang;

import com.intellij.openapi.fileTypes.FileNameMatcher;
import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class ErlangFileTypeFactory extends FileTypeFactory {
  private static final class ExtensionFileNameMatcher implements FileNameMatcher {
    private final String myDotExtension;

    private ExtensionFileNameMatcher(@NonNls @NotNull String extension) {
      myDotExtension = "." + extension;
    }

    @Override
    public boolean accept(@NonNls @NotNull String fileName) {
      return fileName.endsWith(myDotExtension);
    }

    @NotNull
    @Override
    public String getPresentableString() {
      return "*" + myDotExtension;
    }
  }

  @Override
  public void createFileTypes(@NotNull FileTypeConsumer fileTypeConsumer) {
    fileTypeConsumer.consume(ErlangFileType.MODULE);
    fileTypeConsumer.consume(ErlangFileType.HEADER);
    fileTypeConsumer.consume(ErlangFileType.APP,
      new ExtensionFileNameMatcher(ErlangFileType.APP.getDefaultExtension()),
      new ExtensionFileNameMatcher(ErlangFileType.APP.getDefaultExtension() + ".src"));
    fileTypeConsumer.consume(ErlangFileType.TERMS, 
      new ExtensionFileNameMatcher("routes"), 
      new ExtensionFileNameMatcher("config"),
      new ExtensionFileNameMatcher("rel")
    );
  }
}
