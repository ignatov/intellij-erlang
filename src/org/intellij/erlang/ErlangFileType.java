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

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

/**
 * @author ignatov
 */
public class ErlangFileType extends LanguageFileType {
  public static ErlangFileType MODULE = new ErlangFileType();
  public static HrlFileType HEADER = new HrlFileType();
  public static AppFileType APP = new AppFileType();
  public static ErlangTermsFileType TERMS = new ErlangTermsFileType();

  protected ErlangFileType() {
    super(ErlangLanguage.INSTANCE);
  }

  @NotNull
  @Override
  public String getName() {
    return "Erlang";
  }

  @NotNull
  @Override
  public String getDescription() {
    return "Erlang";
  }

  @NotNull
  @Override
  public String getDefaultExtension() {
    return "erl";
  }

  @Override
  public Icon getIcon() {
    return ErlangIcons.FILE;
  }

  @Override
  public boolean isJVMDebuggingSupported() {
    // turn off for now
    return false;
  }

  public static class HrlFileType extends ErlangFileType {
    @NotNull
    @Override
    public String getName() {
      return "Erlang Header";
    }

    @NotNull
    @Override
    public String getDescription() {
      return "Erlang/OTP Header File";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
      return "hrl";
    }

    @Override
    public Icon getIcon() {
      return ErlangIcons.HEADER;
    }
  }

  public static class AppFileType extends ErlangFileType {
    @NotNull
    @Override
    public String getName() {
      return "Erlang/OTP app";
    }

    @NotNull
    @Override
    public String getDescription() {
      return "Erlang/OTP Application Resource File";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
      return "app";
    }

    @Override
    public Icon getIcon() {
      return ErlangIcons.OTP_APP_RESOURCE;
    }
  }

  public static class ErlangTermsFileType extends ErlangFileType {
    @NotNull
    @Override
    public String getName() {
      return "Erlang Terms";
    }

    @NotNull
    @Override
    public String getDescription() {
      return "Erlang Terms File";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
      return "config";
    }

    @Override
    public Icon getIcon() {
      return ErlangIcons.TERMS;
    }
  }

  @Nullable
  public static ErlangFileType getFileType(String filename) {
    if (filename == null) return null;
    if (filename.endsWith(MODULE.getDefaultExtension())) return MODULE;
    if (filename.endsWith(HEADER.getDefaultExtension())) return HEADER;
    if (filename.endsWith(APP.getDefaultExtension())) return APP;
    if (filename.endsWith(TERMS.getDefaultExtension())) return TERMS;
    return null;
  }

  @Nullable
  public static Icon getIconForFile(String filename) {
    ErlangFileType fileType = getFileType(filename);
    return fileType == null ? null : fileType.getIcon();
  }
}
