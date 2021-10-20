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

import com.intellij.openapi.fileTypes.ExtensionFileNameMatcher;
import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.icons.ErlangIcons;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.fileTypes.FileNameMatcherFactory;

import javax.swing.*;
import java.util.List;

public abstract class ErlangFileType extends LanguageFileType {
  public static final ErlangFileType MODULE = new ModuleFileType();
  public static final ErlangFileType HEADER = new HrlFileType();
  public static final ErlangFileType APP = new AppFileType();
  public static final ErlangFileType TERMS = new ErlangTermsFileType();
  public static final List<ErlangFileType> TYPES = ContainerUtil.immutableList(MODULE, HEADER, APP, TERMS);

  private final String myName;
  private final String myDescription;
  private final Icon myIcon;
  private final List<String> myExtensions;

  private ErlangFileType(
    @NotNull String name,
    @NotNull String description,
    @NotNull Icon icon,
    @NotNull String... extensions) {
    super(ErlangLanguage.INSTANCE);

    myName = name;
    myDescription = description;
    myIcon = icon;
    myExtensions = ContainerUtil.immutableList(extensions);
  }

  @NotNull
  @Override
  public final String getName() {
    return myName;
  }

  @Override
  public @Nls @NotNull String getDisplayName() {
    return myName;
  }

  @NotNull
  @Override
  public final String getDescription() {
    return myDescription;
  }

  @Nullable
  @Override
  public final Icon getIcon() {
    return myIcon;
  }

  @NotNull
  @Override
  public final String getDefaultExtension() {
    return myExtensions.get(0);
  }

  @NotNull
  public final List<String> getDefaultExtensions() {
    return myExtensions;
  }

  public void register(@NotNull FileTypeConsumer registrar) {
    registrar.consume(this, StringUtil.join(myExtensions, FileTypeConsumer.EXTENSION_DELIMITER));
  }


  private static class ModuleFileType extends ErlangFileType {
    private ModuleFileType() {
      super("Erlang",
            "Erlang",
            ErlangIcons.FILE,
            "erl");
    }
  }

  private static class HrlFileType extends ErlangFileType {
    private HrlFileType() {
      super("Erlang Header",
            "Erlang/OTP Header File",
            ErlangIcons.HEADER,
            "hrl");
    }
  }

  private static class AppFileType extends ErlangFileType {
    private static final String APP = "app";
    private static final String APP_SRC = "app.src";

    private AppFileType() {
      super("Erlang/OTP app",
            "Erlang/OTP Application Resource File",
            ErlangIcons.OTP_APP_RESOURCE,
            APP,
            APP_SRC);
    }

    @Override
    public void register(@NotNull FileTypeConsumer registrar) {
      // if we feed "app.src" as an extension, it may get compared to extension part of file name and won't match.
      registrar.consume(this,
                        new ExtensionFileNameMatcher(APP),
                        FileNameMatcherFactory.getInstance().createMatcher("*." + APP_SRC));
    }
  }

  private static class ErlangTermsFileType extends ErlangFileType {
    private ErlangTermsFileType() {
      super("Erlang Terms",
            "Erlang Terms File",
            ErlangIcons.TERMS,
            "config",
            "routes",
            "rel");
    }
  }
}
