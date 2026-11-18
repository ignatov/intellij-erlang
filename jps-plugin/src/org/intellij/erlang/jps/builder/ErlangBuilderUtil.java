package org.intellij.erlang.jps.builder;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangCompilerOptionsExtension;
import org.jdom.JDOMException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.model.JpsProject;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;


public class ErlangBuilderUtil {
  public static final String BUILDER_DIRECTORY = "erlang-builder";
  public static final String BUILD_ORDER_FILE_NAME = "deps-tree.xml";

  static final Logger LOG = Logger.getInstance(ErlangBuilder.class);

  private ErlangBuilderUtil() {
  }

  @Nullable
  public static <T> T readFromXML(@NotNull CompileContext context, @NotNull String filename, @NotNull Class<T> tClass) {
    try {
      Path xmlFile = getXmlFile(context, filename);
      if (!Files.exists(xmlFile)) return null;

      return XmlSerializer.deserialize(JDOMUtil.load(xmlFile), tClass);
    }
    catch (JDOMException e) {
      LOG.error("Can't read XML from " + filename, e);
    }
    catch (IOException e) {
      LOG.warn("Can't read " + filename, e);
    }
    return null;
  }

  @NotNull
  private static Path getXmlFile(@NotNull CompileContext context, @NotNull String filename) {
    Path dataStorageDir = context.getProjectDescriptor().dataManager.getDataPaths().getDataStorageDir();
    return dataStorageDir.resolve(BUILDER_DIRECTORY).resolve(filename);
  }

  public static boolean isSource(@NotNull String fileName) {
    return fileName.endsWith(".erl");
  }

  public static boolean isAppConfigFileName(@NotNull String fileName) {
    return fileName.endsWith(".app") || fileName.endsWith(".app.src");
  }

  public static boolean isHeader(@NotNull String fileName) {
    return fileName.endsWith(".hrl");
  }

  @NotNull
  public static ErlangCompilerOptions getCompilerOptions(@NotNull JpsProject project) {
    JpsErlangCompilerOptionsExtension extension = project.getContainer().getChild(JpsErlangCompilerOptionsExtension.ROLE);
    ErlangCompilerOptions options = extension != null ? extension.getOptions() : null;
    return options != null ? options : new ErlangCompilerOptions();
  }

  @NotNull
  public static String getPath(@NotNull File file) {
    return file.getAbsolutePath();
  }
}
