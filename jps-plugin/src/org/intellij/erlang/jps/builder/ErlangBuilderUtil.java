package org.intellij.erlang.jps.builder;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangCompilerOptionsExtension;
import org.jdom.Document;
import org.jdom.JDOMException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.model.JpsProject;

import java.io.File;
import java.io.IOException;


public class ErlangBuilderUtil {
  public static final String BUILDER_DIRECTORY = "erlang-builder";
  public static final String BUILD_ORDER_FILE_NAME = "deps-tree.xml";

  static final Logger LOG = Logger.getInstance(ErlangBuilder.class);

  private ErlangBuilderUtil() {
  }

  @Nullable
  public static <T> T readFromXML(@NotNull CompileContext context, @NotNull String filename, @NotNull Class<T> tClass) {
    try {
      File xmlFile = getXmlFile(context, filename);
      if (!xmlFile.exists()) return null;

      Document document = JDOMUtil.loadDocument(xmlFile);
      return XmlSerializer.deserialize(document, tClass);
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
  public static File getXmlFile(@NotNull CompileContext context, @NotNull String filename) {
    File dataStorageRoot = context.getProjectDescriptor().dataManager.getDataPaths().getDataStorageRoot();
    File parentDirectory = new File(dataStorageRoot, BUILDER_DIRECTORY);
    return new File(parentDirectory, filename);
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
