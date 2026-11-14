/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.grammar.generation;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.JavaSdk;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ContentEntry;
import com.intellij.openapi.roots.LanguageLevelModuleExtension;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.JarFileSystem;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.java.LanguageLevel;
import com.intellij.psi.JavaPsiFacade;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

/**
 * Not a test: runs Grammar-Kit's "Generate Parser Code" on grammars/erlang.bnf the way the IDE does, so that
 * psiImplUtilClass, mixins and stub classes are read from the Java sources in src/ and gen/. The Gradle task
 * generateParserFromSources runs it with the Grammar-Kit plugin on the classpath; the test task excludes it.
 * <p>
 * In unit test mode Grammar-Kit defaults to the long local names (builder_, level_) instead of the short ones of the
 * IDE, hence names="short" in the bnf.
 */
public class GenerateParserFromSources extends LightJavaCodeInsightFixtureTestCase {
  private static final String PROJECT_DIR = System.getProperty("grammarKit.projectDir", "");
  private static final String OUTPUT_DIR = System.getProperty("grammarKit.outputDir", "");
  private static final String LIBRARIES = System.getProperty("grammarKit.libraries", "");

  @Override
  protected @NotNull LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return JavaSdk.getInstance().createJdk("JDK", System.getProperty("java.home"), false);
      }

      @Override
      public void configureModule(@NotNull Module module, @NotNull ModifiableRootModel model, @NotNull ContentEntry contentEntry) {
        // The module of the Gradle-imported project in the IDE: the source roots src/ and gen/ and the compile classpath.
        LocalFileSystem fileSystem = LocalFileSystem.getInstance();
        for (String path : new String[]{"src", "gen"}) {
          VirtualFile root = fileSystem.refreshAndFindFileByIoFile(new File(PROJECT_DIR, path));
          assertNotNull(path, root);
          // The VFS and the indices persist in the test sandbox between runs; pick up the files changed since then.
          VfsUtil.markDirtyAndRefresh(false, true, true, root);
          model.addContentEntry(root).addSourceFolder(root, false);
        }
        Library.ModifiableModel library = model.getModuleLibraryTable().createLibrary("compileClasspath").getModifiableModel();
        for (String path : LIBRARIES.split(File.pathSeparator)) {
          VirtualFile file = fileSystem.refreshAndFindFileByIoFile(new File(path));
          if (file == null) continue;
          library.addRoot(file.isDirectory() ? file : JarFileSystem.getInstance().getJarRootForLocalFile(file), OrderRootType.CLASSES);
        }
        library.commit();
        model.getModuleExtension(LanguageLevelModuleExtension.class).setLanguageLevel(LanguageLevel.HIGHEST);
      }
    };
  }

  public void testGenerateParser() throws Exception {
    assertFalse("grammarKit.projectDir is not set", PROJECT_DIR.isEmpty());
    assertFalse("grammarKit.outputDir is not set", OUTPUT_DIR.isEmpty());
    IndexingTestUtil.waitUntilIndexesAreReady(getProject());

    GlobalSearchScope scope = GlobalSearchScope.allScope(getProject());
    for (String className : new String[]{"java.lang.String", "com.intellij.psi.PsiElement",
      "org.intellij.erlang.psi.impl.ErlangPsiImplUtil", "org.intellij.erlang.psi.ErlangCompositeElement"}) {
      assertNotNull("not resolved from the project: " + className, JavaPsiFacade.getInstance(getProject()).findClass(className, scope));
    }

    VirtualFile grammar = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(new File(PROJECT_DIR, "grammars/erlang.bnf"));
    assertNotNull(grammar);
    VfsUtil.markDirtyAndRefresh(false, false, false, grammar);
    PsiFile bnfFile = PsiManager.getInstance(getProject()).findFile(grammar);
    assertNotNull(bnfFile);

    // Grammar-Kit is on the classpath of this task only, so it is called reflectively.
    ClassLoader loader = bnfFile.getClass().getClassLoader();
    Class<?> bnfFileClass = Class.forName("org.intellij.grammar.psi.BnfFile", true, loader);
    assertTrue("not a BnfFile: " + bnfFile.getClass(), bnfFileClass.isInstance(bnfFile));
    Class<?> javaHelperClass = Class.forName("org.intellij.grammar.java.JavaHelper", true, loader);
    Object javaHelper = getProject().getService(javaHelperClass);
    // The helper that reads Java sources (JavaHelper$PsiHelper, a top-level PsiHelper since Grammar-Kit 2023.3.3).
    // Without it Grammar-Kit falls back to reading compiled classes.
    String helper = javaHelper == null ? null : javaHelper.getClass().getName();
    assertTrue("Grammar-Kit does not read Java sources: " + helper, helper != null && helper.endsWith("PsiHelper"));

    Class<?> openerClass = Class.forName("org.intellij.grammar.generator.OutputOpener", true, loader);
    Class<?> generatorClass = Class.forName("org.intellij.grammar.generator.JavaParserGenerator", true, loader);
    // As GenerateAction.doGenerate: the directory of the grammar, the source root of parserClass and its package prefix.
    String sourcePath = FileUtil.toSystemDependentName(FileUtil.toCanonicalPath(grammar.getParent().getPath()));
    Object generator = generatorClass
      .getConstructor(bnfFileClass, String.class, String.class, String.class, openerClass)
      .newInstance(bnfFile, sourcePath, new File(OUTPUT_DIR).getAbsolutePath(), "", openerClass.getField("DEFAULT").get(null));
    try {
      generatorClass.getMethod("generate").invoke(generator);
    }
    catch (InvocationTargetException e) {
      throw (Exception)e.getCause();
    }
  }
}
