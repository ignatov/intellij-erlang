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

package org.intellij.erlang.parser;

import com.intellij.core.CoreApplicationEnvironment;
import com.intellij.lang.Language;
import com.intellij.lang.LanguageExtensionPoint;
import com.intellij.lang.LanguageParserDefinitions;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.fileEditor.impl.LoadTextUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.psi.*;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.impl.PsiFileFactoryImpl;
import com.intellij.testFramework.LightPlatformCodeInsightTestCase;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.testFramework.TestDataFile;
import com.intellij.testFramework.UsefulTestCase;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangParserDefinition;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.util.Set;

/**
 * initialization and testing logic copied from com.intellij.testFramework.ParsingTestCase
 */
public abstract class ErlangParserTestBase extends LightPlatformCodeInsightTestCase {
  private final boolean myOverrideTestData;

  protected String myFileExt;
  @NonNls protected final String myFullDataPath;
  protected PsiFile myFile;

  public ErlangParserTestBase(String dataPath, String fileExt, boolean overrideTestData) {
    myOverrideTestData = overrideTestData;
    myFullDataPath = getTestDataPath() + "/" + dataPath;
    myFileExt = fileExt;
    System.setProperty("idea.platform.prefix", "Idea");
  }

  @NotNull
  protected String getTestDataPath() {
    return "testData";
  }

  protected void doTest(boolean checkResult, boolean suppressErrors) {
    if (myOverrideTestData) {
      doOverrideTestData();
    }
    doTest(true);
    if (!suppressErrors) {
      assertFalse("PsiFile contains error elements", toParseTreeText(myFile, true, false).contains("PsiErrorElement"));
    }
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    CoreApplicationEnvironment.registerExtensionPoint(Extensions.getRootArea(), "com.intellij.lang.braceMatcher", LanguageExtensionPoint.class);
    LanguageParserDefinitions.INSTANCE.addExplicitExtension(ErlangLanguage.INSTANCE, new ErlangParserDefinition());
  }

  @Override
  protected void tearDown() throws Exception {
    super.tearDown();
    myFile = null;
  }

  private void doOverrideTestData() {
    try {
      String testName = getTestName(false);
      String text = loadFile(testName + "." + myFileExt);
      myFile = createPsiFile(testName, text);
      ensureParsed(myFile);
      FileUtil.writeToFile(new File(myFullDataPath + "/" + testName + ".txt"), toParseTreeText(myFile, true, false));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  protected void doTest(boolean checkResult) {
    String name = getTestName(false);
    try {
      String text = loadFile(name + "." + myFileExt);
      myFile = createPsiFile(name, text);
      ensureParsed(myFile);
      assertEquals("light virtual file text mismatch", text, ((LightVirtualFile)myFile.getVirtualFile()).getContent().toString());
      assertEquals("virtual file text mismatch", text, LoadTextUtil.loadText(myFile.getVirtualFile()));
      assertEquals("doc text mismatch", text, myFile.getViewProvider().getDocument().getText());
      assertEquals("psi text mismatch", text, myFile.getText());
      if (checkResult){
        checkResult(name, myFile);
      }
      else{
        toParseTreeText(myFile, true, false);
      }
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  protected String loadFile(@NonNls @TestDataFile String name) throws IOException {
    return doLoadFile(myFullDataPath, name);
  }

  private static String doLoadFile(String myFullDataPath, String name) throws IOException {
    return FileUtil.loadFile(new File(myFullDataPath, name), CharsetToolkit.UTF8, true).trim();
  }

  protected PsiFile createPsiFile(String name, String text) {
    LightVirtualFile virtualFile = new LightVirtualFile(name + "." + myFileExt, ErlangLanguage.INSTANCE, text);
    virtualFile.setCharset(CharsetToolkit.UTF8_CHARSET);
    return createFile(virtualFile);
  }

  protected static PsiFile createFile(LightVirtualFile virtualFile) {
    PsiFileFactory psiFileFactory = PsiFileFactory.getInstance(getProject());
    assert psiFileFactory instanceof PsiFileFactoryImpl;
    //noinspection ConstantConditions
    return ((PsiFileFactoryImpl) psiFileFactory).trySetupPsiForFile(virtualFile, ErlangLanguage.INSTANCE, true, false);
  }

  protected static String toParseTreeText(final PsiElement file,  boolean skipSpaces, boolean printRanges) {
    return DebugUtil.psiToString(file, skipSpaces, printRanges);
  }

  protected void checkResult(@NonNls @TestDataFile String targetDataName, final PsiFile file) throws IOException {
    doCheckResult(myFullDataPath, file, true, targetDataName, true, false);
  }

  public static void doCheckResult(String myFullDataPath,
                                   PsiFile file,
                                   boolean checkAllPsiRoots,
                                   String targetDataName,
                                   boolean skipSpaces,
                                   boolean printRanges) throws IOException {
    FileViewProvider provider = file.getViewProvider();
    Set<Language> languages = provider.getLanguages();

    if (!checkAllPsiRoots || languages.size() == 1) {
      doCheckResult(myFullDataPath, targetDataName + ".txt", toParseTreeText(file, skipSpaces, printRanges).trim());
      return;
    }

    for (Language language : languages) {
      PsiFile root = provider.getPsi(language);
      String expectedName = targetDataName + "." + language.getID() + ".txt";
      doCheckResult(myFullDataPath, expectedName, toParseTreeText(root, skipSpaces, printRanges).trim());
    }
  }

  public static void doCheckResult(String fullPath, String targetDataName, String text) throws IOException {
    String expectedFileName = fullPath + File.separatorChar + targetDataName;
    UsefulTestCase.assertSameLinesWithFile(expectedFileName, text);
  }

  public static void ensureParsed(PsiFile file) {
    file.accept(new PsiElementVisitor() {
      @Override
      public void visitElement(PsiElement element) {
        element.acceptChildren(this);
      }
    });
  }
}
