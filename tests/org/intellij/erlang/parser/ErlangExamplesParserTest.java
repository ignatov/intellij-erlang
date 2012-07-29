/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.FileSetTestCase;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.util.LocalTimeCounter;
import junit.framework.Assert;
import junit.framework.Test;
import org.intellij.erlang.ErlangFileType;
import org.jetbrains.annotations.NonNls;

/**
 * @author ignatov
 */
public class ErlangExamplesParserTest extends FileSetTestCase {
  @NonNls
  private static final String DATA_PATH = "/home/ignatov/src/erlang/testData/parser/examples/mnesia"; //PathManagerEx.getTestDataPath() + "testData/parser/examples";

  public ErlangExamplesParserTest() {
    super(DATA_PATH);
  }

  public static Test suite() {
    return new ErlangExamplesParserTest();
  }

  @Override
  public String transform(String testName, String[] data) throws Exception {
    final String fileText = data[0];

    final PsiFile psiFile = PsiFileFactory.getInstance(myProject)
      .createFileFromText("temp." + ErlangFileType.INSTANCE.getDefaultExtension(), ErlangFileType.INSTANCE,
        fileText, LocalTimeCounter.currentTime(), true);

    Assert.assertEquals(psiFile.getText(), fileText);
    return DebugUtil.psiToString(psiFile, false, false);
  }
}