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

package org.intellij.erlang.highlighting;

import com.intellij.codeInsight.daemon.DaemonAnalyzerTestCase;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.DebugUtil;
import org.intellij.erlang.console.ErlangConsoleView;
import org.intellij.erlang.inspection.ErlangUnboundVariableInspection;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangVarProcessor;

import java.io.File;
import java.util.HashMap;

public class ErlangConsoleViewTest extends DaemonAnalyzerTestCase {
  private ErlangConsoleView myView;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myView = new ErlangConsoleView(getProject());
    System.setProperty("SHOULD_PERFORM_ACCESS_CHECK", "false");
  }

  public void testConsoleResolve() throws Exception {
    PsiFile file = myView.getConsole().getFile();

    PsiDocumentManager instance = PsiDocumentManager.getInstance(getProject());
    final Document document = instance.getDocument(file);
    assert document != null;
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        WriteCommandAction.runWriteCommandAction(getProject(), new Runnable() {
          @Override
          public void run() {
            document.insertString(0, "C = A + B + <error>D</error>.");
          }
        });
      }
    });
    instance.commitDocument(document);
    HashMap<String, ErlangQVar> map = new HashMap<String, ErlangQVar>();
    map.put("A", (ErlangQVar) ErlangElementFactory.createQVarFromText(getProject(), "A"));
    map.put("B", (ErlangQVar) ErlangElementFactory.createQVarFromText(getProject(), "B"));
    file.putUserData(ErlangVarProcessor.ERLANG_VARIABLE_CONTEXT, map);
    myFile = file;
    myEditor = createEditor(file.getVirtualFile());
    assert myEditor != null;
    enableInspectionTool(new ErlangUnboundVariableInspection());
    doDoTest(true, false);
    String testName = getTestName(false);
    assertEquals(FileUtil.loadFile(new File(getTestDataPath() + testName + ".txt"), true), DebugUtil.psiToString(file, false));
  }

  @Override
  protected String getTestDataPath() {
    return "testData/console/";
  }

  @Override
  public void tearDown() throws Exception {
    Disposer.dispose(myView);
    super.tearDown();
  }
}
