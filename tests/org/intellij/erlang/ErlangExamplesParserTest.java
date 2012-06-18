package org.intellij.erlang;

import com.intellij.FileSetTestCase;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.application.ex.PathManagerEx;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.util.LocalTimeCounter;
import junit.framework.Assert;
import junit.framework.Test;
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