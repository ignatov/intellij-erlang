package org.intellij.erlang;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.intellij.erlang.highlighting.ErlangHighlightingTest;
import org.intellij.erlang.parser.ErlangParserTest;

/**
 * @author ignatov
 */
@SuppressWarnings("ALL")
public class ErlangTestCase extends TestCase {
  public static TestSuite suite() {
    TestSuite suite = new TestSuite();
    suite.addTestSuite(ErlangParserTest.class);
    suite.addTestSuite(ErlangHighlightingTest.class);
    return suite;
  }
}
