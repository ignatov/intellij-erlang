package org.intellij.erlang.highlighting;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * D1: errors inside the body of a -define. For epp a macro body is any sequence of tokens up to the closing ")." of a
 * well-formed -define, so a parser error inside such a body is false. Each case is a module with the define and an
 * ordinary function after it; erlc 29.1 accepts every "ok" case and rejects every "err" case (column 3). A valid case
 * must show no error and keep the function after the define; a wrong case must show an error on the define's line.
 * Uses only master's classes (the macro layer is switched on by reflection when it exists), so it also runs on master.
 */
public class ErlangDefineBodyErrorsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String[][] CASES = {
    {"o01", "-define(PAR, ()).", "ok"},
    {"o02", "-define(ARGS(X), (X, 1)).", "ok"},
    {"o03", "-define(TAIL(X), , X]).", "ok"},
    {"o04", "-define(BEGIN, begin).", "ok"},
    {"o05", "-define(END, end).", "ok"},
    {"o06", "-define(HEAD(N), sign(N) when N > 0 ->).", "ok"},
    {"o07", "-define(AND_SMALL(N), , N < 100).", "ok"},
    {"o08", "-define(PLUS_ONE, 1 +).", "ok"},
    {"o09", "-define(ID(X), X).", "ok"},
    {"o10", "-define(LISTS, lists:).", "ok"},
    {"o11", "-define(CASE(E), case E of).", "ok"},
    {"o12", "-define(OPEN, [).", "ok"},
    {"o13", "-define(CLOSE, ]).", "ok"},
    {"o14", "-define(ARROW, ->).", "ok"},
    {"s15", "-define(UNB_PAR, (1).", "ok"},
    {"s16", "-define(UNB_CURLY, {a).", "ok"},
    {"s17", "-define(UNB_BIN, <<1).", "ok"},
    {"s18", "-define(CLOSE_PAR, )).", "ok"},
    {"s19", "-define(CLOSE_CURLY, }).", "ok"},
    {"s20", "-define(CLOSE_BIN, >>).", "ok"},
    {"s21", "-define(OF, of).", "ok"},
    {"s22", "-define(WHEN, when).", "ok"},
    {"s23", "-define(SEMI, ;).", "ok"},
    {"s24", "-define(COMMA, ,).", "ok"},
    {"s25", "-define(STR, \"a). b\").", "ok"},
    {"s26", "-define(CMT, 1 + % close ).\\n  2).", "ok"},
    {"s27", "-define(ML(X),\\n  case X of\\n    a -> 1;\\n    _ -> 2\\n  end).", "ok"},
    {"s28", "-define(ML2, foo(\\n  1,).", "ok"},
    {"s29", "-define(WRAP(X), ?OPEN X ?CLOSE).\\n-define(OPEN, [).\\n-define(CLOSE, ]).", "ok"},
    {"s30", "-define(FIELDS, a :: integer(), b = 1 :: atom()).", "ok"},
    {"s31", "-define(F_EQ, x = ).", "ok"},
    {"s32", "-define(E, ).", "ok"},
    {"s33", "-define(DOTS, ...).", "ok"},
    {"s34", "-define(DOTDOT, ..).", "ok"},
    {"s35", "-define(BIN_OPEN, <<).", "ok"},
    {"s36", "-define(ARROW_OK, -> ok).", "ok"},
    {"s37", "-define(BAR, |).", "ok"},
    {"s38", "-define(ASSOC, =>).", "ok"},
    {"e39", "-define(UNTERM, 1.", "err"},
    {"e40", "-define(NO_DOT, 1)", "err"},
    {"e41", "-define(1, x).", "err"},
    {"e42", "-define(F(1), x).", "err"},
    {"e43", "-define(F(X, X), x).", "err"},
    {"e44", "EOF:-define(CUT, 1", "err"}
  };

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    try {
      Class<?> layer = Class.forName("org.intellij.erlang.macrolayer.ErlangMacroLayer");
      Class<?> mode = Class.forName("org.intellij.erlang.macrolayer.ErlangMacroLayer$Mode");
      layer.getMethod("setModeForTests", mode, com.intellij.openapi.Disposable.class)
        .invoke(null, mode.getEnumConstants()[1], getTestRootDisposable());
    }
    catch (ClassNotFoundException ignored) {
    }
  }

  /** A macro use inside such a body keeps its reference, and a parameter of the body stays what it is today. */
  public void testUsesInsideABodyKeepTheirReferences() {
    PsiFile f = myFixture.configureByText("w.erl", "-module(w).\n-export([f/0]).\n-define(OPEN, [).\n-define(CLOSE, ]).\n" +
                                                   "-define(WRAP(X), ?OPEN X ?CLOSE).\nf() -> ?WRAP(1).\n");
    myFixture.doHighlighting();
    int at = f.getText().indexOf("?OPEN X") + 1;
    com.intellij.psi.PsiElement target = f.findReferenceAt(at).resolve();
    assertNotNull(target);
    assertTrue(target.getText(), target.getText().startsWith("-define(OPEN, [)"));
  }

  public void testDefineBodies() {
    myFixture.enableInspections(new org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection());
    List<String> wrong = new ArrayList<>();
    for (String[] c : CASES) {
      String body = c[1].replace("\\n", "\n");
      String text = body.startsWith("EOF:")
                    ? "-module(" + c[0] + ").\n-export([f/0]).\nf() -> ok.\n" + body.substring(4) + "\n"
                    : "-module(" + c[0] + ").\n-export([f/0]).\n" + body + "\nf() -> ok.\n";
      PsiFile f = myFixture.configureByText(c[0] + ".erl", text);
      List<String> errors = new ArrayList<>();
      boolean onDefine = false;
      int defineLine = text.indexOf("-define");
      int defineEnd = body.startsWith("EOF:") ? text.length() : text.indexOf("\nf() -> ok.");
      for (HighlightInfo h : myFixture.doHighlighting()) {
        if (h.getSeverity() != HighlightSeverity.ERROR) continue;
        errors.add((StringUtil.offsetToLineNumber(text, h.getStartOffset()) + 1) + ": " + h.getDescription());
        if (h.getStartOffset() >= defineLine && h.getStartOffset() <= defineEnd) onDefine = true;
      }
      boolean nextForm = ((ErlangFile)f).getFunction("f", 0) != null;
      boolean ok = c[2].equals("ok") ? errors.isEmpty() && nextForm : onDefine;
      System.out.println("DEFINE-BODY " + c[0] + "\terlc=" + c[2] + "\terrors=" + errors.size() + "\tnext form " + (nextForm ? "kept" : "LOST") +
                         "\t" + (ok ? "right" : "WRONG") + "\t" + errors);
      if (!ok) wrong.add(c[0] + " " + errors + (nextForm ? "" : " (the function after the define is lost)"));
    }
    assertEquals(List.of(), wrong);
  }
}
