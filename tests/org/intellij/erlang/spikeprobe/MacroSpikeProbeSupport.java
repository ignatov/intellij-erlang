package org.intellij.erlang.spikeprobe;

import com.google.gson.JsonObject;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.LoggedErrorProcessor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ThrowableRunnable;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Prober of the macro spike: helpers shared by the probe tests. They use only the public plugin and platform API, so
 * the same sources run in the unmodified baseline export and in the spike branch. The mode of the spike is chosen by
 * the ERLANG_MACRO_SUBSTITUTION environment variable of the run; PROBE_LABEL names the run in the outputs.
 */
final class MacroSpikeProbeSupport {
  private MacroSpikeProbeSupport() {
  }

  static @Nullable String env(String name) {
    String v = System.getenv(name);
    if (v == null || v.isEmpty()) v = System.getProperty(name);
    return v == null || v.isEmpty() ? null : v;
  }

  static String label() {
    String l = env("PROBE_LABEL");
    return l == null ? "unlabeled" : l;
  }

  static LightProjectDescriptor descriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  static String read(Path file) throws IOException {
    byte[] bytes = Files.readAllBytes(file);
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text.replace("\r\n", "\n").replace('\r', '\n');
  }

  /** One failure seen during an operation: thrown or logged. */
  record Failure(String how, String type, String message, String pluginFrame) {
    JsonObject json() {
      JsonObject o = new JsonObject();
      o.addProperty("how", how);
      o.addProperty("type", type);
      o.addProperty("message", message);
      o.addProperty("frame", pluginFrame);
      return o;
    }

    String line() {
      return how + " " + type + " @ " + pluginFrame + " : " + message;
    }
  }

  interface Body {
    void run() throws Throwable;
  }

  /** Runs the body, collecting what it throws and every error it logs. */
  static List<Failure> guarded(Body body) {
    List<Failure> result = Collections.synchronizedList(new ArrayList<>());
    LoggedErrorProcessor processor = new LoggedErrorProcessor() {
      @Override
      public @NotNull Set<Action> processError(@NotNull String category, @NotNull String message, String @NotNull [] details,
                                               @Nullable Throwable t) {
        result.add(failure("logged", t != null ? t : new Throwable(message), message));
        return EnumSet.noneOf(Action.class);
      }
    };
    try {
      LoggedErrorProcessor.executeWith(processor, (ThrowableRunnable<Throwable>)() -> {
        try {
          body.run();
        }
        finally {
          PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        }
      });
    }
    catch (Throwable t) {
      result.add(failure("thrown", t, null));
    }
    return new ArrayList<>(result);
  }

  static Failure failure(String how, Throwable t, @Nullable String message) {
    Throwable root = t;
    String frame = "?";
    for (Throwable c = t; c != null; c = c.getCause()) {
      for (StackTraceElement e : c.getStackTrace()) {
        if (e.getClassName().startsWith("org.intellij.erlang.") && !e.getClassName().startsWith("org.intellij.erlang.spikeprobe.")) {
          frame = e.getClassName() + "." + e.getMethodName() + ":" + e.getLineNumber();
          root = c;
          break;
        }
      }
      if (!"?".equals(frame)) break;
    }
    String m = message != null && !message.isEmpty() ? message : String.valueOf(t.getMessage());
    if (root != t) m = m + " / cause: " + root.getClass().getName() + ": " + root.getMessage();
    if (m.length() > 400) m = m.substring(0, 400);
    return new Failure(how, root.getClass().getName(), m.replace('\n', ' '), frame);
  }

  /** Counts of the tree: error elements, zero-length foreign leaves, substituted macro calls. */
  static int[] treeCounts(PsiFile file) {
    int errors = PsiTreeUtil.collectElementsOfType(file, PsiErrorElement.class).size();
    int foreign = 0, calls = 0;
    Deque<ASTNode> stack = new ArrayDeque<>();
    stack.push(file.getNode());
    while (!stack.isEmpty()) {
      ASTNode n = stack.pop();
      if (n.getClass().getSimpleName().contains("Foreign")) foreign++;
      if ("ERL_SUBSTITUTED_MACRO_CALL".equals(n.getElementType().toString())) calls++;
      for (ASTNode c = n.getFirstChildNode(); c != null; c = c.getTreeNext()) stack.push(c);
    }
    return new int[]{errors, foreign, calls};
  }

  static String clip(String s, int n) {
    s = s.replace('\n', ' ');
    return s.length() <= n ? s : s.substring(0, n) + "...";
  }

  static String describe(@Nullable PsiElement e) {
    if (e == null) return "null";
    String text = e.getText();
    return e.getClass().getSimpleName() + "[" + e.getTextRange() + "] '" + clip(text == null ? "null" : text, 60) + "'";
  }
}
