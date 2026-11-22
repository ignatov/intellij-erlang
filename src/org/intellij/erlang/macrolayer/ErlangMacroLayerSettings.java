package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.SimpleModificationTracker;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.openapi.util.registry.RegistryValue;
import com.intellij.openapi.util.registry.RegistryValueListener;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The switch of the macro layer and its settings. The switch is the Registry key {@link #REGISTRY_KEY} (default on),
 * read at each call, so turning it off in the IDE takes effect at once; the system property {@code erlang.macro.layer},
 * else the environment variable {@code ERLANG_MACRO_LAYER} ({@code off} or {@code layer}), overrides it. The other
 * settings are read once from system properties or environment variables (diagnostics and measurements), and tests set
 * them through the {@code @TestOnly} methods.
 * <p>
 * Threading: any thread, no read action; nothing here is cached except the overrides read at class load.
 */
final class ErlangMacroLayerSettings {
  private ErlangMacroLayerSettings() {
  }

  /** The Registry key of the switch (registered in ErlangPlugin.xml). */
  public static final String REGISTRY_KEY = "erlang.macro.layer";
  /** The system property or the environment variable, when set: it overrides the Registry key. */
  private static final @Nullable ErlangMacroLayer.Mode OVERRIDE_MODE = readMode();
  /**
   * 4,000. With the narrow gate a file pays for the copy only when it needs substitution,
   * and a big function with few expanded tokens (scan_entity_value/7 of xmerl_scan.erl: 2,354) no longer stays broken.
   * Expansion bombs stay out: a form over the cap is not expanded, and the expander itself stops at
   * {@link ErlangMacroExpander#MAX_TOKENS_PER_FORM} substituted tokens per form and {@link ErlangMacroExpander#MAX_DEPTH}
   * nested macro levels.
   */
  public static final int FORM_CAP = readInt("erlang.macro.layer.cap", "ERLANG_MACRO_LAYER_CAP", 4000);

  /** The form cap in force ({@link #FORM_CAP}). */
  public static int formCap() {
    return FORM_CAP;
  }

  private static final boolean REMOTE_DEFAULT = "on".equals(read("erlang.macro.layer.remote", "ERLANG_MACRO_LAYER_REMOTE"));
  private static volatile @Nullable ErlangMacroLayer.Mode ourTestMode;
  /** Diagnostic only: {@code erlang.macro.layer.edt=allow} lets the UI thread build a copy (default: never). */
  private static final boolean EDT_BUILD_DEFAULT = "allow".equals(read("erlang.macro.layer.edt", "ERLANG_MACRO_LAYER_EDT"));
  private static volatile @Nullable Boolean ourTestEdtBuild;
  /**
   * {@code erlang.macro.layer.gate} / {@code ERLANG_MACRO_LAYER_GATE} = {@code wide} restores the wide gate
   * (a form asks the copy as soon as it uses a visible macro; for the soundness check of the narrow gate). Default:
   * narrow (only forms that need substitution ask).
   */
  private static final boolean GATE_WIDE_DEFAULT = "wide".equals(read("erlang.macro.layer.gate", "ERLANG_MACRO_LAYER_GATE"));
  private static volatile @Nullable Boolean ourTestGateWide;

  static boolean isGateWide() {
    Boolean b = ourTestGateWide;
    return b != null ? b : GATE_WIDE_DEFAULT;
  }

  @TestOnly
  public static void setGateWideForTests(boolean wide, @NotNull Disposable parent) {
    Boolean previous = ourTestGateWide;
    ourTestGateWide = wide;
    Disposer.register(parent, () -> ourTestGateWide = previous);
  }

  static boolean isEdtBuildAllowed() {
    Boolean b = ourTestEdtBuild;
    return b != null ? b : EDT_BUILD_DEFAULT;
  }

  @TestOnly
  public static void setEdtBuildForTests(boolean allow, @NotNull Disposable parent) {
    Boolean previous = ourTestEdtBuild;
    ourTestEdtBuild = allow;
    Disposer.register(parent, () -> ourTestEdtBuild = previous);
  }

  static @Nullable String read(String property, String env) {
    String value = System.getProperty(property);
    if (value == null || value.isEmpty()) value = System.getenv(env);
    return value == null || value.isEmpty() ? null : value.trim().toLowerCase(Locale.ROOT);
  }

  static int readInt(String property, String env, int def) {
    String v = read(property, env);
    try {
      return v == null ? def : Integer.parseInt(v);
    }
    catch (NumberFormatException e) {
      return def;
    }
  }

  private static @Nullable ErlangMacroLayer.Mode readMode() {
    String v = read("erlang.macro.layer", "ERLANG_MACRO_LAYER");
    if (v == null) return null;
    return "off".equals(v) || "none".equals(v) || "false".equals(v) ? ErlangMacroLayer.Mode.OFF : ErlangMacroLayer.Mode.LAYER;
  }

  /** Read at each call: a test mode, else the system property or environment variable, else the Registry key. */
  public static @NotNull ErlangMacroLayer.Mode getMode() {
    ErlangMacroLayer.Mode m = ourTestMode;
    if (m != null) return m;
    if (OVERRIDE_MODE != null) return OVERRIDE_MODE;
    if (ApplicationManager.getApplication() == null) return ErlangMacroLayer.Mode.LAYER;
    listenToTheKey();
    return Registry.is(REGISTRY_KEY, true) ? ErlangMacroLayer.Mode.LAYER : ErlangMacroLayer.Mode.OFF;
  }

  /**
   * Moves whenever the mode may have changed (the Registry key flipped, a test set or restored the mode): a cached
   * value that depends on the switch lists it as a dependency, so a flip drops it at once, both ways.
   */
  static final SimpleModificationTracker MODE_TRACKER = new SimpleModificationTracker();
  private static final AtomicBoolean LISTENING = new AtomicBoolean();

  private static void listenToTheKey() {
    if (LISTENING.get() || !LISTENING.compareAndSet(false, true)) return;
    Registry.get(REGISTRY_KEY).addListener(new RegistryValueListener() {
      @Override
      public void afterValueChanged(@NotNull RegistryValue value) {
        MODE_TRACKER.incModificationCount();
      }
    }, ApplicationManager.getApplication());
  }

  public static boolean isEnabled() {
    return getMode() == ErlangMacroLayer.Mode.LAYER;
  }

  public static boolean isRemoteEnabled() {
    return REMOTE_DEFAULT;
  }

  @TestOnly
  public static void setModeForTests(@NotNull ErlangMacroLayer.Mode mode, @NotNull Disposable parent) {
    ErlangMacroLayer.Mode previous = ourTestMode;
    ourTestMode = mode;
    MODE_TRACKER.incModificationCount();
    Disposer.register(parent, () -> {
      ourTestMode = previous;
      MODE_TRACKER.incModificationCount();
    });
  }
}
