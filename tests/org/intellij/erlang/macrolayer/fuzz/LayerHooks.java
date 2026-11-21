package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.openapi.Disposable;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * The few hooks of the macro layer the suite uses, by reflection, so that the same test sources compile and run on
 * master, where the layer does not exist ({@link #present()} is false there and every hook does nothing).
 */
public final class LayerHooks {
  private static final String LAYER = "org.intellij.erlang.macrolayer.ErlangMacroLayer";
  private static final Class<?> ourLayer = load(LAYER);
  public static final String REGISTRY_KEY = "erlang.macro.layer";

  private LayerHooks() {
  }

  private static @Nullable Class<?> load(String name) {
    try {
      return Class.forName(name, false, LayerHooks.class.getClassLoader());
    }
    catch (Throwable e) {
      return null;
    }
  }

  public static boolean present() {
    return ourLayer != null;
  }

  public static String arm() {
    return present() ? "branch" : "master";
  }

  public static boolean isEnabled() {
    if (ourLayer == null) return false;
    try {
      return (Boolean)ourLayer.getMethod("isEnabled").invoke(null);
    }
    catch (Throwable e) {
      return false;
    }
  }

  /** Switches the layer on or off until {@code parent} is disposed (no-op on master). */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static void setEnabled(boolean on, @NotNull Disposable parent) {
    if (ourLayer == null) return;
    // the user's switch (the Registry key), when the branch has it: it survives moves of the layer's classes
    try {
      com.intellij.openapi.util.registry.RegistryValue v = com.intellij.openapi.util.registry.Registry.get(REGISTRY_KEY);
      v.asBoolean();
      v.setValue(on, parent);
      return;
    }
    catch (Throwable ignored) {
      // no such key: the test switch below
    }
    try {
      Class<? extends Enum> mode = (Class<? extends Enum>)load(LAYER + "$Mode");
      Object value = Enum.valueOf(mode, on ? "LAYER" : "OFF");
      ourLayer.getMethod("setModeForTests", mode, Disposable.class).invoke(null, value, parent);
    }
    catch (Throwable e) {
      throw new IllegalStateException("cannot switch the macro layer", e);
    }
  }

  /** Lets the layer build its copy on the UI thread in tests (so that a fixture action sees the layer's answers). */
  public static void allowEdtBuild(@NotNull Disposable parent) {
    if (ourLayer == null) return;
    try {
      ourLayer.getMethod("setEdtBuildForTests", boolean.class, Disposable.class).invoke(null, true, parent);
    }
    catch (Throwable ignored) {
    }
  }

  /** The expanded copy of the file, built now; null on master or when the layer has none. */
  public static @Nullable PsiFile copyFile(@NotNull PsiFile file) {
    if (ourLayer == null) return null;
    try {
      Method get = ourLayer.getMethod("getCopy", PsiFile.class);
      Object copy = get.invoke(null, file);
      if (copy == null) return null;
      Field f = copy.getClass().getField("copyFile");
      return (PsiFile)f.get(copy);
    }
    catch (Throwable e) {
      return null;
    }
  }

  public static boolean isLayerClass(@Nullable Object o) {
    return o != null && o.getClass().getName().startsWith("org.intellij.erlang.macrolayer.");
  }
}
