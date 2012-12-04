package org.intellij.erlang.jps.builder;

import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildTargetLoader;
import org.jetbrains.jps.builders.BuildTargetType;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsModel;
import org.jetbrains.jps.model.module.JpsTypedModule;

import java.util.ArrayList;
import java.util.List;

/**
 * @author @nik
 */
public class ErlangTargetType extends BuildTargetType<ErlangTarget> {
  public static final ErlangTargetType PRODUCTION = new ErlangTargetType("erlang-production", false);
  public static final ErlangTargetType TESTS = new ErlangTargetType("erlang-tests", true);
  private final boolean myTests;

  private ErlangTargetType(String erlang, boolean tests) {
    super(erlang);
    myTests = tests;
  }

  @NotNull
  @Override
  public List<ErlangTarget> computeAllTargets(@NotNull JpsModel model) {
    List<ErlangTarget> targets = new ArrayList<ErlangTarget>();
    for (JpsTypedModule<JpsDummyElement> module : model.getProject().getModules(JpsErlangModuleType.INSTANCE)) {
      targets.add(new ErlangTarget(module, this));
    }
    return targets;
  }

  @NotNull
  @Override
  public BuildTargetLoader<ErlangTarget> createLoader(@NotNull final JpsModel model) {
    return new BuildTargetLoader<ErlangTarget>() {
      @Nullable
      @Override
      public ErlangTarget createTarget(@NotNull String targetId) {
        for (JpsTypedModule<JpsDummyElement> module : model.getProject().getModules(JpsErlangModuleType.INSTANCE)) {
          if (module.getName().equals(targetId)) {
            return new ErlangTarget(module, ErlangTargetType.this);
          }
        }
        return null;
      }
    };
  }

  public boolean isTests() {
    return myTests;
  }
}
