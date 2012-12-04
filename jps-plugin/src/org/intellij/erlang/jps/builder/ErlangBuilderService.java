package org.intellij.erlang.jps.builder;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.builders.BuildTargetType;
import org.jetbrains.jps.incremental.BuilderService;
import org.jetbrains.jps.incremental.TargetBuilder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * @author @nik
 */
public class ErlangBuilderService extends BuilderService {
  @Override
  public List<? extends BuildTargetType<?>> getTargetTypes() {
    return Arrays.asList(ErlangTargetType.PRODUCTION, ErlangTargetType.TESTS);
  }

  @NotNull
  @Override
  public List<? extends TargetBuilder<?, ?>> createBuilders() {
    return Collections.singletonList(new ErlangBuilder());
  }
}
