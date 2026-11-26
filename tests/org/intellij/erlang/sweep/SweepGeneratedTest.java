package org.intellij.erlang.sweep;

import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The generators of the robustness sweep run inside the sweep only, where an exception of one of them stops the whole
 * synthetic run. This checks them for many seeds: they do not throw, and the input ids are unique (a resumed run skips
 * the inputs by id).
 */
public class SweepGeneratedTest extends TestCase {
  private static final Function<String, List<int[]>> WORDS = text -> {
    List<int[]> result = new ArrayList<>();
    Matcher m = Pattern.compile("\\S+").matcher(text);
    while (m.find()) result.add(new int[]{m.start(), m.end()});
    return result;
  };

  public void testManySeeds() {
    for (long seed = 0; seed < 200; seed++) {
      List<ErlangRobustnessSweepTest.Input> inputs = SweepGenerated.inputs(seed * 7919, WORDS);
      Set<String> ids = new HashSet<>();
      for (ErlangRobustnessSweepTest.Input in : inputs) {
        assertTrue("duplicate id " + in.id + " for seed " + seed, ids.add(in.id));
        assertTrue(in.id, in.id.startsWith("synthetic/"));
      }
    }
  }

  public void testSoupUsesTheLexerTokens() {
    List<String> tokens = SweepGenerated.soupTokens();
    assertTrue(tokens.contains("maybe"));
    assertTrue(tokens.contains("?="));
    assertTrue(tokens.contains("~\"s\""));
  }
}
