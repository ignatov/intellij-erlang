package org.intellij.erlang.macrolayer;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Which elements of a file can host the layer's references (a bare macro use, a token that only the copy gives a
 * role).
 */
public final class ErlangMacroLayerHosts {
  /** Scans of an element's children by the hosted references' find (tests: a file with nothing to host needs none). */
  public static final AtomicLong CHILD_SCANS = new AtomicLong();

  private ErlangMacroLayerHosts() {
  }
}
