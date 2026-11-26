package org.intellij.erlang.pbt;

import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A node of a generated module. It prints to Erlang text and it is what the shrinker reduces.
 * <p>
 * A plain node is a concatenation of parts, each a {@link String} or a child node. A sequence node prints its items
 * joined by a separator; the shrinker may remove items as long as {@link #min} are left. An empty sequence with
 * {@link #omitEmpty} prints nothing, its {@link #open} and {@link #close} included (an optional {@code when} guard).
 * The category tells the shrinker what may replace the node: an expression by one of its subexpressions or by
 * {@code ok}, a pattern by {@code _}, a guard test by {@code true}, a type by {@code any()}.
 */
final class PbtNode {
  enum Cat {FORM, CLAUSE, EXPR, PAT, GUARD, TYPE, OTHER}

  final String construct;
  final Cat cat;
  final List<Object> parts;
  final boolean seq;
  final String open;
  final String sep;
  final String close;
  final int min;
  final boolean omitEmpty;

  private PbtNode(String construct, Cat cat, List<Object> parts, boolean seq, String open, String sep, String close, int min,
                  boolean omitEmpty) {
    this.construct = construct;
    this.cat = cat;
    this.parts = Collections.unmodifiableList(parts);
    this.seq = seq;
    this.open = open;
    this.sep = sep;
    this.close = close;
    this.min = min;
    this.omitEmpty = omitEmpty;
  }

  static PbtNode of(String construct, Cat cat, Object... parts) {
    List<Object> list = new ArrayList<>(parts.length);
    for (Object p : parts) {
      if (!(p instanceof String) && !(p instanceof PbtNode)) throw new IllegalArgumentException(String.valueOf(p));
      list.add(p);
    }
    return new PbtNode(construct, cat, list, false, "", "", "", 0, false);
  }

  static PbtNode leaf(String construct, Cat cat, String text) {
    return of(construct, cat, text);
  }

  static PbtNode seq(String construct, Cat cat, String open, String sep, String close, int min, List<PbtNode> items) {
    return new PbtNode(construct, cat, new ArrayList<>(items), true, open, sep, close, min, false);
  }

  /** A sequence that prints nothing at all when it is empty, for example an optional guard. */
  static PbtNode optional(String construct, String open, String sep, String close, List<PbtNode> items) {
    return new PbtNode(construct, Cat.OTHER, new ArrayList<>(items), true, open, sep, close, 0, true);
  }

  PbtNode withParts(List<Object> newParts) {
    return new PbtNode(construct, cat, new ArrayList<>(newParts), seq, open, sep, close, min, omitEmpty);
  }

  String print() {
    StringBuilder sb = new StringBuilder();
    print(sb);
    return sb.toString();
  }

  void print(StringBuilder sb) {
    if (seq) {
      if (parts.isEmpty() && omitEmpty) return;
      sb.append(open);
      for (int i = 0; i < parts.size(); i++) {
        if (i > 0) sb.append(sep);
        ((PbtNode)parts.get(i)).print(sb);
      }
      sb.append(close);
      return;
    }
    for (Object p : parts) {
      if (p instanceof String s) sb.append(s);
      else ((PbtNode)p).print(sb);
    }
  }

  int size() {
    int n = 1;
    for (Object p : parts) if (p instanceof PbtNode c) n += c.size();
    return n;
  }

  // ============================================================================================ edits for the shrinker

  /** One reduction of a tree: remove {@code count} items of the sequence at {@code path}, or replace the node there. */
  record Edit(List<Integer> path, int from, int count, @Nullable PbtNode replacement, String what) {
  }

  PbtNode apply(Edit e) {
    return apply(e, 0);
  }

  private PbtNode apply(Edit e, int depth) {
    if (depth == e.path.size()) {
      if (e.replacement != null) return e.replacement;
      List<Object> np = new ArrayList<>(parts);
      np.subList(e.from, e.from + e.count).clear();
      return withParts(np);
    }
    int i = e.path.get(depth);
    List<Object> np = new ArrayList<>(parts);
    np.set(i, ((PbtNode)parts.get(i)).apply(e, depth + 1));
    return withParts(np);
  }

  /**
   * The reductions of this tree, largest first: removals of chunks of sequence items (halves, quarters, ..., single
   * items), then replacements of nodes by simpler ones, top-down.
   */
  List<Edit> edits() {
    List<Edit> removals = new ArrayList<>();
    List<Edit> replacements = new ArrayList<>();
    collect(new ArrayList<>(), removals, replacements);
    removals.sort((a, b) -> Integer.compare(b.count, a.count));
    List<Edit> all = new ArrayList<>(removals);
    all.addAll(replacements);
    return all;
  }

  private void collect(List<Integer> path, List<Edit> removals, List<Edit> replacements) {
    if (seq) {
      int removable = parts.size() - min;
      for (int chunk = Integer.highestOneBit(Math.max(removable, 1)); chunk >= 1 && removable > 0; chunk /= 2) {
        if (chunk > removable) continue;
        for (int from = 0; from + chunk <= parts.size(); from += chunk) {
          removals.add(new Edit(List.copyOf(path), from, chunk, null, "remove " + chunk + " of " + construct));
        }
      }
    }
    if (!path.isEmpty()) replacementsOf(path, replacements);
    for (int i = 0; i < parts.size(); i++) {
      if (parts.get(i) instanceof PbtNode child) {
        path.add(i);
        child.collect(path, removals, replacements);
        path.remove(path.size() - 1);
      }
    }
  }

  private void replacementsOf(List<Integer> path, List<Edit> out) {
    List<Integer> p = List.copyOf(path);
    switch (cat) {
      case EXPR -> {
        List<PbtNode> subs = new ArrayList<>();
        descendants(Cat.EXPR, subs, 2);
        for (PbtNode s : subs) out.add(new Edit(p, 0, 0, s, "hoist " + s.construct + " out of " + construct));
        if (!"ok".equals(print())) out.add(new Edit(p, 0, 0, leaf("expr.atom", Cat.EXPR, "ok"), "replace " + construct + " by ok"));
      }
      case PAT -> {
        if (!"_".equals(print())) out.add(new Edit(p, 0, 0, leaf("pat.wildcard", Cat.PAT, "_"), "replace " + construct + " by _"));
      }
      case GUARD -> {
        if (!"true".equals(print())) out.add(new Edit(p, 0, 0, leaf("guard.true", Cat.GUARD, "true"), "replace " + construct + " by true"));
      }
      case TYPE -> {
        List<PbtNode> subs = new ArrayList<>();
        descendants(Cat.TYPE, subs, 2);
        for (PbtNode s : subs) out.add(new Edit(p, 0, 0, s, "hoist " + s.construct + " out of " + construct));
        if (!"any()".equals(print())) out.add(new Edit(p, 0, 0, leaf("type.any", Cat.TYPE, "any()"), "replace " + construct + " by any()"));
      }
      default -> {
      }
    }
  }

  /** Descendants of the category, at most {@code levels} nodes of that category deep. */
  private void descendants(Cat c, List<PbtNode> out, int levels) {
    for (Object o : parts) {
      if (!(o instanceof PbtNode child)) continue;
      if (child.cat == c) {
        out.add(child);
        if (levels > 1) child.descendants(c, out, levels - 1);
      }
      else {
        child.descendants(c, out, levels);
      }
    }
  }
}
