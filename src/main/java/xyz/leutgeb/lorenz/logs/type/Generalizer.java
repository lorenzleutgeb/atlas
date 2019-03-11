package xyz.leutgeb.lorenz.logs.type;

import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

import java.util.HashMap;

import static xyz.leutgeb.lorenz.logs.type.TypeVar.GREEK;

public class Generalizer {
  private final HashMap<UnificationVariable, TypeVar> mapping;

  public Generalizer() {
    this.mapping = new HashMap<>(GREEK.length);
  }

  public Type generalize(UnificationVariable u) {
    return this.mapping.computeIfAbsent(
        u,
        k -> {
          var x = this.mapping.size();
          return x < GREEK.length ? GREEK[x] : new TypeVar("gen" + x);
        });
  }
}
