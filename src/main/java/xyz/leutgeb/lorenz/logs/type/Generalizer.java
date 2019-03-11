package xyz.leutgeb.lorenz.logs.type;

import static xyz.leutgeb.lorenz.logs.type.TypeVar.GREEK;

import java.util.HashMap;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

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
