package xyz.leutgeb.lorenz.logs.unification;

import static xyz.leutgeb.lorenz.logs.type.TypeVariable.GREEK;

import java.util.HashMap;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeVariable;

public class Generalizer {
  private final HashMap<UnificationVariable, TypeVariable> mapping;

  public Generalizer() {
    this.mapping = new HashMap<>(GREEK.length);
  }

  public Type generalize(UnificationVariable u) {
    return this.mapping.computeIfAbsent(
        u,
        k -> {
          var x = this.mapping.size();
          return x < GREEK.length ? GREEK[x] : new TypeVariable("gen" + x);
        });
  }

  public Substitution toSubstitution() {
    var result = new Substitution();
    for (var e : mapping.entrySet()) {
      result.substitute(e.getKey(), e.getValue());
    }
    return result;
  }
}
