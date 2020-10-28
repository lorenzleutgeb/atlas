package xyz.leutgeb.lorenz.lac.unification;

import java.util.HashMap;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;

public class Generalizer {
  private final HashMap<UnificationVariable, TypeVariable> mapping;

  public Generalizer() {
    this.mapping = new HashMap<>();
  }

  public TypeVariable generalize(UnificationVariable u) {
    return this.mapping.computeIfAbsent(
        u,
        k -> {
          var x = this.mapping.size();
          return new TypeVariable(x);
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
