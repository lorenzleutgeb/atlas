package xyz.leutgeb.lorenz.logs.unification;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeVariable;

public class Substitution implements Function<Type, Type> {
  public static Substitution identity() {
    return new Substitution();
  }

  private Map<TypeVariable, Type> raw;

  private Substitution() {
    this.raw = new HashMap<>();
  }

  public Substitution(Type... literal) {
    if (literal.length % 2 != 0) {
      throw new IllegalArgumentException("literal substitution must have even length (pairs)");
    }
    this.raw = new HashMap<>();
    for (int i = 0; i < literal.length - 1; i += 2) {
      this.raw.put((TypeVariable) literal[i], literal[i + 1]);
    }
  }

  private Substitution(Map<TypeVariable, Type> raw) {
    this.raw = raw;
  }

  @Override
  public String toString() {
    if (this.raw.isEmpty()) {
      return "ɛ";
    }
    return "{"
        + this.raw
            .entrySet()
            .stream()
            .map(e -> e.getKey() + " ↦ " + e.getValue())
            .collect(Collectors.joining(", "))
        + "}";
  }

  public Substitution compose(Substitution other) {
    var result = new Substitution(other.raw);
    for (var e : raw.entrySet()) {
      result.raw.put(e.getKey(), other.apply(e.getValue()));
    }
    for (var e : other.raw.entrySet()) {
      result.raw.putIfAbsent(e.getKey(), e.getValue());
    }
    return result;
  }

  /** NOTE: This mutates the state of the substitution. */
  public void substitute(TypeVariable variable, Type target) {
    raw.replaceAll((k, v) -> v.substitute(variable, target));
    raw.put(variable, target);
  }

  @Override
  public Type apply(Type target) {
    Type t = target;
    for (TypeVariable var : this.raw.keySet()) {
      t = t.substitute(var, this.raw.get(var));
    }
    return t;
  }

  public void generalize(Generalizer generalizer) {
    raw.replaceAll((k, v) -> apply(k).generalize(generalizer));
  }

  public boolean equals(Substitution other) {
    return equals(other, raw.keySet()) && equals(other, other.raw.keySet());
  }

  public boolean equals(Substitution other, Collection<TypeVariable> projection) {
    return projection.parallelStream().allMatch(x -> apply(x).equals(other.apply(x)));
  }
}
