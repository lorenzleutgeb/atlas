package xyz.leutgeb.lorenz.atlas.unification;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;

@Value
public class Substitution implements Function<Type, Type> {
  Map<TypeVariable, Type> raw;

  public Substitution(Type... literal) {
    if (literal.length % 2 != 0) {
      throw new IllegalArgumentException("literal substitution must have even length (pairs)");
    }
    this.raw = new HashMap<>();
    for (int i = 0; i < literal.length - 1; i += 2) {
      put((TypeVariable) literal[i], literal[i + 1]);
    }
  }

  /** Cloning constructor. */
  private Substitution(Map<TypeVariable, Type> raw) {
    this.raw = new HashMap<>(raw);
  }

  private void put(TypeVariable variable, Type target) {
    if (variable == target) {
      return;
    }
    if (variable instanceof UnificationVariable) {
      if (target.occurs(variable)) {
        throw new IllegalArgumentException("eager occurs check failed");
      }
    }
    this.raw.put(variable, target);
  }

  @Override
  public String toString() {
    if (this.raw.isEmpty()) {
      return "ɛ";
    }
    return "{"
        + this.raw.entrySet().stream()
            .map(e -> e.getKey() + " ↦ " + e.getValue())
            .collect(Collectors.joining(", "))
        + "}";
  }

  /**
   * Idempotent, pure composition. Only reads from {@code this} and {@code other} and writes to
   * separate resulting instance.
   */
  public Substitution compose(Substitution other) {
    var result = new Substitution(other.raw);
    for (var e : raw.entrySet()) {
      result.put(e.getKey(), other.apply(e.getValue()));
    }
    for (var e : other.raw.entrySet()) {
      if (!result.raw.containsKey(e.getKey())) {
        result.put(e.getKey(), e.getValue());
      }
    }
    return result;
  }

  /** This mutates the state of the substitution. */
  public void substitute(TypeVariable variable, Type target) {
    raw.replaceAll((k, v) -> v.substitute(variable, target));
    put(variable, target);
  }

  @Override
  public Type apply(Type target) {
    if (target == null) {
      return null;
    }
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

  private boolean equals(Substitution other, Collection<TypeVariable> projection) {
    return projection.parallelStream().allMatch(x -> apply(x).equals(other.apply(x)));
  }

  public boolean isInDomain(TypeVariable v) {
    return raw.containsKey(v);
  }
}
