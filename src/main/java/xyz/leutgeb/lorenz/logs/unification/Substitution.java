package xyz.leutgeb.lorenz.logs.unification;

import java.util.HashMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.type.Generalizer;
import xyz.leutgeb.lorenz.logs.type.Type;

public class Substitution implements Function<Type, Type> {
  private HashMap<UnificationVariable, Type> subs;

  public Substitution() {
    this.subs = new HashMap<>();
  }

  @Override
  public String toString() {
    return "{"
        + this.subs
            .entrySet()
            .stream()
            .map(e -> e.getKey() + " ↦ " + e.getValue())
            .collect(Collectors.joining(", "))
        + "}";
  }

  public void add(UnificationVariable var, Type target) {
    this.subs.put(var, target);
  }

  public void substitute(UnificationVariable variable, Type target) {
    subs.replaceAll((k, v) -> v.substitute(variable, target));
  }

  @Deprecated
  public void merge(Substitution to) {
    for (UnificationVariable v : this.subs.keySet()) {
      to.add(v, this.subs.get(v));
    }
  }

  @Override
  public Type apply(Type target) {
    Type t = target;
    for (UnificationVariable var : this.subs.keySet()) {
      t = t.substitute(var, this.subs.get(var));
    }
    return t;
  }

  public void generalize(Generalizer generalizer) {
    subs.replaceAll((k, v) -> v.generalize(generalizer));
  }
}
