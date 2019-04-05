package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Problem;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Value
@RequiredArgsConstructor
public class FunctionType extends Type {
  Type from;
  Type to;

  public FunctionType(List<Type> from, Type to) {
    this.from = new ProductType(from);
    this.to = to;
  }

  public Type generalize(Generalizer g) {
    return new FunctionType(from.generalize(g), to.generalize(g));
  }

  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof FunctionType)) {
      throw new TypeMismatch(this, b);
    }
    var ft = (FunctionType) b;
    return List.of(new Equivalence(from, ft.from), new Equivalence(to, ft.to));
  }

  public Type substitute(UnificationVariable v, Type t) {
    return new FunctionType(from.substitute(v, t), to.substitute(v, t));
  }

  public boolean occurs(UnificationVariable v) {
    return from.occurs(v) || to.occurs(v);
  }

  @Override
  public Type wiggle(Map<TypeVar, UnificationVariable> wiggled, Problem context) {
    return new FunctionType(from.wiggle(wiggled, context), to.wiggle(wiggled, context));
  }

  @Override
  public String toString() {
    return "(" + from + " → " + to + ")";
  }
}
