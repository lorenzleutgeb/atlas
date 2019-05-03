package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;
import xyz.leutgeb.lorenz.logs.unification.UnificiationProblem;

@Value
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
public class FunctionType extends Type {
  ProductType from;
  Type to;

  public FunctionType(List<Type> from, Type to) {
    this.from = new ProductType(from);
    this.to = to;
  }

  public Type generalize(Generalizer g) {
    return new FunctionType((ProductType) from.generalize(g), to.generalize(g));
  }

  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof FunctionType)) {
      throw new TypeMismatch(this, b);
    }
    var ft = (FunctionType) b;
    return List.of(new Equivalence(from, ft.from), new Equivalence(to, ft.to));
  }

  public Type substitute(TypeVariable v, Type t) {
    return new FunctionType((ProductType) from.substitute(v, t), to.substitute(v, t));
  }

  public boolean occurs(UnificationVariable v) {
    return from.occurs(v) || to.occurs(v);
  }

  @Override
  public Type wiggle(Map<TypeVariable, UnificationVariable> wiggled, UnificiationProblem context) {
    return new FunctionType(
        (ProductType) from.wiggle(wiggled, context), to.wiggle(wiggled, context));
  }

  @Override
  public String toString() {
    return "(" + from + " -> " + to + ")";
  }
}
