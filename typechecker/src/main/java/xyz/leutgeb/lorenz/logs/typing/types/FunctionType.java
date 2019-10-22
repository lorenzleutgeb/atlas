package xyz.leutgeb.lorenz.logs.typing.types;

import static java.util.Arrays.asList;
import static java.util.Arrays.copyOf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.typing.TypeVariable;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Value
@EqualsAndHashCode(callSuper = false)
public class FunctionType extends Type {
  xyz.leutgeb.lorenz.logs.typing.types.ProductType from;
  Type to;

  public FunctionType(xyz.leutgeb.lorenz.logs.typing.types.ProductType from, Type to) {
    Objects.requireNonNull(from);
    Objects.requireNonNull(to);
    if (to instanceof FunctionType) {
      throw new IllegalArgumentException("curried functions are not supported");
    }
    this.from = from;
    this.to = to;
  }

  public FunctionType(List<Type> from, Type to) {
    this(new xyz.leutgeb.lorenz.logs.typing.types.ProductType(from), to);
  }

  public FunctionType(Type... types) {
    this(new ProductType(asList(copyOf(types, types.length - 1))), types[types.length - 1]);
  }

  public Type generalize(Generalizer g) {
    return new FunctionType(
        (xyz.leutgeb.lorenz.logs.typing.types.ProductType) from.generalize(g), to.generalize(g));
  }

  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof FunctionType)) {
      throw new TypeMismatch(this, b);
    }
    var ft = (FunctionType) b;
    // Check lengths of "from" here, to catch errors early. Not strictly necessary, but helps.
    if (from.size() != ft.from.size()) {
      throw new TypeMismatch(from, ft.from);
    }
    final var result = new ArrayList<Equivalence>(2);
    if (!from.equals(ft.from)) {
      result.add(new Equivalence(from, ft.from));
    }
    if (!to.equals(ft.to)) {
      result.add(new Equivalence(to, ft.to));
    }
    return result;
  }

  public Type substitute(TypeVariable v, Type t) {
    return new FunctionType(
        (xyz.leutgeb.lorenz.logs.typing.types.ProductType) from.substitute(v, t),
        to.substitute(v, t));
  }

  public boolean occurs(UnificationVariable v) {
    return from.occurs(v) || to.occurs(v);
  }

  @Override
  public Type wiggle(Substitution wiggled, UnificationProblem context) {
    return new FunctionType(
        (xyz.leutgeb.lorenz.logs.typing.types.ProductType) from.wiggle(wiggled, context),
        to.wiggle(wiggled, context));
  }

  @Override
  public String toHaskell() {
    return (from.size() > 0 ? from.toCurriedHaskell() + " -> " : "") + to.toHaskell();
  }

  @Override
  public String toString() {
    return from + " -> " + to;
  }
}
