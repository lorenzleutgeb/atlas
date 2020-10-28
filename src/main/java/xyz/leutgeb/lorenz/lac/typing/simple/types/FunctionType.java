package xyz.leutgeb.lorenz.lac.typing.simple.types;

import static java.util.Arrays.asList;
import static java.util.Arrays.copyOf;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.unification.Equivalence;
import xyz.leutgeb.lorenz.lac.unification.Generalizer;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.TypeMismatch;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;

@Value
@EqualsAndHashCode(callSuper = false)
public class FunctionType implements Type {
  ProductType from;
  Type to;

  public FunctionType(ProductType from, Type to) {
    Objects.requireNonNull(from);
    Objects.requireNonNull(to);
    if (to instanceof FunctionType) {
      throw new IllegalArgumentException("curried functions are not supported");
    }
    this.from = from;
    this.to = to;
  }

  public FunctionType(List<Type> from, Type to) {
    this(new ProductType(from), to);
  }

  public FunctionType(Type... types) {
    this(new ProductType(asList(copyOf(types, types.length - 1))), types[types.length - 1]);
  }

  public Type generalize(Generalizer g) {
    return new FunctionType((ProductType) from.generalize(g), to.generalize(g));
  }

  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof FunctionType)) {
      throw new TypeMismatch(this, b);
    }
    final var ft = (FunctionType) b;
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
    return new FunctionType((ProductType) from.substitute(v, t), to.substitute(v, t));
  }

  public boolean occurs(TypeVariable v) {
    return from.occurs(v) || to.occurs(v);
  }

  @Override
  public Type wiggle(Substitution wiggled, UnificationContext context) {
    return new FunctionType(
        (ProductType) from.wiggle(wiggled, context), to.wiggle(wiggled, context));
  }

  @Override
  public Set<TypeVariable> variables() {
    return Sets.union(from.variables(), to.variables());
  }

  @Override
  public String toHaskell() {
    return (from.size() > 0 ? from.toCurriedHaskell() + " -> " : "") + to.toHaskell();
  }

  @Override
  public String toJava() {
    throw bug();
  }

  @Override
  public String toString() {
    return from + " → " + to;
  }
}
