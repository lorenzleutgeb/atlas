package xyz.leutgeb.lorenz.atlas.typing.simple.types;

import static java.util.Arrays.asList;
import static java.util.Arrays.copyOf;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import com.google.common.collect.Sets;
import jakarta.json.Json;
import jakarta.json.JsonValue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.unification.Equivalence;
import xyz.leutgeb.lorenz.atlas.unification.Generalizer;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.TypeMismatch;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;

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

  public Collection<Equivalence> decompose(Type b, Source source) throws TypeMismatch {
    if (!(b instanceof final FunctionType ft)) {
      throw new TypeMismatch(this, b, source);
    }
    // Check lengths of "from" here, to catch errors early. Not strictly necessary, but helps.
    if (from.size() != ft.from.size()) {
      throw new TypeMismatch(from, ft.from, source);
    }
    final var result = new ArrayList<Equivalence>(2);
    if (!from.equals(ft.from)) {
      result.add(new Equivalence(from, ft.from, source));
    }
    if (!to.equals(ft.to)) {
      result.add(new Equivalence(to, ft.to, source));
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
  public JsonValue toJson() {
    final var builder = Json.createObjectBuilder();
    builder.add("from", from.toJson());
    builder.add("to", to.toJson());
    return builder.build();
  }

  @Override
  public String toString() {
    return from + " → " + to;
  }
}
