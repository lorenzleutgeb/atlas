package xyz.leutgeb.lorenz.atlas.typing.simple.types;

import static java.util.Collections.emptySet;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import com.google.common.collect.Sets;
import jakarta.json.Json;
import jakarta.json.JsonValue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.atlas.unification.Equivalence;
import xyz.leutgeb.lorenz.atlas.unification.Generalizer;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.TypeMismatch;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;

@Value
@EqualsAndHashCode(callSuper = false)
public class ProductType implements Type {
  List<Type> elements;

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof ProductType pt)) {
      throw new TypeMismatch(this, b);
    }
    if (pt.size() != elements.size()) {
      throw new TypeMismatch(this, b);
    }
    var result = new ArrayList<Equivalence>(elements.size());
    for (int i = 0; i < elements.size(); i++) {
      if (!elements.get(i).equals(pt.elements.get(i))) {
        result.add(new Equivalence(elements.get(i), pt.elements.get(i)));
      }
    }
    return result;
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    return map(x -> x.substitute(v, t));
  }

  @Override
  public Type generalize(Generalizer g) {
    return map(x -> x.generalize(g));
  }

  @Override
  public Type wiggle(Substitution wiggled, UnificationContext context) {
    return map(x -> x.wiggle(wiggled, context));
  }

  @Override
  public Set<TypeVariable> variables() {
    return elements.stream().map(Type::variables).reduce(emptySet(), Sets::union);
  }

  @Override
  public String toHaskell() {
    if (elements.size() == 0) {
      return "()";
    }
    if (elements.size() == 1) {
      return elements.get(0).toHaskell();
    }
    return "(" + stream().map(Type::toHaskell).collect(Collectors.joining(", ")) + ")";
  }

  @Override
  public String toJava() {
    throw bug();
  }

  @Override
  public JsonValue toJson() {
    final var builder = Json.createObjectBuilder();
    builder.add("name", "Product");

    final var argumentsBuilder = Json.createArrayBuilder();
    elements.stream().map(Type::toJson).forEach(argumentsBuilder::add);
    builder.add("arguments", argumentsBuilder.build());

    return builder.build();
  }

  public String toCurriedHaskell() {
    if (elements.size() == 0) {
      return "";
    }
    if (elements.size() == 1) {
      return elements.get(0).toHaskell();
    }
    return stream().map(Type::toHaskell).collect(Collectors.joining(" -> "));
  }

  @Override
  public String toString() {
    return (elements.size() > 1 ? "(" : "")
        + stream().map(Object::toString).collect(Collectors.joining(" ⨯ "))
        + (elements.size() > 1 ? ")" : "");
  }

  @Override
  public boolean occurs(TypeVariable b) {
    return stream().anyMatch(x -> x.occurs(b));
  }

  private ProductType map(Function<Type, Type> f) {
    return new ProductType(stream().map(f).collect(Collectors.toList()));
  }

  private Stream<Type> stream() {
    return elements.stream();
  }

  public int size() {
    return elements.size();
  }

  public long treeSize() {
    return elements.stream().filter(element -> element instanceof TreeType).count();
  }
}
