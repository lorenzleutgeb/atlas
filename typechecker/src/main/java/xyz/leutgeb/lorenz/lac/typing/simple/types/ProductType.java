package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
public class ProductType implements Type {
  List<Type> elements;

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof ProductType)) {
      throw new TypeMismatch(this, b);
    }
    var pt = (ProductType) b;
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
  public String toHaskell() {
    if (elements.size() == 0) {
      return "()";
    }
    if (elements.size() == 1) {
      return elements.get(0).toHaskell();
    }
    return "(" + stream().map(Type::toHaskell).collect(Collectors.joining(", ")) + ")";
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
