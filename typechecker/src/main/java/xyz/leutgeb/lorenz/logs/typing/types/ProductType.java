package xyz.leutgeb.lorenz.logs.typing.types;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
public class ProductType extends Type {
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
  public Type wiggle(Substitution wiggled, UnificationProblem context) {
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
    return "(" + stream().map(Object::toString).collect(Collectors.joining(" ⨯ ")) + ")";
  }

  @Override
  public boolean occurs(UnificationVariable b) {
    return stream().anyMatch(x -> x.occurs(b));
  }

  private ProductType map(Function<Type, Type> f) {
    return new ProductType(stream().map(f).collect(Collectors.toList()));
  }

  public Stream<Type> stream() {
    return elements.stream();
  }

  public int size() {
    return elements.size();
  }
}
