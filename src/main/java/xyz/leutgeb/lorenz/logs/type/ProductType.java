package xyz.leutgeb.lorenz.logs.type;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

public class ProductType extends Type {
  private final List<Type> elements;

  public ProductType(List<Type> elements) {
    this.elements = elements;
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof ProductType)) {
      throw new TypeMismatch(this, b);
    }
    var pt = (ProductType) b;
    if (pt.elements.size() != elements.size()) {
      throw new TypeMismatch(this, b);
    }
    var result = new ArrayList<Equivalence>(elements.size());
    for (int i = 0; i < elements.size(); i++) {
      result.add(new Equivalence(elements.get(i), pt.elements.get(i)));
    }
    return result;
  }

  @Override
  public Type substitute(UnificationVariable v, Type t) {
    return new ProductType(
        elements.stream().map(x -> x.substitute(v, t)).collect(Collectors.toList()));
  }

  @Override
  public Type generalize(Generalizer g) {
    return new ProductType(
        elements.stream().map(x -> x.generalize(g)).collect(Collectors.toList()));
  }

  @Override
  public String toString() {
    return "(" + elements.stream().map(Object::toString).collect(Collectors.joining(" ⨯ ")) + ")";
  }
}
