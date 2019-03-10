package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.List;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Value
public class TreeType extends Type {
  Type elementType;

  @Override
  public String toString() {
    return "(T " + elementType + ")";
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof TreeType)) {
      throw new TypeMismatch(this, b);
    }

    var tree = (TreeType) b;
    return List.of(new Equivalence(elementType, tree.elementType));
  }

  @Override
  public Type substitute(UnificationVariable v, Type t) {
    return new TreeType(elementType.substitute(v, t));
  }

  @Override
  public Type generalize(Generalizer g) {
    return new TreeType(elementType.generalize(g));
  }
}
