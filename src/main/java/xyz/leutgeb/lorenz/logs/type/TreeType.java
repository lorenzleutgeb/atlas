package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;
import xyz.leutgeb.lorenz.logs.unification.UnificiationProblem;

@Value
@EqualsAndHashCode(callSuper = false)
public class TreeType extends Type {
  Type elementType;

  @Override
  public String toString() {
    return "(Tree " + elementType + ")";
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
  public Type substitute(TypeVariable v, Type t) {
    return new TreeType(elementType.substitute(v, t));
  }

  @Override
  public Type wiggle(Map<TypeVariable, UnificationVariable> wiggled, UnificiationProblem context) {
    return new TreeType(elementType.wiggle(wiggled, context));
  }

  @Override
  public Type generalize(Generalizer g) {
    return new TreeType(elementType.generalize(g));
  }

  /*
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    TreeType treeType = (TreeType) o;

    return elementType != null ? elementType.equals(treeType.elementType) : treeType.elementType == null;
  }

  @Override
  public int hashCode() {
    return elementType != null ? elementType.hashCode() : 0;
  }
   */
}
