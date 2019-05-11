package xyz.leutgeb.lorenz.logs.type;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Equivalence;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.TypeMismatch;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Value
@EqualsAndHashCode(callSuper = false)
public class TreeType extends Type {
  TypeVariable elementType;

  public TreeType(TypeVariable elementType) {
    this.elementType = elementType;
  }

  @Override
  public String toString() {
    return "T " + elementType;
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
    Type substitute = elementType.substitute(v, t);
    if (!(substitute instanceof TypeVariable)) {
      throw new RuntimeException("this type of tree cannot be constructed");
    }
    return new TreeType((TypeVariable) substitute);
  }

  @Override
  public Type wiggle(Map<TypeVariable, UnificationVariable> wiggled, UnificationProblem context) {
    return new TreeType(elementType.wiggle(wiggled, context));
  }

  @Override
  public Type generalize(Generalizer g) {
    return new TreeType(elementType.generalize(g));
  }

  @Override
  public boolean occurs(UnificationVariable b) {
    return elementType.equals(b);
  }
}
