package xyz.leutgeb.lorenz.lac.typing.simple.types;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.unification.*;

@Value
@EqualsAndHashCode(callSuper = false)
public class TreeType implements Type {
  TypeVariable elementType;

  public TreeType(TypeVariable elementType) {
    this.elementType = elementType;
  }

  @Override
  public String toString() {
    return "Tree " + elementType;
  }

  @Override
  public Collection<Equivalence> decompose(Type b) throws TypeMismatch {
    if (!(b instanceof TreeType)) {
      throw new TypeMismatch(this, b);
    }

    var tree = (TreeType) b;
    if (!elementType.equals(tree.elementType)) {
      return Collections.singletonList(new Equivalence(elementType, tree.elementType));
    }
    return Collections.emptyList();
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    var substitute = elementType.substitute(v, t);
    if (substitute instanceof TypeVariable) {
      // TODO: Remove cast once we're on Java 14?
      return new TreeType((TypeVariable) substitute);
    }
    throw new RuntimeException("this type of tree cannot be constructed");
  }

  @Override
  public Type wiggle(Substitution wiggled, UnificationContext context) {
    return new TreeType(elementType.wiggle(wiggled, context));
  }

  @Override
  public Set<TypeVariable> variables() {
    return elementType.variables();
  }

  @Override
  public String toHaskell() {
    return "Tree " + elementType.toHaskell();
  }

  @Override
  public Type generalize(Generalizer g) {
    return new TreeType(elementType.generalize(g));
  }

  @Override
  public boolean occurs(TypeVariable b) {
    return elementType.equals(b);
  }
}
