package xyz.leutgeb.lorenz.logs.type;

import java.util.stream.Collectors;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.Substitution;

/**
 * Denotes that some signature (represented by a variable) must be a member of a signature class.
 * This is done by specifying a substitution that parametrizes the signature class.
 */
@Value
public class TypeConstraint {
  TypeClass typeClass;

  /**
   * Maps from variables in the declaration of the signature class to variables where the constraint
   * applies.
   */
  // TODO(lorenz.leutgeb): Maybe just a list would be enough?
  Substitution substitution;

  // Set<TypeConstraint> preconditions;
  // String name;
  // List<TypeVar> arguments;

  /*
  public TypeConstraint(Set<TypeConstraint> preconditions, String name, List<TypeVar> arguments) {
    for (var cond : preconditions) {
      for (var ty : cond.getArguments()) {
        if (!arguments.contains(ty)) {
          throw new IllegalArgumentException("free signature variable");
        }
      }
    }
    if (arguments.size() < 1) {
      throw new IllegalArgumentException("arguments must be non-empty");
    }
    if (name.isBlank()) {
      throw new IllegalArgumentException("name must be non-blank");
    }
    this.preconditions = preconditions;
    this.name = name;
    this.arguments = arguments;
  }
   */

  @Override
  public String toString() {
    return typeClass.getName()
        + " "
        + typeClass
            .getVariables()
            .stream()
            .map(substitution)
            .map(Object::toString)
            .collect(Collectors.joining(" "));
  }

  public TypeConstraint apply(Substitution substitution) {
    return new TypeConstraint(this.typeClass, this.substitution.compose(substitution));
  }

  public boolean implies(TypeConstraint other) {
    if (other.equals(this)) {
      return true;
    }

    if (typeClass.getPreconditions().isEmpty()) {
      return false;
    }

    // typeClass == ORD
    for (var precondition : typeClass.getPreconditions()) {
      // precondition == EQ
      if (!precondition.typeClass.equals(other.typeClass)) {
        continue;
      }
      if (precondition
          .substitution
          .compose(substitution)
          .equals(other.substitution, precondition.typeClass.getVariables())) {
        return true;
      }
      if (precondition.implies(other)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    TypeConstraint that = (TypeConstraint) o;

    if (!typeClass.equals(that.typeClass)) {
      return false;
    }
    return substitution.equals(that.substitution, typeClass.getVariables());
  }

  @Override
  public int hashCode() {
    int result = typeClass.hashCode();
    result = 31 * result + substitution.hashCode();
    return result;
  }
}
