package xyz.leutgeb.lorenz.logs.typing;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;

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
  // Substitution substitution;

  List<Type> constrained;

  // Set<TypeConstraint> preconditions;
  // String name;
  // List<TypeVar> arguments;

  /*
  public TypeConstraint(TypeClass typeClass, Substitution substitution) {
    if (!substitution.isIdentity()) {
      for (var v : typeClass.getVariables()) {
        if (!substitution.isInDomain(v)) {
          throw new IllegalArgumentException("variable " + v + " is not in domain of substitution");
        }
      }
      // if (typeClass.getVariables().size() == 1 &&
      // (substitution.apply(typeClass.getVariables().get(0)) instanceof TreeType)) {
      //  throw new IllegalArgumentException("trees cannot be compared!");
      // }
    }
    this.typeClass = typeClass;
    this.substitution = substitution;
  }
   */

  public TypeConstraint(TypeClass typeClass, List<Type> binding) {
    if (typeClass.getArity() != binding.size()) {
      throw new IllegalArgumentException("number of variables does not match length of binding");
    }
    this.constrained = binding;
    this.typeClass = typeClass;
  }

  public TypeConstraint(TypeClass typeClass, Type... binding) {
    if (typeClass.getArity() != binding.length) {
      throw new IllegalArgumentException("number of variables does not match length of binding");
    }
    this.constrained = Arrays.asList(binding);
    this.typeClass = typeClass;
  }

  public boolean appliesTo(Type type) {
    if (typeClass.getArity() != 1) {
      throw new UnsupportedOperationException();
    }
    return type.equals(constrained.get(0));
  }
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
    final var substitutedPart =
        constrained.stream().map(Object::toString).collect(Collectors.joining(" "));
    final var result = typeClass.getName() + " " + substitutedPart;
    return substitutedPart.contains(" ") ? "(" + result + ")" : result;
  }

  public TypeConstraint apply(Substitution substitution) {
    return new TypeConstraint(
        this.typeClass, this.constrained.stream().map(substitution).collect(Collectors.toList()));
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
      if (!precondition.equals(other.typeClass)) {
        continue;
      }
      if (constrained.equals(other.constrained)) {
        return true;
      }
      if (new TypeConstraint(precondition, constrained).implies(other)) {
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
    return constrained.equals(that.constrained);
  }

  @Override
  public int hashCode() {
    int result = typeClass.hashCode();
    result = 31 * result + constrained.hashCode();
    return result;
  }

  public String toHaskell() {
    return typeClass.getName()
        + " "
        + constrained.stream()
            .map(Type::toHaskell)
            .map(x -> x.contains(" ") ? "(" + x + ")" : x)
            .collect(Collectors.joining(" "));
  }

  public TypeConstraint wiggle(Substitution wiggled, UnificationProblem context) {
    return new TypeConstraint(
        typeClass,
        constrained.stream().map(t -> t.wiggle(wiggled, context)).collect(Collectors.toList()));
  }
}
