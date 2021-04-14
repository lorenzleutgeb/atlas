package xyz.leutgeb.lorenz.atlas.typing.simple;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;

/**
 * Denotes that some signature (represented by a variable) must be a member of a signature class.
 * This is done by specifying a substitution that parametrizes the signature class.
 */
@Value
@Slf4j
public class TypeConstraint {
  TypeClass typeClass;

  List<Type> constrained;

  private TypeConstraint(TypeClass typeClass, List<Type> binding) {
    if (typeClass.getArity() != binding.size()) {
      throw new IllegalArgumentException("number of variables does not match length of binding");
    }
    if (binding.stream().anyMatch(t -> t instanceof TreeType) && !TypeClass.EQ.equals(typeClass)) {
      throw new IllegalArgumentException("cannot constrain the type Tree as " + typeClass);
    }
    this.constrained = binding;
    this.typeClass = typeClass;
  }

  public TypeConstraint(TypeClass typeClass, Type... binding) {
    this(typeClass, Arrays.asList(binding));
  }

  public static Set<TypeConstraint> minimize(Set<TypeConstraint> constraints) {
    var minimizedConstraints = new HashSet<TypeConstraint>();
    for (var constraint : constraints) {
      minimizedConstraints.removeIf(x -> !x.equals(constraint) && constraint.implies(x));
      if (minimizedConstraints.stream().noneMatch(x -> x.implies(constraint))) {
        minimizedConstraints.add(constraint);
      }
    }
    return minimizedConstraints;
  }

  public static TypeConstraint eq(Type alpha) {
    return new TypeConstraint(TypeClass.EQ, alpha);
  }

  public static TypeConstraint ord(Type alpha) {
    return new TypeConstraint(TypeClass.ORD, alpha);
  }

  public boolean appliesTo(Type type) {
    if (typeClass.getArity() != 1) {
      throw new UnsupportedOperationException();
    }
    return type.equals(constrained.get(0));
  }

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

    for (var precondition : typeClass.getPreconditions()) {
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

  public TypeConstraint wiggle(Substitution wiggled, UnificationContext context) {
    return new TypeConstraint(
        typeClass,
        constrained.stream().map(t -> t.wiggle(wiggled, context)).collect(Collectors.toList()));
  }
}
