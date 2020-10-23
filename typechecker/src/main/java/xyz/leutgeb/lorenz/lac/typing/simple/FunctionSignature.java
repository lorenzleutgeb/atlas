package xyz.leutgeb.lorenz.lac.typing.simple;

import java.util.Collections;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;

@Value
public final class FunctionSignature {
  private final FunctionType type;
  private final Set<TypeConstraint> constraints;
  private final Optional<CombinedFunctionAnnotation> annotation;

  private FunctionSignature(FunctionType type) {
    this.type = type;
    this.constraints = Collections.emptySet();
    this.annotation = Optional.empty();
  }

  public FunctionSignature(Type types) {
    this(new FunctionType(types));
  }

  public FunctionSignature(Set<TypeConstraint> constraints, Type... types) {
    this.constraints = constraints;
    this.type = new FunctionType(types);
    this.annotation = Optional.empty();
  }

  public FunctionSignature(Set<TypeConstraint> constraints, FunctionType type) {
    this.constraints = constraints;
    this.type = type;
    this.annotation = Optional.empty();
  }

  public FunctionSignature(
      Set<TypeConstraint> constraints,
      FunctionType type,
      Optional<CombinedFunctionAnnotation> annotation) {
    this.type = type;
    this.constraints = constraints;
    this.annotation = annotation;
  }

  public FunctionSignature wiggle(Substitution wiggled, UnificationContext problem) {
    final FunctionType wiggledType = (FunctionType) type.wiggle(wiggled, problem);
    return new FunctionSignature(
        constraints.stream()
            .map(typeConstraint -> typeConstraint.wiggle(wiggled, problem))
            .collect(Collectors.toSet()),
        wiggledType);
  }

  public FunctionSignature apply(Substitution substitution) {
    return new FunctionSignature(
        constraints.stream().map(tc -> tc.apply(substitution)).collect(Collectors.toSet()),
        (FunctionType) substitution.apply(type));
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (!constraints.isEmpty()) {
      final var omitParens = constraints.size() == 1;
      if (!omitParens) {
        sb.append("(");
      }
      sb.append(constraints.stream().map(Objects::toString).collect(Collectors.joining(", ")));
      if (!omitParens) {
        sb.append(")");
      }
      sb.append(" ⇒ ");
    }

    sb.append(type.getFrom());

    sb.append(" → ");
    sb.append(type.getTo());

    if (annotation.isPresent()) {
      sb.append(" | ");
      sb.append(annotation.get());
    }
    return sb.toString();
  }

  public String toHaskell() {
    StringBuilder sb = new StringBuilder();
    if (!constraints.isEmpty()) {
      final var omitParens = constraints.size() == 1;
      if (!omitParens) {
        sb.append("(");
      }
      sb.append(
          constraints.stream().map(TypeConstraint::toHaskell).collect(Collectors.joining(", ")));
      if (!omitParens) {
        sb.append(")");
      }
      sb.append(" => ");
    }
    sb.append(type.toHaskell());
    return sb.toString();
  }

  public boolean equals(final Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof FunctionSignature)) {
      return false;
    }
    final FunctionSignature other = (FunctionSignature) o;
    final Object this$type = this.getType();
    final Object other$type = other.getType();
    if (this$type == null ? other$type != null : !this$type.equals(other$type)) {
      return false;
    }
    final Object this$constraints = this.getConstraints();
    final Object other$constraints = other.getConstraints();
    if (this$constraints == null
        ? other$constraints != null
        : !this$constraints.equals(other$constraints)) {
      return false;
    }
    // final Object this$annotation = this.getAnnotation();
    // final Object other$annotation = other.getAnnotation();
    // if (this$annotation == null ? other$annotation != null :
    // !this$annotation.equals(other$annotation)) {
    // return false;
    // }
    return true;
  }

  public int hashCode() {
    final int PRIME = 59;
    int result = 1;
    final Object $type = this.getType();
    result = result * PRIME + ($type == null ? 43 : $type.hashCode());
    final Object $constraints = this.getConstraints();
    result = result * PRIME + ($constraints == null ? 43 : $constraints.hashCode());
    // final Object $annotation = this.getAnnotation();
    // result = result * PRIME + ($annotation == null ? 43 : $annotation.hashCode());
    return result;
  }
}
