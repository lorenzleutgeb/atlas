package xyz.leutgeb.lorenz.atlas.typing.simple;

import jakarta.json.Json;
import jakarta.json.JsonValue;
import java.util.Collections;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;

@Value
public class FunctionSignature {
  FunctionType type;
  Set<TypeConstraint> constraints;
  Optional<CombinedFunctionAnnotation> annotation;

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

  public JsonValue toJson() {
    return Json.createValue(toString());
    /*
    final var builder = Json.createObjectBuilder();
    builder.add("type", type.toJson());
    final var constraintBuilder = Json.createArrayBuilder();
    constraints.stream().map(TypeConstraint::toJson).forEach(constraintBuilder::add);
    builder.add("constraints", constraintBuilder.build());
    annotation.ifPresent(combinedFunctionAnnotation -> builder.add("annotation", combinedFunctionAnnotation.toJson()));
    return builder.build();
     */
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
      sb.append(constraints.stream().map(Objects::toString).sorted().collect(Collectors.joining(", ")));
      if (!omitParens) {
        sb.append(")");
      }
      sb.append(" ⇒ ");
    }

    sb.append(type.getFrom());

    if (type.getFrom().size() > 0) {
      sb.append(" → ");
    }
    sb.append(type.getTo());

    if (annotation.isPresent()) {
      sb.append(" | ");
      sb.append(annotation.get());
    }
    return sb.toString();
  }

  public FunctionSignature sealConstraints() {
    return new FunctionSignature(Collections.unmodifiableSet(constraints), type, annotation);
  }

  public void addConstraints(Set<TypeConstraint> constraints) {
    this.constraints.addAll(constraints);
  }

  public void addConstraint(TypeConstraint constraint) {
    this.constraints.add(constraint);
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

  public boolean simplePartEquals(final Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof final FunctionSignature other)) {
      return false;
    }
    if (!Objects.equals(this.getType(), other.getType())) {
      return false;
    }
    return Objects.equals(this.getConstraints(), other.getConstraints());
  }

  public boolean equals(final Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof final FunctionSignature other)) {
      return false;
    }
    return this.simplePartEquals(other)
        && Objects.equals(this.getAnnotation(), other.getAnnotation());
  }

  public int hashCode() {
    final int PRIME = 59;
    int result = 1;
    final Object $type = this.getType();
    result = result * PRIME + ($type == null ? 43 : $type.hashCode());
    final Object $constraints = this.getConstraints();
    result = result * PRIME + ($constraints == null ? 43 : $constraints.hashCode());
    final Object $annotation = this.getAnnotation();
    result = result * PRIME + ($annotation == null ? 43 : $annotation.hashCode());
    return result;
  }
}
