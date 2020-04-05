package xyz.leutgeb.lorenz.lac.typing.simple;

import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;

@Value
public class FunctionSignature {
  FunctionType type;
  Set<TypeConstraint> constraints;

  private FunctionSignature(FunctionType type) {
    this.type = type;
    this.constraints = Collections.emptySet();
  }

  public FunctionSignature(Type types) {
    this(new FunctionType(types));
  }

  public FunctionSignature(Set<TypeConstraint> constraints, Type... types) {
    this.constraints = constraints;
    this.type = new FunctionType(types);
  }

  public FunctionSignature(Set<TypeConstraint> constraints, FunctionType type) {
    this.constraints = constraints;
    this.type = type;
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
    sb.append(type);
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
}
