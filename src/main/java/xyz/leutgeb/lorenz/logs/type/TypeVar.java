package xyz.leutgeb.lorenz.logs.type;

import java.util.Map;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;
import xyz.leutgeb.lorenz.logs.unification.UnificiationProblem;

@Value
@EqualsAndHashCode(callSuper = true)
public class TypeVar extends Type {
  public static final TypeVar ALPHA = new TypeVar("α");
  public static final TypeVar BETA = new TypeVar("β");
  public static final TypeVar GAMMA = new TypeVar("γ");
  public static final TypeVar DELTA = new TypeVar("δ");
  public static final TypeVar EPSILON = new TypeVar("ε");
  public static final TypeVar LAMBDA = new TypeVar("λ");
  public static final TypeVar MU = new TypeVar("μ");
  public static final TypeVar NU = new TypeVar("ν");
  public static final TypeVar PI = new TypeVar("π");
  public static final TypeVar OMEGA = new TypeVar("ω");
  public static final TypeVar RHO = new TypeVar("ρ");
  public static final TypeVar SIGMA = new TypeVar("σ");
  public static final TypeVar TAU = new TypeVar("τ");
  public static final TypeVar XI = new TypeVar("ξ");

  public static final TypeVar[] GREEK =
      new TypeVar[] {
        ALPHA, BETA, GAMMA, DELTA, EPSILON, LAMBDA, MU, NU, PI, OMEGA, RHO, SIGMA, TAU, XI
      };

  String name;

  @Override
  public Type wiggle(Map<TypeVar, UnificationVariable> wiggled, UnificiationProblem context) {
    return wiggled.computeIfAbsent(this, k -> context.fresh());
  }

  public String toString() {
    return name;
  }
}
