package xyz.leutgeb.lorenz.logs.type;

import java.util.Arrays;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;
import xyz.leutgeb.lorenz.logs.unification.Generalizer;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

@Data
@EqualsAndHashCode(callSuper = false)
public class TypeVariable extends Type {
  public static final TypeVariable ALPHA = new TypeVariable("α");
  public static final TypeVariable BETA = new TypeVariable("β");
  public static final TypeVariable GAMMA = new TypeVariable("γ");
  public static final TypeVariable DELTA = new TypeVariable("δ");
  public static final TypeVariable EPSILON = new TypeVariable("ε");
  public static final TypeVariable LAMBDA = new TypeVariable("λ");
  public static final TypeVariable MU = new TypeVariable("μ");
  public static final TypeVariable NU = new TypeVariable("ν");
  public static final TypeVariable PI = new TypeVariable("π");
  public static final TypeVariable OMEGA = new TypeVariable("ω");
  public static final TypeVariable RHO = new TypeVariable("ρ");
  public static final TypeVariable SIGMA = new TypeVariable("σ");
  public static final TypeVariable TAU = new TypeVariable("τ");
  public static final TypeVariable XI = new TypeVariable("ξ");

  public static final TypeVariable[] GREEK =
      new TypeVariable[] {
        ALPHA, BETA, GAMMA, DELTA, EPSILON, LAMBDA, MU, NU, PI, OMEGA, RHO, SIGMA, TAU, XI
      };

  private final String name;

  @Override
  public UnificationVariable wiggle(
      Map<TypeVariable, UnificationVariable> wiggled, UnificationProblem context) {
    return wiggled.computeIfAbsent(this, k -> context.fresh());
  }

  @Override
  public Type substitute(TypeVariable v, Type t) {
    return v.equals(this) ? t : this;
  }

  public static boolean isGreek(Type t) {
    return Arrays.asList(GREEK).contains(t);
  }

  public String toString() {
    return name;
  }

  @Override
  public TypeVariable generalize(Generalizer generalizer) {
    return this;
  }
}
