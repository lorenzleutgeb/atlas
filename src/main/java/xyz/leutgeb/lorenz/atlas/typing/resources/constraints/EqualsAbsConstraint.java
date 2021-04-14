package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static java.util.Collections.singleton;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import com.google.common.collect.BiMap;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.solving.ConstraintSystemSolver;

@Data
@Slf4j
@EqualsAndHashCode(callSuper = true)
public class EqualsAbsConstraint extends Constraint {
  @NonNull protected final Coefficient left, right;

  public EqualsAbsConstraint(Coefficient left, Coefficient right, String reason) {
    super(reason);
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    if (left.equals(right)) {
      // log.warn("Creating an equality constraint for equal coefficients.");
    }
    this.left = left;
    this.right = right;
  }

  public BoolExpr encode(
      Context ctx,
      BiMap<UnknownCoefficient, ArithExpr> coefficients,
      ConstraintSystemSolver.Domain domain) {
    return ctx.mkEq(
        left.encode(ctx, coefficients, domain),
        ctx.mkApp(
            ctx.mkFuncDecl("abs", ctx.getIntSort(), ctx.getIntSort()),
            right.encode(ctx, coefficients, domain)));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    throw bug("cannot translate abs to graph");
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new EqualsAbsConstraint(
        left.replace(target, replacement), right.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    final var leftCanonical = left.canonical();
    final var rightCanonical = right.canonical();

    if (leftCanonical.equals(rightCanonical)) {
      return singleton(leftCanonical);
    }

    return Set.of(left.canonical(), right.canonical());
  }

  @Override
  public boolean known() {
    return (left instanceof KnownCoefficient) && (right instanceof KnownCoefficient);
  }

  @Override
  protected boolean satisfiedInternal() {
    return left.equals(right);
  }

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
