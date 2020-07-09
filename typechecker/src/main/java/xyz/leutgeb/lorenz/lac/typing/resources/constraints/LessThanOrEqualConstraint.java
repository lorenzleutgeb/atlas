package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

/** @see GreaterThanOrEqualConstraint */
@Value
@EqualsAndHashCode(callSuper = true)
public class LessThanOrEqualConstraint extends Constraint {
  @NonNull protected Coefficient left, right;

  public LessThanOrEqualConstraint(
      @NonNull Coefficient left, @NonNull Coefficient right, String reason) {
    super(reason);
    this.left = left;
    this.right = right;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    return ctx.mkLe(left.encode(ctx, coefficients), right.encode(ctx, coefficients));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    // Arrow on the edge should go from big to small, like the '<=' symbol.
    return graph.with(
        nodes
            .get(left)
            .link(
                highlight(
                    to(nodes.get(right))
                        .with(Color.DARKOLIVEGREEN)
                        .with("dir", "back")
                        .with("arrowTail", "open")
                        .with("arrowHead", "open"))));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new LessThanOrEqualConstraint(
        left.replace(target, replacement), right.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Set.of(left.canonical(), right.canonical());
  }

  @Override
  public String toString() {
    return left + " ≤ " + right;
  }
}
