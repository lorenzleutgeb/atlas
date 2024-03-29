package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

/** @see GreaterThanOrEqualConstraint */
@Value
@EqualsAndHashCode(callSuper = true)
public class LessThanOrEqualConstraint extends Constraint {
  @NonNull protected Coefficient smaller, bigger;

  public LessThanOrEqualConstraint(
      @NonNull Coefficient smaller, @NonNull Coefficient bigger, String reason) {
    super(reason);
    this.smaller = smaller;
    this.bigger = bigger;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    return ctx.mkLe(smaller.encode(ctx, coefficients), bigger.encode(ctx, coefficients));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    // Arrow on the edge should go from big to small, like the '<=' symbol.
    return graph.with(
        nodes
            .get(smaller)
            .link(
                highlight(
                    to(nodes.get(bigger))
                        .with(Color.DARKOLIVEGREEN)
                        .with("dir", "back")
                        .with("arrowTail", "open")
                        .with("arrowHead", "open"))));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new LessThanOrEqualConstraint(
        smaller.replace(target, replacement), bigger.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    final var result = new HashSet<Coefficient>();
    result.add(smaller.canonical());
    result.add(bigger.canonical());
    return result;
    // return Set.of(left.canonical(), right.canonical());
  }

  @Override
  public String toString() {
    return smaller + " ≤ " + bigger;
  }
}
