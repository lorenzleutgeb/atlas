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
import java.util.Objects;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

@Data
@Log4j2
@EqualsAndHashCode(callSuper = true)
public class InequalityConstraint extends Constraint {
  @NonNull protected final Coefficient left, right;

  public InequalityConstraint(Coefficient left, Coefficient right) {
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    if (left.equals(right)) {
      throw new IllegalArgumentException(
          "cannot create equality constraint for equal coefficients");
    }
    this.left = left;
    this.right = right;
  }

  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    return ctx.mkNot(ctx.mkEq(left.encode(ctx, coefficients), right.encode(ctx, coefficients)));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var leftNode = toNode(left, nodes);
    var rightNode = toNode(right, nodes);
    Objects.requireNonNull(leftNode);
    Objects.requireNonNull(rightNode);
    return graph.with(
        leftNode.link(
            highlight(to(rightNode).with(Color.RED1).with("dir", "none").with("penwidth", "1.5"))));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new InequalityConstraint(
        left.replace(target, replacement), right.replace(target, replacement));
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Set.of(left, right);
  }

  @Override
  public String toString() {
    return left + " ≠ " + right;
  }
}
