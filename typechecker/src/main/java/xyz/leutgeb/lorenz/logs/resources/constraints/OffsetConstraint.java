package xyz.leutgeb.lorenz.logs.resources.constraints;

import static guru.nidi.graphviz.model.Factory.node;
import static guru.nidi.graphviz.model.Link.to;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class OffsetConstraint extends EqualityConstraint {
  private final Fraction offset;

  public OffsetConstraint(
      int id, Expression source, Coefficient left, Coefficient right, Fraction offset) {
    super(id, source, left, right);
    this.offset = offset;
  }

  public static OffsetConstraint increment(
      int id, Expression source, Coefficient left, Coefficient right) {
    return new OffsetConstraint(id, source, left, right, Fraction.ONE);
  }

  @Override
  public String toString() {
    return prefixed(left + " = " + right + " + " + offset);
  }

  @Override
  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    return ctx.mkEq(
        left.encode(ctx, coefficients),
        ctx.mkAdd(
            right.encode(ctx, coefficients),
            ctx.mkReal(offset.getNumerator(), offset.getDenominator())));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    Node plus = node("+");
    return graph.with(
        nodes.get(left).link(to(plus).with(Color.RED)),
        plus.link(to(nodes.get(right)), to(node(offset.toString())).with(Color.RED)));
  }
}
