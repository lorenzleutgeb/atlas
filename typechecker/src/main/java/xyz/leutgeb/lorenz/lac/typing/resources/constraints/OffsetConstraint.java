package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;
import static xyz.leutgeb.lorenz.lac.Util.objectNode;

import com.google.common.collect.BiMap;
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
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class OffsetConstraint extends EqualityConstraint {
  private final Fraction offset;

  public OffsetConstraint(Coefficient left, Coefficient right, Fraction offset, String reason) {
    super(left, right, reason);
    this.offset = offset;
  }

  @Override
  public String toString() {
    return left + " = " + right + " + " + offset;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    return ctx.mkEq(
        left.encode(ctx, coefficients),
        ctx.mkAdd(
            right.encode(ctx, coefficients),
            ctx.mkReal(offset.getNumerator(), offset.getDenominator())));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var aux = objectNode(this, "+ " + offset, "_aux").with("style", "dashed");

    return graph
        .with(nodes.get(left).link(highlight(to(aux).with(Color.BLUE4).with("dir", "none"))))
        .with(aux.link(highlight(to(nodes.get(right)).with(Color.RED).with("dir", "none"))));
  }
}
