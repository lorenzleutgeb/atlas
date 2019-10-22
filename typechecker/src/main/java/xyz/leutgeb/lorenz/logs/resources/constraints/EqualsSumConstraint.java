package xyz.leutgeb.lorenz.logs.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.LinkTarget;
import guru.nidi.graphviz.model.Node;
import java.util.Collection;
import java.util.Map;
import lombok.Value;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

@Value
public class EqualsSumConstraint extends Constraint {
  Coefficient left;
  Collection<Coefficient> sum;

  public EqualsSumConstraint(
      int id, Expression source, Coefficient left, Collection<Coefficient> sum) {
    super(id, source);
    this.left = left;
    this.sum = sum;
  }

  @Override
  public String toString() {
    return prefixed(left + " = Σ" + sum);
  }

  @Override
  public BoolExpr encode(Context ctx, RealExpr[] coefficients) {
    final ArithExpr[] encodedSum =
        sum.stream().map(c -> c.encode(ctx, coefficients)).toArray(ArithExpr[]::new);

    return ctx.mkEq(left.encode(ctx, coefficients), ctx.mkAdd(encodedSum));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var link = new LinkTarget[sum.size()];
    int i = 0;
    for (var c : sum) {
      link[i++] = to(nodes.get(c)).with(Color.CYAN);
    }
    return graph.with(nodes.get(left).link(link));
  }
}
