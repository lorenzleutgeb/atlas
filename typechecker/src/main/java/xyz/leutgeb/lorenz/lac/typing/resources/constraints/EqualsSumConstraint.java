package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static com.google.common.collect.Sets.union;
import static guru.nidi.graphviz.model.Link.to;
import static java.util.Collections.singleton;
import static xyz.leutgeb.lorenz.lac.Util.objectNode;

import com.google.common.collect.BiMap;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Link;
import guru.nidi.graphviz.model.LinkTarget;
import guru.nidi.graphviz.model.Node;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

@Value
@EqualsAndHashCode(callSuper = true)
public class EqualsSumConstraint extends Constraint {
  Coefficient left;
  Collection<Coefficient> sum;

  public EqualsSumConstraint(Coefficient left, Collection<Coefficient> sum) {
    this.left = left;
    this.sum = sum;
  }

  @Override
  public String toString() {
    return left + " = Σ" + sum;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    final ArithExpr[] encodedSum =
        sum.stream().map(c -> c.encode(ctx, coefficients)).toArray(ArithExpr[]::new);

    return ctx.mkEq(left.encode(ctx, coefficients), ctx.mkAdd(encodedSum));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var aux = objectNode(this, "Σ", "_aux").with("style", "dashed").with("shape", "hexagon");

    var link =
        sum.stream()
            .map(nodes::get)
            .map(Link::to)
            .map(x -> x.with(Color.ORANGERED))
            .map(this::highlight)
            .toArray(LinkTarget[]::new);

    return graph
        .with(nodes.get(left).link(highlight(to(aux).with(Color.BLUE4))))
        .with(aux.link(link));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new EqualsSumConstraint(
        left, sum.stream().map(c -> c.replace(target, replacement)).collect(Collectors.toList()));
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return union(singleton(left), new HashSet<>(sum));
  }
}
