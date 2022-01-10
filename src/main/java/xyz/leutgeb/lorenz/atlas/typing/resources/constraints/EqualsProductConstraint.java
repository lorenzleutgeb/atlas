package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static com.google.common.collect.Sets.union;
import static guru.nidi.graphviz.model.Link.to;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toSet;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.atlas.util.Util.objectNode;
import static xyz.leutgeb.lorenz.atlas.util.Util.pick;

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
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Value
@EqualsAndHashCode(callSuper = true)
public class EqualsProductConstraint extends Constraint {
  Coefficient left;
  Collection<Coefficient> product;

  public EqualsProductConstraint(Coefficient left, Collection<Coefficient> product, String reason) {
    super(reason);
    if (product.isEmpty()) {
      product = singletonList(ONE);
    }
    if (product.contains(KnownCoefficient.ZERO)) {
      product = singletonList(ZERO);
    }
    this.left = left;
    this.product = product;
  }

  public String toString() {
    return left + " = " + product.stream().map(Object::toString).collect(Collectors.joining(" ⋅ "));
    // return left + " = Π" + product;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    if (product.size() == 1) {
      return ctx.mkEq(left.encode(ctx, coefficients), pick(product).encode(ctx, coefficients));
    }

    final ArithExpr[] encodedSum =
        product.stream().map(c -> c.encode(ctx, coefficients)).toArray(ArithExpr[]::new);

    return ctx.mkEq(left.encode(ctx, coefficients), ctx.mkMul(encodedSum));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var aux = objectNode(this, "Π", "_aux").with("style", "dashed").with("shape", "hexagon");

    if (product.stream().map(Coefficient::canonical).anyMatch(Predicate.not(nodes::containsKey))) {
      throw new RuntimeException("missing node for product coefficient");
    }

    var link =
        product.stream()
            .map(Coefficient::canonical)
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
    return new EqualsProductConstraint(
        left,
        product.stream().map(c -> c.replace(target, replacement)).collect(Collectors.toList()),
        getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return union(
        singleton(left.canonical()), product.stream().map(Coefficient::canonical).collect(toSet()));
  }
}
