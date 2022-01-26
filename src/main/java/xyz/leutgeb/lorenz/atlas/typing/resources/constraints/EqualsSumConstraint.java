package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static com.google.common.collect.Sets.union;
import static guru.nidi.graphviz.model.Link.to;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toSet;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.sosy_lab.common.rationals.Rational;
import org.sosy_lab.java_smt.api.BooleanFormula;
import org.sosy_lab.java_smt.api.FormulaManager;
import org.sosy_lab.java_smt.api.NumeralFormula;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Value
@EqualsAndHashCode(callSuper = true)
public class EqualsSumConstraint extends Constraint {
  Coefficient left;
  List<Coefficient> sum;

  public EqualsSumConstraint(
      Coefficient left, Collection<Coefficient> sumCollection, String reason) {
    super(reason);
    List<Coefficient> sum = new ArrayList<>(sumCollection);
    if (sum.isEmpty()) {
      sum = singletonList(ZERO);
    }
    if (sum.size() == 2) {
      if (ZERO.equals(sum.get(0))) {
        sum = List.of(sum.get(1));
      } else if (ZERO.equals(sum.get(1))) {
        sum = List.of(sum.get(0));
      }
    }
    this.left = left;
    this.sum = sum;
  }

  @Override
  public String toString() {
    return left + " = " + sum.stream().map(Object::toString).collect(Collectors.joining(" + "));
    // return left + " = Σ" + sum;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    if (sum.size() == 1) {
      return ctx.mkEq(left.encode(ctx, coefficients), pick(sum).encode(ctx, coefficients));
    }

    final ArithExpr[] encodedSum =
        sum.stream().map(c -> c.encode(ctx, coefficients)).toArray(ArithExpr[]::new);

    return ctx.mkEq(left.encode(ctx, coefficients), ctx.mkAdd(encodedSum));
  }

  @Override
  public BooleanFormula encode(
      FormulaManager manager,
      Map<UnknownCoefficient, NumeralFormula.RationalFormula> coefficients) {
    if (sum.size() == 1) {
      return manager
          .getRationalFormulaManager()
          .equal(
              left.encode(manager.getRationalFormulaManager(), coefficients),
              pick(sum).encode(manager.getRationalFormulaManager(), coefficients));
    }

    final NumeralFormula.RationalFormula encodedSum =
        sum.stream()
            .map(c -> c.encode(manager.getRationalFormulaManager(), coefficients))
            .reduce(
                manager.getRationalFormulaManager().makeNumber(Rational.ONE),
                manager.getRationalFormulaManager()::add);

    return manager
        .getRationalFormulaManager()
        .equal(left.encode(manager.getRationalFormulaManager(), coefficients), encodedSum);
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var aux = objectNode(this, "Σ", "_aux").with("style", "dashed").with("shape", "hexagon");

    if (sum.stream().map(Coefficient::canonical).anyMatch(Predicate.not(nodes::containsKey))) {
      throw new RuntimeException("missing node for some coefficient");
    }

    var link =
        sum.stream()
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
    return new EqualsSumConstraint(
        left,
        sum.stream().map(c -> c.replace(target, replacement)).collect(Collectors.toList()),
        getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return union(
        singleton(left.canonical()), sum.stream().map(Coefficient::canonical).collect(toSet()));
  }
}
