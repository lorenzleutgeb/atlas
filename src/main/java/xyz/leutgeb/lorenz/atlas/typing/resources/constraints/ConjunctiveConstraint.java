package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;
import static xyz.leutgeb.lorenz.atlas.util.Util.pick;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.sosy_lab.java_smt.api.BooleanFormula;
import org.sosy_lab.java_smt.api.FormulaManager;
import org.sosy_lab.java_smt.api.NumeralFormula;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Value
@EqualsAndHashCode(callSuper = false)
public class ConjunctiveConstraint extends Constraint {
  List<Constraint> elements;

  public ConjunctiveConstraint(List<Constraint> elements, String reason) {
    super(reason);
    Objects.requireNonNull(elements);
    this.elements = elements;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    if (elements.isEmpty()) {
      return ctx.mkTrue();
    }
    if (elements.size() == 1) {
      return pick(elements).encode(ctx, coefficients);
    }
    return ctx.mkAnd(
        elements.stream()
            .map(element -> element.encode(ctx, coefficients))
            .toArray(BoolExpr[]::new));
  }

  @Override
  public BooleanFormula encode(
      FormulaManager manager,
      Map<UnknownCoefficient, NumeralFormula.RationalFormula> coefficients) {
    if (elements.isEmpty()) {
      return manager.getBooleanFormulaManager().makeTrue();
    }
    if (elements.size() == 1) {
      return pick(elements).encode(manager, coefficients);
    }
    return manager
        .getBooleanFormulaManager()
        .and(elements.stream().map(element -> element.encode(manager, coefficients)).toList());
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    throw new UnsupportedOperationException("cannot convert conjunctive constraint to graph");
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new ConjunctiveConstraint(
        elements.stream().map(element -> element.replace(target, replacement)).collect(toList()),
        getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return elements.stream()
        .map(Constraint::occurringCoefficients)
        .flatMap(Set::stream)
        .collect(toSet());
  }

  @Override
  public String toString() {
    if (elements.isEmpty()) {
      return "true";
    }
    return elements.stream()
        .map(Constraint::toStringWithReason)
        .collect(Collectors.joining("\n\t∧ ", "(\n\t", "\n\n)"));
  }

  @Override
  public Stream<Constraint> children() {
    return elements.stream();
  }
}
