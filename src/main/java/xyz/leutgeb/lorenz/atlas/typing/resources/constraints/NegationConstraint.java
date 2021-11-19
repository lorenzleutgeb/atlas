package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Set;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Value
@Slf4j
public class NegationConstraint extends Constraint {
  Constraint element;

  public NegationConstraint(Constraint element, String reason) {
    super(reason);
    this.element = element;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    return ctx.mkNot(element.encode(ctx, coefficients));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    // TODO
    return graph;
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new NegationConstraint(element.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return element.occurringCoefficients();
  }

  @Override
  public String toString() {
    return "¬(" + element + ")";
  }
}
