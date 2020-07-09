package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Set;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

@Value
public class NegationConstraint extends Constraint {
  Constraint element;

  public NegationConstraint(Constraint element, String reason) {
    super(reason);
    this.element = element;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
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

  @Override
  public void markCoreByTrackings(Set<String> trackings) {
    throw new UnsupportedOperationException("don't know how to mark core of negated constraints");
  }
}
