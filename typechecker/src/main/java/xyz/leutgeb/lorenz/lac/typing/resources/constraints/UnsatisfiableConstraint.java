package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

public class UnsatisfiableConstraint extends Constraint {
  public UnsatisfiableConstraint(String reason) {
    super(reason);
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    return ctx.mkFalse();
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    // TODO
    return graph;
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return this;
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Collections.emptySet();
  }
}
