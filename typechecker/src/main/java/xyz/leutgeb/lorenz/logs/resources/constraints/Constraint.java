package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;

public abstract class Constraint {
  protected final int id;
  private final Expression source;

  protected Constraint(int id, Expression source) {
    this.id = id;
    this.source = source;
    source.addPrecondition(this);
  }

  public int getId() {
    return id;
  }

  public abstract BoolExpr encode(Context ctx, RealExpr[] coefficients);

  protected String prefixed(String suffix) {
    return "(" + id + ": " + suffix + ")";
  }

  public abstract Graph toGraph(Graph graph, Map<Coefficient, Node> nodes);
}
