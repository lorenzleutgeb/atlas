package xyz.leutgeb.lorenz.logs.resources.constraints;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import xyz.leutgeb.lorenz.logs.ast.Expression;

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
}
