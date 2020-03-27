package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Map;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;

public abstract class Coefficient {
  public Coefficient add(Coefficient other, ConstraintSystemSolver context) {
    throw new UnsupportedOperationException();
  }

  public abstract RealExpr encode(Context ctx, Map<Coefficient, RealExpr> coefficients);

  public abstract Coefficient replace(Coefficient target, Coefficient replacement);
}
