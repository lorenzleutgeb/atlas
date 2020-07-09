package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import java.util.Map;

public interface Coefficient {
  RealExpr encode(Context ctx, Map<Coefficient, RealExpr> coefficients);

  Coefficient replace(Coefficient target, Coefficient replacement);

  Coefficient canonical();

  Coefficient negate();
}
