package xyz.leutgeb.lorenz.lac.typing.resources.coefficients;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import java.util.Map;

public interface Coefficient {
  ArithExpr encode(Context ctx, Map<UnknownCoefficient, ArithExpr> coefficients);

  Coefficient replace(Coefficient target, Coefficient replacement);

  Coefficient canonical();

  Coefficient negate();
}
