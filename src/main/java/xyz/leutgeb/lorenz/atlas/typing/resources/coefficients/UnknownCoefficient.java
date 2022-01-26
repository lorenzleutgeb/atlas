package xyz.leutgeb.lorenz.atlas.typing.resources.coefficients;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import com.microsoft.z3.RealSort;
import java.util.Map;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.sosy_lab.java_smt.api.NumeralFormula;
import org.sosy_lab.java_smt.api.RationalFormulaManager;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Value
@EqualsAndHashCode
@AllArgsConstructor(access = AccessLevel.PACKAGE)
public class UnknownCoefficient implements Coefficient {
  private static final IntIdGenerator ID_GENERATOR = IntIdGenerator.fromOneInclusive();

  String name;
  boolean negated;
  boolean maybeNegative;

  public static UnknownCoefficient raw(String name) {
    return new UnknownCoefficient(name);
  }

  public static UnknownCoefficient fromPrefix(String prefix) {
    return new UnknownCoefficient(prefix + ID_GENERATOR.next());
  }

  private UnknownCoefficient(String name) {
    if (name == null || name.isBlank()) {
      throw new IllegalArgumentException("name cannot be null or blank");
    }
    this.name = name;
    this.negated = false;
    this.maybeNegative = false;
  }

  @Override
  public UnknownCoefficient negate() {
    return new UnknownCoefficient(name, !negated, maybeNegative);
  }

  @Override
  public String toString() {
    return (negated ? "-" : "") + name;
  }

  public ArithExpr<RealSort> encode(Context ctx, Map<UnknownCoefficient, RealExpr> coefficients) {
    final var inner = coefficients.get(this.canonical());
    return negated ? ctx.mkUnaryMinus(inner) : inner;
  }

  @Override
  public NumeralFormula.RationalFormula encode(
      RationalFormulaManager manager,
      Map<UnknownCoefficient, NumeralFormula.RationalFormula> coefficients) {
    final var inner = coefficients.get(this.canonical());
    return negated ? manager.negate(inner) : inner;
  }

  @Override
  public Coefficient replace(Coefficient target, Coefficient replacement) {
    if (negated) {
      throw bug("don't know how to replace negated coefficient");
    }
    if (target instanceof final UnknownCoefficient unknownCoefficient) {
      if (unknownCoefficient.name.equals(name)) {
        return replacement;
      }
    }
    return this;
  }

  @Override
  public UnknownCoefficient canonical() {
    return negated ? negate() : this;
  }
}
