package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;
import static xyz.leutgeb.lorenz.atlas.util.Util.objectNode;

import com.google.common.collect.BiMap;
import com.google.common.collect.Sets;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.fraction.Fraction;
import org.sosy_lab.java_smt.api.BooleanFormula;
import org.sosy_lab.java_smt.api.FormulaManager;
import org.sosy_lab.java_smt.api.NumeralFormula;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Data
@EqualsAndHashCode(callSuper = true)
public class OffsetConstraint extends EqualityConstraint {
  private final Coefficient offset;

  public OffsetConstraint(Coefficient left, Coefficient right, Fraction offset, String reason) {
    this(left, right, new KnownCoefficient(offset), reason);
  }

  public OffsetConstraint(Coefficient left, Coefficient right, Coefficient offset, String reason) {
    super(left, right, reason);
    this.offset = offset;
  }

  @Override
  public String toString() {
    return left + " = " + right + " + " + offset;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    /*
    if (offset instanceof KnownCoefficient
        && ((KnownCoefficient) offset).getValue().getDenominator() != 1) {
      throw bug("oops");
    }
     */
    return ctx.mkEq(
        left.encode(ctx, coefficients),
        ctx.mkAdd(right.encode(ctx, coefficients), offset.encode(ctx, coefficients)));
  }

  @Override
  public BooleanFormula encode(
      FormulaManager manager,
      Map<UnknownCoefficient, NumeralFormula.RationalFormula> coefficients) {
    return manager
        .getRationalFormulaManager()
        .equal(
            left.encode(manager.getRationalFormulaManager(), coefficients),
            manager
                .getRationalFormulaManager()
                .add(
                    right.encode(manager.getRationalFormulaManager(), coefficients),
                    offset.encode(manager.getRationalFormulaManager(), coefficients)));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var aux = objectNode(this, "+ " + offset, "_aux").with("style", "dashed");

    return graph
        .with(nodes.get(left).link(highlight(to(aux).with(Color.BLUE4).with("dir", "none"))))
        .with(aux.link(highlight(to(nodes.get(right)).with(Color.RED).with("dir", "none"))));
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Sets.union(super.occurringCoefficients(), Collections.singleton(offset.canonical()));
  }
}
