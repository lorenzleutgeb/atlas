package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import org.sosy_lab.java_smt.api.BooleanFormula;
import org.sosy_lab.java_smt.api.FormulaManager;
import org.sosy_lab.java_smt.api.NumeralFormula;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Value
@Slf4j
@EqualsAndHashCode(callSuper = true)
public class InequalityConstraint extends Constraint {
  @NonNull protected Coefficient left, right;

  public InequalityConstraint(Coefficient left, Coefficient right, String reason) {
    super(reason);
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    if (left.equals(right)) {
      throw new IllegalArgumentException(
          "cannot create inequality constraint for equal coefficients");
    }
    this.left = left;
    this.right = right;
  }

  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    return ctx.mkNot(ctx.mkEq(left.encode(ctx, coefficients), right.encode(ctx, coefficients)));
  }

  @Override
  public BooleanFormula encode(
      FormulaManager manager,
      Map<UnknownCoefficient, NumeralFormula.RationalFormula> coefficients) {
    return manager
        .getBooleanFormulaManager()
        .not(
            manager
                .getRationalFormulaManager()
                .equal(
                    left.encode(manager.getRationalFormulaManager(), coefficients),
                    right.encode(manager.getRationalFormulaManager(), coefficients)));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var leftNode = toNode(left, nodes);
    var rightNode = toNode(right, nodes);
    Objects.requireNonNull(leftNode);
    Objects.requireNonNull(rightNode);
    return graph.with(
        leftNode.link(
            highlight(to(rightNode).with(Color.RED1).with("dir", "none").with("penwidth", "1.5"))));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new InequalityConstraint(
        left.replace(target, replacement), right.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Set.of(left.canonical(), right.canonical());
  }

  @Override
  public String toString() {
    return left + " ≠ " + right;
  }

  @Override
  public boolean known() {
    return (left instanceof KnownCoefficient) && (right instanceof KnownCoefficient);
  }

  @Override
  protected boolean satisfiedInternal() {
    return !left.equals(right);
  }
}
