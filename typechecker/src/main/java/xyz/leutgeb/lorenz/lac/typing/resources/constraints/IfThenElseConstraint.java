package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import com.google.common.collect.BiMap;
import com.google.common.collect.Sets;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Set;
import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;

@Value
@EqualsAndHashCode(callSuper = true)
@Slf4j
public class IfThenElseConstraint extends Constraint {
  Constraint condition;
  Coefficient thenBranch;
  Coefficient elseBranch;
  Coefficient target;

  public IfThenElseConstraint(
      Constraint condition,
      Coefficient thenBranch,
      Coefficient elseBranch,
      Coefficient target,
      String reason) {
    super(reason);
    this.condition = condition;
    this.thenBranch = thenBranch;
    this.elseBranch = elseBranch;
    this.target = target;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, ArithExpr> coefficients) {
    return ctx.mkEq(
        target.encode(ctx, coefficients),
        ctx.mkITE(
            condition.encode(ctx, coefficients),
            thenBranch.encode(ctx, coefficients),
            elseBranch.encode(ctx, coefficients)));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    log.warn("cannot convert disjunctive constraint to graph");
    return graph;
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new IfThenElseConstraint(
        condition.replace(target, replacement),
        thenBranch.replace(target, replacement),
        elseBranch.replace(target, replacement),
        this.target.replace(target, replacement),
        getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Sets.union(Set.of(thenBranch, elseBranch, target), condition.occurringCoefficients());
  }
}
