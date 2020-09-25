package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static java.util.stream.Collectors.toSet;

import com.google.common.collect.BiMap;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;

@Value
@Slf4j
public class ExclusiveDisjunctiveConstraint extends Constraint {
  Constraint left;
  Constraint right;

  public ExclusiveDisjunctiveConstraint(Constraint left, Constraint right, String reason) {
    super(reason);
    this.left = left;
    this.right = right;
  }

  @Override
  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, ArithExpr> coefficients) {
    return ctx.mkXor(left.encode(ctx, coefficients), right.encode(ctx, coefficients));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    log.warn("cannot convert disjunctive constraint to graph");
    return graph;
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new ExclusiveDisjunctiveConstraint(
        left.replace(target, replacement), right.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Stream.of(left, right)
        .map(Constraint::occurringCoefficients)
        .flatMap(Set::stream)
        .collect(toSet());
  }

  @Override
  public String toString() {
    return left + " xor " + right;
  }

  @Override
  public Stream<Constraint> children() {
    return Stream.of(left, right);
  }
}
