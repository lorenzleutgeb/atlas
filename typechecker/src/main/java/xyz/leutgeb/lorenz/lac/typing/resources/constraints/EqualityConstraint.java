package xyz.leutgeb.lorenz.lac.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;

@Data
@Slf4j
@EqualsAndHashCode(callSuper = true)
public class EqualityConstraint extends Constraint {
  @NonNull protected final Coefficient left, right;

  public EqualityConstraint(Coefficient left, Coefficient right, String reason) {
    super(reason);
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    if (left.equals(right)) {
      throw new IllegalArgumentException(
          "cannot create equality constraint for equal coefficients");
    }
    this.left = left;
    this.right = right;
  }

  public static List<Constraint> eqRanks(
      Iterable<String> ids, AnnotatingContext left, AnnotatingContext right, String reason) {
    return StreamSupport.stream(ids.spliterator(), false)
        .map(
            id ->
                new EqualityConstraint(
                    left.getRankCoefficient(id), right.getRankCoefficient(id), reason))
        .collect(Collectors.toList());
  }

  public static List<Constraint> eq(String reason, Annotation left, Annotation right) {
    if (left.size() != right.size()) {
      throw new IllegalArgumentException("annotations of different sizes cannot be equal");
    }
    final var result = new ArrayList<Constraint>();

    final int size = left.size();
    for (int x = 0; x < size; x++) {
      if (left.getRankCoefficient(x).equals(right.getRankCoefficient(x))) {
        continue;
      }
      result.add(
          new EqualityConstraint(left.getRankCoefficient(x), right.getRankCoefficient(x), reason));
    }

    final Set<List<Integer>> leftIndices = new HashSet<>();
    for (Map.Entry<List<Integer>, Coefficient> entry : left.getCoefficients()) {
      leftIndices.add(entry.getKey());
      var other = right.getCoefficientOrZero(entry.getKey());
      if (entry.getValue().equals(other)) {
        continue;
      }
      result.add(new EqualityConstraint(entry.getValue(), other, reason));
    }

    for (Map.Entry<List<Integer>, Coefficient> entry : right.getCoefficients()) {
      if (!leftIndices.contains(entry.getKey())) {
        result.add(new EqualityConstraint(entry.getValue(), ZERO, reason));
      }
    }

    return result;
  }

  public BoolExpr encode(Context ctx, BiMap<Coefficient, RealExpr> coefficients) {
    return ctx.mkEq(left.encode(ctx, coefficients), right.encode(ctx, coefficients));
  }

  @Override
  public Graph toGraph(Graph graph, Map<Coefficient, Node> nodes) {
    var leftNode = toNode(left, nodes);
    var rightNode = toNode(right, nodes);
    Objects.requireNonNull(leftNode);
    Objects.requireNonNull(rightNode);
    return graph.with(
        leftNode.link(
            highlight(
                to(rightNode).with(Color.BLUE4).with("dir", "none").with("penwidth", "1.5"))));
  }

  @Override
  public Constraint replace(Coefficient target, Coefficient replacement) {
    return new EqualityConstraint(
        left.replace(target, replacement), right.replace(target, replacement), getReason());
  }

  @Override
  public Set<Coefficient> occurringCoefficients() {
    return Set.of(left.canonical(), right.canonical());
  }

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
