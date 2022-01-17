package xyz.leutgeb.lorenz.atlas.typing.resources.constraints;

import static guru.nidi.graphviz.model.Link.to;
import static java.util.Collections.singleton;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;

import com.google.common.collect.BiMap;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.RealExpr;
import guru.nidi.graphviz.attribute.Color;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;

@Data
@Slf4j
@EqualsAndHashCode(callSuper = true)
public class EqualityConstraint extends Constraint {
  @NonNull protected final Coefficient left, right;

  private final boolean satisfiable;

  public EqualityConstraint(Coefficient left, Coefficient right, String reason) {
    super(reason);
    Objects.requireNonNull(left);
    Objects.requireNonNull(right);
    this.left = left;
    this.right = right;

    if (left instanceof KnownCoefficient l && right instanceof KnownCoefficient r && !l.equals(r)) {
      satisfiable = false;
      log.debug("Generating unsatisfiable constraint '{} = {}'.", left, right);
    } else {
      satisfiable = true;
    }
  }

  public static List<Constraint> eqRanksDefineFromLeft(
      Iterable<IdentifierExpression> ids,
      AnnotatingContext left,
      AnnotatingContext right,
      String reason) {
    return StreamSupport.stream(ids.spliterator(), false)
        .map(
            id ->
                new EqualityConstraint(
                    left.getRankCoefficientOrZero(id),
                    right.getRankCoefficientOrDefine(id),
                    reason))
        .collect(Collectors.toList());
  }

  public static List<Constraint> eqRanks(
      Iterable<IdentifierExpression> ids,
      AnnotatingContext left,
      AnnotatingContext right,
      String reason) {
    return StreamSupport.stream(ids.spliterator(), false)
        .map(
            id ->
                new EqualityConstraint(
                    left.getRankCoefficient(id), right.getRankCoefficient(id), reason))
        .collect(Collectors.toList());
  }

  public static List<Constraint> eqSoft(Annotation left, Annotation right, String reason) {
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
          new EqualityConstraint(
              left.getRankCoefficient(x), right.getRankCoefficient(x), reason + " rk " + x));
    }

    for (Map.Entry<List<Integer>, Coefficient> entry : left.getCoefficients()) {
      var other = right.getCoefficientOrZero(entry.getKey());
      if (entry.getValue().equals(other)) {
        continue;
      }
      result.add(new EqualityConstraint(entry.getValue(), other, reason + " " + entry.getKey()));
    }

    return result;
  }

  public static List<Constraint> eq(Annotation left, Annotation right, String reason) {
    if (left.size() != right.size()) {
      throw new IllegalArgumentException("annotations of different sizes cannot be equal");
    }
    final var result = new ArrayList<Constraint>();

    final int size = left.size();
    for (int x = 0; x < size; x++) {
      if (left.getRankCoefficientOrZero(x).equals(right.getRankCoefficientOrZero(x))) {
        continue;
      }
      result.add(
          new EqualityConstraint(
              left.getRankCoefficientOrZero(x),
              right.getRankCoefficientOrZero(x),
              reason + " rk " + x));
    }

    final Set<List<Integer>> leftIndices = new HashSet<>();
    for (Map.Entry<List<Integer>, Coefficient> entry : left.getCoefficients()) {
      leftIndices.add(entry.getKey());
      var other = right.getCoefficientOrZero(entry.getKey());
      if (entry.getValue().equals(other)) {
        continue;
      }
      result.add(new EqualityConstraint(entry.getValue(), other, reason + " " + entry.getKey()));
    }

    for (Map.Entry<List<Integer>, Coefficient> entry : right.getCoefficients()) {
      if (!leftIndices.contains(entry.getKey())) {
        result.add(new EqualityConstraint(entry.getValue(), ZERO, reason + " " + entry.getKey()));
      }
    }

    return result;
  }

  public BoolExpr encode(Context ctx, BiMap<UnknownCoefficient, RealExpr> coefficients) {
    /*if (!satisfiable) {
      return ctx.mkFalse();
    }*/
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
    final var leftCanonical = left.canonical();
    final var rightCanonical = right.canonical();

    if (leftCanonical.equals(rightCanonical)) {
      return singleton(leftCanonical);
    }

    return Set.of(left.canonical(), right.canonical());
  }

  @Override
  public boolean known() {
    return (left instanceof KnownCoefficient) && (right instanceof KnownCoefficient);
  }

  @Override
  protected boolean satisfiedInternal() {
    return left.equals(right);
  }

  @Override
  public String toString() {
    return left + " = " + right;
  }
}
