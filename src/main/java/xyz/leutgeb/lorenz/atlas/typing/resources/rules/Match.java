package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.MatchTreeExpression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.NodeExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public class Match implements Rule {
  public static final Match INSTANCE = new Match();

  private String ruleName(Obligation obligation) {
    return "("
        + getName()
        + " "
        + ((MatchTreeExpression) obligation.getExpression()).getScrut().terminalOrBox()
        + ")";
  }

  public static AnnotatingContext pop(
      List<IdentifierExpression> that, IdentifierExpression scrutinee) {
    final var newIds = new ArrayList<IdentifierExpression>(Math.max(that.size() - 1, 0));
    for (final var id : that) {
      if (scrutinee.equals(id)) {
        continue;
      }
      newIds.add(id);
    }
    return new AnnotatingContext(newIds, "P(" + scrutinee + ")");
  }

  public static AnnotatingContext extend(
      List<IdentifierExpression> that,
      IdentifierExpression scrutinee,
      IdentifierExpression left,
      IdentifierExpression right) {
    final var newIds = new ArrayList<IdentifierExpression>(2 + that.size());
    newIds.addAll(that);
    newIds.add(left);
    newIds.add(right);
    return new AnnotatingContext(newIds, "R(" + scrutinee + ")(" + left + "," + right + ")");
  }

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (MatchTreeExpression) obligation.getExpression();

    final var x = (IdentifierExpression) expression.getScrut();
    final var pattern = expression.getNodePattern();

    final var gammaxq = obligation.getContext();
    final var gammap = pop(gammaxq.getIds(), x);

    final var pConstraints = new ArrayList<Constraint>();

    final var sums = new HashMap<List<Integer>, List<Coefficient>>();
    final var listWithRank = new ArrayList<Coefficient>();
    listWithRank.add(gammaxq.getRankCoefficient(x));
    sums.put(Annotation.unitIndex(gammap.size()), listWithRank);

    gammaxq
        .streamNonRank()
        .forEach(
            entry -> {
              int a = entry.getAssociatedIndex(x);
              int b = entry.getOffsetIndex();

              if (b < 0 || a < 0) {
                return;
              }

              int c = a + b;

              if (gammap.isEmpty() && c == 1) {
                return;
              }

              final var index = new ArrayList<Integer>(gammap.size());
              for (var id : gammap.getIds()) {
                index.add(entry.getAssociatedIndex(id));
              }
              index.add(c);

              final var sum = sums.computeIfAbsent(index, key -> new ArrayList<>());
              sum.add(entry.getValue());
            });

    for (var entry : sums.entrySet()) {
      if (entry.getValue().isEmpty()) {
        continue;
      }
      pConstraints.add(
          new EqualsSumConstraint(
              gammap.getAnnotation().getCoefficientOrDefine(entry.getKey()),
              entry.getValue(),
              ruleName(obligation) + " p_{a⃗⃗, c} = Σ_{a+b=c} q_{a⃗⃗, a, b}"));
    }

    for (var id : gammaxq.getIds()) {
      if (id.equals(x)) {
        continue;
      }
      pConstraints.add(
          new EqualityConstraint(
              gammaxq.getRankCoefficient(id),
              gammap.getRankCoefficientOrDefine(id),
              ruleName(obligation) + " q_i = p_i"));
    }

    if (x.equals(pattern)) {
      return new Rule.ApplicationResult(
          List.of(
              obligation.keepAnnotationAndCost(gammap, expression.getLeaf()),
              obligation.keepContextAndAnnotationAndCost(expression.getNode())),
          List.of(pConstraints, emptyList()));
    }

    final var x1 = (IdentifierExpression) ((NodeExpression) pattern).getLeft();
    final var x3 = (IdentifierExpression) ((NodeExpression) pattern).getRight();

    final var gammaxsr = extend(gammap.getIds(), x, x1, x3);

    final var rConstraints = new ArrayList<Constraint>();

    rConstraints.addAll(
        List.of(
            new EqualityConstraint(
                gammaxsr.getRankCoefficientOrDefine(x1),
                gammaxq.getRankCoefficient(x),
                ruleName(obligation) + " r_{m+1} = q_{m+1} (for var " + x1 + ")"),
            new EqualityConstraint(
                gammaxsr.getRankCoefficientOrDefine(x3),
                gammaxq.getRankCoefficient(x),
                ruleName(obligation) + " r_{m+2} = q_{m+1} (for var " + x3 + ")"),
            new EqualityConstraint(
                gammaxsr.getCoefficientOrDefine(id -> id.equals(x1) ? 1 : 0, 0),
                gammaxq.getRankCoefficient(x),
                ruleName(obligation) + " r_{(0,...,0,1,0,0)} = q_{m+1}"),
            new EqualityConstraint(
                gammaxsr.getCoefficientOrDefine(id -> id.equals(x3) ? 1 : 0, 0),
                gammaxq.getRankCoefficient(x),
                ruleName(obligation) + " r_{(0,...,0,0,1,0)} = q_{m+1}")));

    gammaxq
        .streamNonRank()
        .map(
            qEntry -> {
              final var a = qEntry.getAssociatedIndex(x);
              return new EqualityConstraint(
                  gammaxsr.getCoefficientOrDefine(qEntry.mask(x1, a).mask(x3, a)),
                  qEntry.getValue(),
                  ruleName(obligation)
                      + " r_{a⃗⃗,a,a,b} = q_{a⃗⃗,a,b} for expression "
                      + expression);
            })
        .forEach(rConstraints::add);

    for (var id : gammaxq.getIds()) {
      if (id.equals(x)) {
        continue;
      }
      rConstraints.add(
          new EqualityConstraint(
              gammaxq.getRankCoefficient(id),
              gammaxsr.getRankCoefficientOrDefine(id),
              ruleName(obligation) + " q_i = r_i"));
    }

    return new Rule.ApplicationResult(
        List.of(
            obligation.keepAnnotationAndCost(gammap, expression.getLeaf()),
            obligation.keepAnnotationAndCost(gammaxsr, expression.getNode())),
        List.of(pConstraints, rConstraints));
  }
}
