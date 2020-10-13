package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Match implements Rule {
  public static final Match INSTANCE = new Match();

  public static AnnotatingContext pop(List<Identifier> that, Identifier scrutinee) {
    final var newIds = new ArrayList<Identifier>(that.size() - 1);
    for (final var id : that) {
      if (scrutinee.equals(id)) {
        continue;
      }
      newIds.add(id);
    }
    return new AnnotatingContext(newIds, "P(" + scrutinee + ")");
  }

  public static AnnotatingContext extend(
      List<Identifier> that, Identifier scrutinee, Identifier left, Identifier right) {
    final var newIds = new ArrayList<Identifier>(2 + that.size());
    newIds.addAll(that);
    newIds.add(left);
    newIds.add(right);
    return new AnnotatingContext(newIds, "R(" + scrutinee + ")(" + left + "," + right + ")");
  }

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (MatchExpression) obligation.getExpression();

    final var x = (Identifier) expression.getScrut();
    final var pattern = expression.getNodePattern();
    final var x1 = (Identifier) pattern.getLeft();
    final var x3 = (Identifier) pattern.getRight();

    final var gammaxq = obligation.getContext();
    final var gammap = pop(gammaxq.getIds(), x);
    final var gammaxsr = extend(gammap.getIds(), x, x1, x3);

    final var rConstraints = new ArrayList<Constraint>();
    final var pConstraints = new ArrayList<Constraint>();

    rConstraints.addAll(
        List.of(
            new EqualityConstraint(
                gammaxsr.getRankCoefficientOrDefine(x1),
                gammaxq.getRankCoefficient(x),
                "(match) r_{m+1} = q_{m+1} (for var `" + x1 + "`)"),
            new EqualityConstraint(
                gammaxsr.getRankCoefficientOrDefine(x3),
                gammaxq.getRankCoefficient(x),
                "(match) r_{m+2} = q_{m+1} (for var `" + x3 + "`)"),
            new EqualityConstraint(
                gammaxsr.getCoefficientOrDefine(id -> id.equals(x1) ? 1 : 0, 0),
                gammaxq.getRankCoefficient(x),
                "r_{(0⃗⃗,1,0,0)} = q_{m+1}"),
            new EqualityConstraint(
                gammaxsr.getCoefficientOrDefine(id -> id.equals(x3) ? 1 : 0, 0),
                gammaxq.getRankCoefficient(x),
                "r_{(0⃗,0,1,0)} = q_{m+1}")));

    gammaxq
        .streamNonRank()
        .map(
            qEntry -> {
              final var a = qEntry.getAssociatedIndex(x);
              return new EqualityConstraint(
                  gammaxsr.getCoefficientOrDefine(qEntry.mask(x1, a).mask(x3, a)),
                  qEntry.getValue(),
                  "(match) r_{a⃗⃗,a,a,b} = q_{a⃗⃗,a,b} for expression `" + expression + "`");
            })
        .forEach(rConstraints::add);

    final var sums = new HashMap<List<Integer>, List<Coefficient>>();
    final var listWithRank = new ArrayList<Coefficient>();
    listWithRank.add(gammaxq.getRankCoefficient(x));
    sums.put(Annotation.unitIndex(gammap.size()), listWithRank);

    gammaxq
        .streamNonRank()
        .forEach(
            entry -> {
              int c = entry.getAssociatedIndex(x) + entry.getOffsetIndex();

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
              "(match) p_{a⃗⃗, c} = Σ_{a+b=c} q_{a⃗⃗, a, b}"));
    }

    for (var id : gammaxq.getIds()) {
      if (id.equals(x)) {
        continue;
      }
      pConstraints.add(
          new EqualityConstraint(
              gammaxq.getRankCoefficient(id),
              gammap.getRankCoefficientOrDefine(id),
              "(match) q_i = p_i"));
      rConstraints.add(
          new EqualityConstraint(
              gammaxq.getRankCoefficient(id),
              gammaxsr.getRankCoefficientOrDefine(id),
              "(match) q_i = r_i"));
    }

    return new Rule.ApplicationResult(
        List.of(
            obligation.keepAnnotationAndCost(gammap, expression.getLeaf()),
            obligation.keepAnnotationAndCost(gammaxsr, expression.getNode())),
        List.of(pConstraints, rConstraints));
  }

  @Override
  public String getName() {
    return "match";
  }
}
