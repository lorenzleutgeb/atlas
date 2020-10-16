package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.toVectorString;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.NodeExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Node implements Rule {
  public static final Node INSTANCE = new Node();

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (NodeExpression) obligation.getExpression();

    final var leftId = ((Identifier) expression.getLeft());
    final var rightId = ((Identifier) expression.getRight());

    final var expectedSize = leftId.equals(rightId) ? 1 : 2;
    final var qunordered = obligation.getContext();

    if (qunordered.size() != expectedSize) {
      throw bug("context was not weakened");
    }

    // We reorder the context such that it matches the syntax.
    final var q = qunordered.reorder(List.of(leftId, rightId)).getAnnotation();

    Annotation qp = obligation.getAnnotation();
    final var rankCoefficient = qp.getRankCoefficientOrZero(0);

    // Rank coefficient is covered, we will however check on non-rank coefficients.
    final var occurred = new HashSet<Coefficient>();

    final var constraints = new ArrayList<Constraint>();

    q.streamNonRankCoefficients()
        .filter(qEntry -> qEntry.getKey().get(0).equals(qEntry.getKey().get(1)))
        .map(
            qEntry -> {
              var a = qEntry.getKey().get(0);
              var c = qEntry.getKey().get(2);
              Coefficient qpCoeff = qp.getCoefficientOrZero(List.of(a, c));
              occurred.add(qpCoeff);
              return new EqualityConstraint(
                  q.getCoefficientOrZero(a, a, c),
                  qpCoeff,
                  "(node) q_{(a,a,c)} = q'_{(a,c)} for expression `"
                      + expression
                      + "` with (a,a,c)="
                      + toVectorString(qEntry.getKey()));
            })
        .forEach(constraints::add);

    constraints.add(
        new EqualityConstraint(
            q.getRankCoefficientOrZero(0),
            q.getRankCoefficientOrZero(1),
            "(node) q₀ = q₁ for expression (note starting index 0) `" + expression + "`"));

    constraints.add(
        new EqualityConstraint(
            q.getRankCoefficientOrZero(1),
            rankCoefficient,
            "(node) q₂ = q'₀ for expression `" + expression + "`"));

    constraints.add(
        new EqualityConstraint(
            q.getCoefficientOrZero(1, 0, 0),
            rankCoefficient,
            "(node) q₍₁ ₀ ₀₎ = q'₀ for expression `" + expression + "`"));

    constraints.add(
        new EqualityConstraint(
            q.getCoefficientOrZero(0, 1, 0),
            rankCoefficient,
            "(node) q₍₀ ₁ ₀₎ = q'₀ for expression `" + expression + "`"));

    qp.streamNonRankCoefficients()
        .filter(entry -> !occurred.contains(entry.getValue()))
        .map(x -> new EqualityConstraint(x.getValue(), ZERO, "(node) setToZero " + x.getKey()))
        .forEach(constraints::add);

    return Rule.ApplicationResult.onlyConstraints(constraints);
  }

  @Override
  public String getName() {
    return "node";
  }
}
