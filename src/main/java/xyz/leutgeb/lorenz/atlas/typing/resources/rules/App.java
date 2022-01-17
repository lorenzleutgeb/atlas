package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.zero;
import static xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint.eq;
import static xyz.leutgeb.lorenz.atlas.util.Util.append;

import java.util.*;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.expressions.CallExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.DisjunctiveConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

@Slf4j
public class App implements Rule {
  public static final App INSTANCE = new App();

  private static int readM(Map<String, String> arguments) {
    // NOTE(lorenzleuteb): Default of 2 is chosen empirically.
    return Integer.parseInt(arguments.getOrDefault("m", "2"));
  }

  private static String ruleName(Obligation obligation) {
    return "(app"
        + (obligation.isCost() ? "" : ":cf")
        + " "
        + obligation.getExpression().terminalOrBox()
        + ")";
  }

  private static Constraint withoutCost(
      Obligation obligation, Set<FunctionAnnotation> candidates, Map<String, String> arguments) {
    final var from = obligation.getContext().getAnnotation();
    final var to = obligation.getAnnotation();

    final var cfSize = candidates.size();
    final var m = readM(arguments);
    final var result = new ArrayList<Constraint>(1 + cfSize * m);

    // 0 -> 0
    result.add(
        new ConjunctiveConstraint(
            append(
                eq(from, zero(from.size()), ruleName(obligation)),
                eq(to, zero(to.size()), ruleName(obligation))),
            ruleName(obligation)));

    for (final var candidate : candidates) {
      for (int i = 1; i <= m; i++) {
        final var mulFrom = candidate.from.multiply(Coefficient.known(i));
        final var mulTo = candidate.to.multiply(Coefficient.known(i));

        result.add(
            new ConjunctiveConstraint(
                append(
                    append(mulFrom.getRight(), mulTo.getRight()),
                    append(
                        eq(mulFrom.getLeft(), from, "(app:cf) Q"),
                        eq(mulTo.getLeft(), to, "(app:cf) Q'"))),
                "(app:cf)"));
      }
    }

    return new DisjunctiveConstraint(result, ruleName(obligation));
  }

  private static Constraint withCost(
      Obligation obligation, CombinedFunctionAnnotation annotation, Map<String, String> arguments) {
    final var from = obligation.getContext().getAnnotation();
    final var to = obligation.getAnnotation();

    final var candidates = annotation.withoutCost;
    final var cfSize = candidates.size();
    final var m = readM(arguments);
    final var result = new ArrayList<Constraint>(1 + cfSize * m);

    result.add(
        new ConjunctiveConstraint(
            append(
                eq(annotation.withCost.from, from, ruleName(obligation)),
                eq(annotation.withCost.to, to, ruleName(obligation))),
            ruleName(obligation)));

    for (final var candidate : candidates) {
      for (int i = 1; i <= m; i++) {
        final var mulFrom = candidate.from.multiply(Coefficient.known(i));
        final var mulTo = candidate.to.multiply(Coefficient.known(i));

        final var addFrom = Annotation.add(annotation.withCost.from, mulFrom.getLeft());
        final var addTo = Annotation.add(annotation.withCost.to, mulTo.getLeft());

        result.add(
            new ConjunctiveConstraint(
                append(
                    append(
                        append(mulFrom.getRight(), mulTo.getRight()),
                        append(addFrom.getRight(), addTo.getRight())),
                    append(
                        eq(addFrom.getLeft(), from, "(app) " + i + " * Qcf + Qsig = Qctx"),
                        eq(addTo.getLeft(), to, "(app) " + i + " * Qcf' + Qsig' = Qctx'"))),
                ruleName(obligation)));
      }
    }

    return new DisjunctiveConstraint(result, ruleName(obligation));
  }

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (CallExpression) obligation.getExpression();
    final var annotation = globals.getSignature(expression.getFullyQualifiedName());
    return Rule.ApplicationResult.onlyConstraints(
        List.of(
            obligation.isCost()
                ? withCost(obligation, annotation, arguments)
                : withoutCost(obligation, annotation.withoutCost, arguments)));
  }
}
