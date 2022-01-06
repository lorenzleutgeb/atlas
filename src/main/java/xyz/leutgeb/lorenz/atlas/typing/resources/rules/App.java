package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.util.Util.append;

import java.util.*;
import java.util.stream.IntStream;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.CallExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.DisjunctiveConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
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

  private static List<Constraint> costFree(
      Obligation obligation, Set<FunctionAnnotation> candidates, Map<String, String> arguments) {
    if (candidates == null || candidates.isEmpty()) {
      throw new IllegalArgumentException("expected some candidate annotations (at least one)");
    }

    return List.of(
        new DisjunctiveConstraint(
            candidates.stream()
                .flatMap(
                    candidate ->
                        IntStream.rangeClosed(0, readM(arguments))
                            .mapToObj(
                                i -> {
                                  final var from = candidate.from.multiply(Coefficient.of(i));
                                  final var to = candidate.to.multiply(Coefficient.of(i));

                                  return (Constraint)
                                      new ConjunctiveConstraint(
                                          append(
                                              append(from.getRight(), to.getRight()),
                                              append(
                                                  EqualityConstraint.eq(
                                                      from.getLeft(),
                                                      obligation.getContext().getAnnotation(),
                                                      "(app:cf) Q"),
                                                  EqualityConstraint.eq(
                                                      to.getLeft(),
                                                      obligation.getAnnotation(),
                                                      "(app:cf) Q'"))),
                                          "(app:cf)");
                                }))
                .toList(),
            "(app:cf)"));
  }

  private List<Constraint> appMinusWithCfIncludingShift(
      Obligation obligation, CombinedFunctionAnnotation annotation, Map<String, String> arguments) {
    return List.of(
        new DisjunctiveConstraint(
            annotation.withoutCost.stream()
                .flatMap(
                    candidate ->
                        IntStream.rangeClosed(0, readM(arguments))
                            .mapToObj(
                                i -> {
                                  final var from = candidate.from.multiply(Coefficient.of(i));
                                  final var to = candidate.to.multiply(Coefficient.of(i));

                                  final var addFrom =
                                      Annotation.add(annotation.withCost.from, from.getLeft());
                                  final var addTo =
                                      Annotation.add(annotation.withCost.to, to.getLeft());

                                  return (Constraint)
                                      new ConjunctiveConstraint(
                                          append(
                                              append(
                                                  append(from.getRight(), to.getRight()),
                                                  append(addFrom.getRight(), addTo.getRight())),
                                              append(
                                                  EqualityConstraint.eq(
                                                      addFrom.getLeft(),
                                                      obligation.getContext().getAnnotation(),
                                                      "(app) Q"),
                                                  EqualityConstraint.eq(
                                                      addTo.getLeft(),
                                                      obligation.getAnnotation(),
                                                      "(app) Q' from signature = Q' from context"))),
                                          "(app)");
                                }))
                .toList(),
            "(app)"));
  }

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (CallExpression) obligation.getExpression();
    final var annotation = globals.getSignature(expression.getFullyQualifiedName());
    return Rule.ApplicationResult.onlyConstraints(
        obligation.isCost()
            ? appMinusWithCfIncludingShift(obligation, annotation, arguments)
            : costFree(obligation, annotation.withoutCost, arguments));
  }

  @Override
  public String getName() {
    return "app";
  }
}
