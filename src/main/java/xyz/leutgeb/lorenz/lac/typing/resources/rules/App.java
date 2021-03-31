package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.singletonList;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.util.Util.append;
import static xyz.leutgeb.lorenz.lac.util.Util.flag;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.ast.CallExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.DisjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

@Slf4j
public class App implements Rule {
  public static final App INSTANCE = new App();

  private static int readM(Map<String, String> arguments) {
    return Integer.parseInt(arguments.getOrDefault("m", "2"));
  }

  private static String ruleName(Obligation obligation) {
    return "(app"
        + (obligation.getCost() == 0 ? ":cf" : "")
        + " "
        + obligation.getExpression().terminalOrBox()
        + ")";
  }

  private static List<Constraint> costFree(
      Obligation obligation, Set<FunctionAnnotation> candidates, Map<String, String> arguments) {
    if (candidates == null || candidates.isEmpty()) {
      throw new IllegalArgumentException("expected some candidate annotations (at least one)");
    }

    final boolean noscale = flag(App.class, arguments, "noscale");
    final int m = readM(arguments);

    if (noscale || m == 1) {
      return singletonList(
          new DisjunctiveConstraint(
              candidates.stream()
                  .map(
                      candidate ->
                          new ConjunctiveConstraint(
                              append(
                                  EqualityConstraint.eq(
                                      candidate.from,
                                      obligation.getContext().getAnnotation(),
                                      "(app:cf) Q"),
                                  EqualityConstraint.eq(
                                      candidate.to, obligation.getAnnotation(), "(app:cf) Q'")),
                              "(app:cf)"))
                  .collect(Collectors.toUnmodifiableList()),
              "(app:cf)"));
    }

    return singletonList(
        new DisjunctiveConstraint(
            candidates.stream()
                .flatMap(
                    candidate ->
                        IntStream.rangeClosed(0, m)
                            .mapToObj(
                                i -> {
                                  final var from = candidate.from.multiply(Coefficient.of(i));
                                  final var to = candidate.to.multiply(Coefficient.of(i));

                                  return new ConjunctiveConstraint(
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
                .collect(Collectors.toUnmodifiableList()),
            "(app:cf)"));
  }

  private List<Constraint> appMinus(Obligation obligation, FunctionAnnotation signature) {
    return append(
        EqualityConstraint.eq(
            signature.from, obligation.getContext().getAnnotation(), ruleName(obligation) + " Q"),
        signature.to.increment(
            obligation.getAnnotation(),
            1,
            ruleName(obligation) + " Q' from signature = Q' + 1 from context"));
  }

  private List<Constraint> appMinusWithCf(
      Obligation obligation,
      FunctionAnnotation signature,
      Set<FunctionAnnotation> candidates,
      Map<String, String> arguments) {
    final var m = readM(arguments);

    return singletonList(
        new DisjunctiveConstraint(
            Stream.concat(
                    Stream.of(
                        new ConjunctiveConstraint(
                            append(
                                EqualityConstraint.eq(
                                    signature.from,
                                    obligation.getContext().getAnnotation(),
                                    ruleName(obligation) + " Q"),
                                signature.to.increment(
                                    obligation.getAnnotation(),
                                    1,
                                    ruleName(obligation)
                                        + " Q' from signature = Q' + 1 from context")),
                            "(app)")),
                    candidates.stream()
                        .flatMap(
                            candidate ->
                                IntStream.rangeClosed(1, m)
                                    .mapToObj(
                                        i -> {
                                          final var from =
                                              candidate.from.multiply(Coefficient.of(i));
                                          final var to = candidate.to.multiply(Coefficient.of(i));

                                          final var addFrom =
                                              Annotation.add(signature.from, from.getLeft());
                                          final var addTo =
                                              Annotation.add(signature.to, to.getLeft());

                                          return new ConjunctiveConstraint(
                                              append(
                                                  append(
                                                      append(from.getRight(), to.getRight()),
                                                      append(addFrom.getRight(), addTo.getRight())),
                                                  append(
                                                      EqualityConstraint.eq(
                                                          addFrom.getLeft(),
                                                          obligation.getContext().getAnnotation(),
                                                          "(app) Q"),
                                                      addTo
                                                          .getLeft()
                                                          .increment(
                                                              obligation.getAnnotation(),
                                                              1,
                                                              ruleName(obligation)
                                                                  + " Q' from signature = Q' + 1 from context"))),
                                              "(app)");
                                        })))
                .collect(Collectors.toUnmodifiableList()),
            "(app)"));
  }

  private List<Constraint> appPlusWithCfIncludingShift(
      Obligation obligation,
      FunctionAnnotation signature,
      Set<FunctionAnnotation> candidates,
      Map<String, String> arguments) {
    final var m = readM(arguments);

    final Annotation q = obligation.getContext().getAnnotation();
    final Annotation qp = obligation.getAnnotation();

    final var k = UnknownCoefficient.unknown("shiftapp");

    final List<Constraint> constraints = new ArrayList<>();
    final var kPlusCost = UnknownCoefficient.unknown("shiftcostapp");
    constraints.add(new EqualsSumConstraint(kPlusCost, List.of(k, ONE), "(app) shift cost"));

    final var qk = SmartRangeHeuristic.DEFAULT.generate("qk", q);
    final var qpk = SmartRangeHeuristic.DEFAULT.generate("qk", qp);

    constraints.addAll(q.increment(qk, kPlusCost, "(app) shift"));
    constraints.addAll(qp.increment(qpk, k, "(app) shift"));

    constraints.add(
        new DisjunctiveConstraint(
            Stream.concat(
                    Stream.of(
                        new ConjunctiveConstraint(
                            append(
                                EqualityConstraint.eq(
                                    signature.from, qk, ruleName(obligation) + " Q"),
                                EqualityConstraint.eq(signature.to, qpk, "(app)")),
                            "(app)")),
                    candidates.stream()
                        .flatMap(
                            candidate ->
                                IntStream.rangeClosed(1, m)
                                    .mapToObj(
                                        i -> {
                                          final var from =
                                              candidate.from.multiply(Coefficient.of(i));
                                          final var to = candidate.to.multiply(Coefficient.of(i));

                                          final var addFrom =
                                              Annotation.add(signature.from, from.getLeft());
                                          final var addTo =
                                              Annotation.add(signature.to, to.getLeft());

                                          return new ConjunctiveConstraint(
                                              append(
                                                  append(
                                                      append(from.getRight(), to.getRight()),
                                                      append(addFrom.getRight(), addTo.getRight())),
                                                  append(
                                                      EqualityConstraint.eq(
                                                          addFrom.getLeft(), qk, "(app) Q"),
                                                      EqualityConstraint.eq(
                                                          addTo.getLeft(), qpk, "(app)"))),
                                              "(app)");
                                        })))
                .collect(Collectors.toUnmodifiableList()),
            "(app)"));

    return constraints;
  }

  private List<Constraint> appMinusWithCfIncludingShift(
      Obligation obligation,
      FunctionAnnotation signature,
      Set<FunctionAnnotation> candidates,
      Map<String, String> arguments) {
    final var m = readM(arguments);

    final Annotation q = obligation.getContext().getAnnotation();
    final Annotation qp = obligation.getAnnotation();

    final var k = UnknownCoefficient.unknown("shiftapp");

    final var qk = SmartRangeHeuristic.DEFAULT.generate("qk", q);
    final var qpk = SmartRangeHeuristic.DEFAULT.generate("qk", qp);

    final List<Constraint> constraints = new ArrayList<>();
    constraints.addAll(q.increment(qk, k, "(app) shift"));
    constraints.addAll(qp.increment(qpk, k, "(app) shift"));

    constraints.add(
        new DisjunctiveConstraint(
            Stream.concat(
                    Stream.of(
                        new ConjunctiveConstraint(
                            append(
                                EqualityConstraint.eq(
                                    signature.from, qk, ruleName(obligation) + " Q"),
                                signature.to.increment(
                                    qpk,
                                    1,
                                    ruleName(obligation)
                                        + " Q' from signature = Q' + 1 from context")),
                            "(app)")),
                    candidates.stream()
                        .flatMap(
                            candidate ->
                                IntStream.rangeClosed(1, m)
                                    .mapToObj(
                                        i -> {
                                          final var from =
                                              candidate.from.multiply(Coefficient.of(i));
                                          final var to = candidate.to.multiply(Coefficient.of(i));

                                          final var addFrom =
                                              Annotation.add(signature.from, from.getLeft());
                                          final var addTo =
                                              Annotation.add(signature.to, to.getLeft());

                                          return new ConjunctiveConstraint(
                                              append(
                                                  append(
                                                      append(from.getRight(), to.getRight()),
                                                      append(addFrom.getRight(), addTo.getRight())),
                                                  append(
                                                      EqualityConstraint.eq(
                                                          addFrom.getLeft(), qk, "(app) Q"),
                                                      addTo
                                                          .getLeft()
                                                          .increment(
                                                              qpk,
                                                              1,
                                                              ruleName(obligation)
                                                                  + " Q' from signature = Q' + 1 from context"))),
                                              "(app)");
                                        })))
                .collect(Collectors.toUnmodifiableList()),
            "(app)"));

    return constraints;
  }

  private List<Constraint> appPlus(Obligation obligation, FunctionAnnotation signature) {
    return append(
        obligation
            .getContext()
            .getAnnotation()
            .increment(
                signature.from, 1, ruleName(obligation) + " Q from signature + 1 = Q from context"),
        EqualityConstraint.eq(
            signature.to, obligation.getAnnotation(), ruleName(obligation) + " Q'"));
  }

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (CallExpression) obligation.getExpression();

    // Look up global annotation for this function.
    final var annotation = globals.getSignature(expression.getFullyQualifiedName());

    return Rule.ApplicationResult.onlyConstraints(
        obligation.getCost() == 1
            ? appMinusWithCfIncludingShift(
                obligation, annotation.withCost, annotation.withoutCost, arguments)
            // ? appMinusWithCf(obligation, annotation.withCost, annotation.withoutCost, arguments)
            // ? appMinus(obligation, annotation.withCost)
            : costFree(obligation, annotation.withoutCost, arguments));
  }

  /*
  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (CallExpression) obligation.getExpression();

    // Look up global annotation for this function.
    final var annotation = globals.getSignature(expression.getFullyQualifiedName());

    return Rule.ApplicationResult.onlyConstraints(
        obligation.getCost() == 1
            ? append(
                obligation
                    .getContext()
                    .getAnnotation()
                    .increment(
                        annotation.withCost.from,
                        obligation.getCost(),
                        ruleName(obligation.getCost()) + " Q from signature + 1 = Q from context"),
                EqualityConstraint.eq(
                    annotation.withCost.to,
                    obligation.getAnnotation(),
                    ruleName(obligation.getCost()) + " Q'"))
            : costFree(obligation, annotation.withoutCost));
  }
   */

  @Override
  public String getName() {
    return "app";
  }
}
