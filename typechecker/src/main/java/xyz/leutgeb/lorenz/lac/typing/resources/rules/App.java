package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.singletonList;
import static xyz.leutgeb.lorenz.lac.util.Util.append;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.ast.CallExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.DisjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

@Slf4j
public class App implements Rule {
  public static final App INSTANCE = new App();

  private static String ruleName(int cost) {
    return "(app" + (cost == 0 ? ":cf" : "") + ")";
  }

  private static List<Constraint> costFree(
      Obligation obligation, Set<FunctionAnnotation> candidates) {
    if (candidates == null || candidates.isEmpty()) {
      throw new IllegalArgumentException("expected some candidate annotations (at least one)");
    }

    return singletonList(
        new DisjunctiveConstraint(
            candidates.stream()
                .map(
                    candidate ->
                        new ConjunctiveConstraint(
                            append(
                                EqualityConstraint.eq(
                                    candidate.from(),
                                    obligation.getContext().getAnnotation(),
                                    "(app:cf) Q"),
                                EqualityConstraint.eq(
                                    candidate.to(),
                                    obligation.getContext().getAnnotation(),
                                    "(app:cf) Q'")),
                            "(app:cf)"))
                .collect(Collectors.toUnmodifiableList()),
            "(app:cf)"));
  }

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (CallExpression) obligation.getExpression();

    // Look up global annotation for this function.
    final var annotation = globals.getSignature(expression.getName().getName());

    return Rule.ApplicationResult.onlyConstraints(
        obligation.getCost() == 1
            ? append(
                obligation
                    .getContext()
                    .getAnnotation()
                    .increment(
                        annotation.from(),
                        obligation.getCost(),
                        ruleName(obligation.getCost()) + " Q from signature + 1 = Q from context"),
                EqualityConstraint.eq(
                    annotation.to(),
                    obligation.getAnnotation(),
                    ruleName(obligation.getCost()) + " Q'"))
            : costFree(obligation, globals.getCostFreeSignatures(expression.getName().getName())));
  }

  @Override
  public String getName() {
    return "app";
  }
}
