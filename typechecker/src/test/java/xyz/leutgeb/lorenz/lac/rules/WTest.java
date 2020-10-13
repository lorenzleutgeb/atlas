package xyz.leutgeb.lorenz.lac.rules;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;

public class WTest {
  @Test
  public void constant() {
    Obligation o =
        new Obligation(
            Collections.singletonList(Identifier.get("_", null)),
            Annotation.constant(1, "two", TWO),
            Identifier.get("a", null),
            Annotation.zero(1));
    final var result = W.INSTANCE.apply(o, AnnotatingGlobals.empty());
    assertEquals(1, result.getObligations().size());

    Set<Constraint> constraints = new HashSet<>();
    result.collectInto(constraints);

    constraints.addAll(
        EqualityConstraint.eq(
            result.getObligations().get(0).getContext().getAnnotation(),
            Annotation.constant(1, "expected", ONE),
            "test"));

    final var solution = ConstraintSystemSolver.solve(constraints, "test");
    assertTrue(solution.isPresent());
  }
}
