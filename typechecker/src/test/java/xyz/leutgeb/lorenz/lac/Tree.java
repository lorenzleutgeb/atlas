package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.singleton;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static xyz.leutgeb.lorenz.lac.Tests.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint.eq;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.NegationConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class Tree {
  private static final String EXT = "external";

  @ParameterizedTest
  @ValueSource(strings = {"Tree.id", "Tree.id_let"})
  void id(final String fqn) throws UnificationError, TypeError, IOException {
    final var program = loadAndNormalizeAndInferAndUnshare(fqn);
    final var definition = program.getFunctionDefinitions().get(fqn);
    assertNotNull(definition);
    Map<String, CombinedFunctionAnnotation> signature = new HashMap<>();
    definition.stubAnnotations(signature, SmartRangeHeuristic.DEFAULT, true);
    final var functionSignature = signature.get(fqn);

    // NOTE: We show unsatisfiability of the constraint system together
    // with inequality of the "input" and "output" potential.
    // This only makes sense without applications of (w), but there
    // are none scheduled as of 2020-07-27.
    // To make sure that this also holds for an application of (let:tree:cf)
    // in Tree.id_let, one must "hack" the instance of Prover used in
    // Program#solve by adding prover.setTreeCf(true).

    assertFalse(
        program
            .solve(
                signature,
                singleton(
                    new NegationConstraint(
                        new ConjunctiveConstraint(
                            eq(
                                functionSignature.withCost().from(),
                                functionSignature.withCost().to(),
                                EXT),
                            EXT),
                        EXT)))
            .isPresent());
  }
}
