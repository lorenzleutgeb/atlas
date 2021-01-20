package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class SplayTreeTest {
  @Test
  public void insert() throws UnificationError, TypeError, IOException {
    final var immutableAnnotations =
        Map.of(
            "Scratch.test",
            Config.of(
                "Scratch/test",
                CombinedFunctionAnnotation.of(
                    new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Q"),
                    new Annotation(List.of(ONE), emptyMap(), "Q'"))));

    final var program = loadAndNormalizeAndInferAndUnshare(immutableAnnotations.keySet());

    final var annotations =
        immutableAnnotations.entrySet().stream()
            .filter(entry -> entry.getValue().annotation.isPresent())
            .collect(
                Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().annotation.get()));

    final var tactics =
        immutableAnnotations.entrySet().stream()
            .filter(entry -> entry.getValue().tactic.isPresent())
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    entry ->
                        Paths.get(
                            ".",
                            "src",
                            "test",
                            "resources",
                            "tactics",
                            entry.getValue().tactic.get() + ".txt")));

    final var optionalProver = program.proveWithTactics(annotations, tactics, true);
    assertTrue(optionalProver.isPresent());

    final var prover = optionalProver.get();

    final Set<Constraint> hackingConstraints = new HashSet<>();

    final List<UnknownCoefficient> pairwiseDiffRankCoefficients = new ArrayList<>();
    final List<UnknownCoefficient> pairwiseDiffNonRankCoefficients = new ArrayList<>();

    final Set<Constraint> pairwiseDiffConstraints = new HashSet<>();

    ConstraintSystemSolver.Domain domain = ConstraintSystemSolver.Domain.RATIONAL;

    for (final var fqn : immutableAnnotations.keySet()) {
      if (!program.getFunctionDefinitions().containsKey(fqn)) {
        fail("Could not find function definition for '" + fqn + "'.");
      }

      final var fd = program.getFunctionDefinitions().get(fqn);

      FunctionAnnotation functionAnnotation =
          fd.getInferredSignature().getAnnotation().get().withCost;

      final var pairwiseDiff =
          Optimization.squareWeightedComponentWiseDifference(functionAnnotation);
      if (pairwiseDiff.isPresent()) {
        /*
        pairwiseDiffRankCoefficients.addAll(pairwiseDiff.get().rankCoefficients);
        pairwiseDiffNonRankCoefficients.addAll(pairwiseDiff.get().nonRankCoefficients);
         */
        pairwiseDiffConstraints.addAll(pairwiseDiff.get().constraints);
      }

      if (functionAnnotation.to.size() == 1
          && functionAnnotation.to.getRankCoefficientOrZero() instanceof UnknownCoefficient
          && functionAnnotation.from.size() == 1) {
        hackingConstraints.add(
            new EqualityConstraint(
                functionAnnotation.from.getRankCoefficient(),
                functionAnnotation.to.getRankCoefficient(),
                "(hack) force rank"));
      }
    }

    var solverResult = prover.solve(hackingConstraints, emptyList(), "sat", domain);
    assertTrue(solverResult.getSolution().isPresent());
    program.mockIngest(solverResult.getSolution());
  }
}
