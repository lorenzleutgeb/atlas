package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class PairingHeapTest {
  @Test
  @Disabled("continue after CAV")
  public void merge() throws UnificationError, TypeError, IOException {
    final Annotation qp =
        new Annotation(List.of(Coefficient.of(1, 2)), Map.of(unitIndex(1), ONE), "Q'");

    final var mergeSig =
        new FunctionAnnotation(
            SmartRangeHeuristic.DEFAULT.generate("splayleft", 2),
            // SmartRangeHeuristic.DEFAULT.generate("base", 1)
            qp);

    final var immutableAnnotations =
        Map.of(
            "PairingHeap.link",
            Config.of(
                "PairingHeap/link",
                CombinedFunctionAnnotation.of(
                    SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                    SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                    // qp,
                    SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                    SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                    SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                    SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1))),
            /*
            "PairingHeap.merge",
            Config.of(
                "PairingHeap/merge-debug", new CombinedFunctionAnnotation(mergeSig, emptySet())),

            "PairingHeap.insert",
            Config.of(
                CombinedFunctionAnnotation.of(SmartRangeHeuristic.DEFAULT.generate("Q", 1), qp)),
            */
            "PairingHeap.merge_pairs",
            Config.of(
                "PairingHeap/merge_pairs",
                CombinedFunctionAnnotation.of(
                    SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                    SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                    // qp,
                    SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                    SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1))) // ,
            /*
            "PairingHeap.del_min_via_merge_pairs",
            Config.of(
                CombinedFunctionAnnotation.of(SmartRangeHeuristic.DEFAULT.generate("Q", 1), qp)
             */

            );

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

    var multiTarget = Optimization.customWeightedComponentWiseDifference(mergeSig).get();

    for (var fd : program.getFunctionDefinitions().values()) {
      multiTarget.constraints.addAll(
          Optimization.forceRank(fd.getInferredSignature().getAnnotation().get().withCost));
    }

    final var result =
        prover.solve(
            multiTarget.constraints,
            List.of(multiTarget.target),
            "minq",
            ConstraintSystemSolver.Domain.RATIONAL);
    program.mockIngest(result.getSolution());
    assertTrue(result.getSolution().isPresent());
  }
}
