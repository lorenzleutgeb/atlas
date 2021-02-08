package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.Tactics.SPLAY_EXPECTED;
import static xyz.leutgeb.lorenz.lac.Tactics.SPLAY_INSERT_EXPECTED;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class SplayTreeTest {
  @Test
  public void sequential() throws UnificationError, TypeError, IOException {
    final var fixed =
        CombinedFunctionAnnotation.of(
            new Annotation(
                List.of(Coefficient.of(1, 2)),
                Map.of(unitIndex(1), ONE, List.of(1, 0), Coefficient.of(1, 2), List.of(1, 1), ONE),
                "Q"),
            new Annotation(List.of(Coefficient.of(1, 2)), Map.of(unitIndex(1), ONE), "Q'"),
            SmartRangeHeuristic.DEFAULT.generate("splaycf", 1),
            SmartRangeHeuristic.DEFAULT.generate("splaycf'", 1));

    final var base = SmartRangeHeuristic.DEFAULT.generate("base", 1);

    final var unk =
        CombinedFunctionAnnotation.of(
            SmartRangeHeuristic.DEFAULT.generate("splayleft", 1),
            base,
            SmartRangeHeuristic.DEFAULT.generate("splaycf", 1),
            SmartRangeHeuristic.DEFAULT.generate("splaycf'", 1));

    final var immutableAnnotations =
        Map.of(
            /*
            "SplayHeap.partition", Config.of("SplayHeap/partition-improve",
                        fixed
                        ),
                 */
            "SplayTree.insert",
            Config.of("SplayTree/insert", SPLAY_INSERT_EXPECTED),
            "SplayTree.splay",
            Config.of("SplayTree/splay", SPLAY_EXPECTED)
            // ,
            /*
            "SplayHeap.insert",
            Config.of(
                    CombinedFunctionAnnotation.of(
                            SmartRangeHeuristic.DEFAULT.generate("insertleft", 1), base)),
            "SplayHeap.del_min",
            Config.of(
                    CombinedFunctionAnnotation.of(
                            SmartRangeHeuristic.DEFAULT.generate("splay_maxleft", 1),
                            base,
                            SmartRangeHeuristic.DEFAULT.generate("splay_maxcf", 1),
                            SmartRangeHeuristic.DEFAULT.generate("splay_maxcf'", 1)))
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

    if (true) {
      final var optionalProver = program.sequentiallyProveWithTactics(annotations, tactics, true);
    } else {
      final var optionalProver = program.proveWithTactics(annotations, tactics, true);
      assertTrue(optionalProver.isPresent());
      final var solution = optionalProver.get().solve();
      assertTrue(solution.hasSolution());
      program.mockIngest(solution.getSolution());
    }

    // final var prover = optionalProver.get();

    // var multiTarget = Optimization.standard(program);

    /*
    should not be required
    multiTarget.constraints.addAll(
        forceRank(
            program
                .getFunctionDefinitions()
                .get("SplayHeap.partition")
                .getInferredSignature()
                .getAnnotation()
                .get()
                .withCost));
     */

    /*
    final var result =
        prover.solve(
            multiTarget.constraints,
            List.of(multiTarget.target),
            "minq",
            ConstraintSystemSolver.Domain.RATIONAL);
    program.mockIngest(result.getSolution());
    assertTrue(result.getSolution().isPresent());
     */
  }
}
