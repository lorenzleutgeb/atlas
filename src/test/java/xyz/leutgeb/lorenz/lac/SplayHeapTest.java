package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class SplayHeapTest {
  @Test
  @Disabled("continue after CAV")
  public void improvePartition() throws UnificationError, TypeError, IOException {
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
            "SplayHeap.partition", Config.of("SplayHeap/partition-improve", fixed)
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
    final var result = program.solve(annotations, tactics, true, Collections.emptySet());
    assertTrue(result.getSolution().isPresent());
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
