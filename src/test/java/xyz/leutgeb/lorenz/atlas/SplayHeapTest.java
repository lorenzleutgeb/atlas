package xyz.leutgeb.lorenz.atlas;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.heuristics.SmartRangeHeuristic;

public class SplayHeapTest {
  @Test
  @Disabled("Continue after CAV 2021.")
  public void improvePartition() {
    final var fixed =
        CombinedFunctionAnnotation.of(
            new Annotation(
                List.of(Coefficient.known(1, 2)),
                Map.of(
                    unitIndex(1), ONE, List.of(1, 0), Coefficient.known(1, 2), List.of(1, 1), ONE),
                "Q"),
            new Annotation(List.of(Coefficient.known(1, 2)), Map.of(unitIndex(1), ONE), "Q'"),
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

    final Map<String, CombinedFunctionAnnotation> annotations =
        TestUtil.extractAnnotations(immutableAnnotations);
    final Map<String, java.nio.file.Path> tactics = TestUtil.extractTactics(immutableAnnotations);

    final var result =
        program.solve(annotations, tactics, true, true, false, Collections.emptySet());
    assertTrue(result.getSolution().isPresent());
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
