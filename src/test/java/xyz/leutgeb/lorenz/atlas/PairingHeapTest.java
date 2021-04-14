package xyz.leutgeb.lorenz.atlas;

import static java.util.Collections.emptySet;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE_BY_TWO;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.TWO;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;

public class PairingHeapTest {
  @Test
  @Disabled("continue after CAV")
  public void merge() throws UnificationError, TypeError, IOException {

    final Annotation qp =
        new Annotation(List.of(Coefficient.of(1, 2)), Map.of(unitIndex(1), ONE), "Q'");
    // SmartRangeHeuristic.DEFAULT.generate("merges", 1);

    final var mergeSig =
        new FunctionAnnotation(
            // SmartRangeHeuristic.DEFAULT.generate("splayleft", 2),
            new Annotation(
                List.of(ONE_BY_TWO, ONE_BY_TWO),
                Map.of(List.of(1, 1, 0), ONE_BY_TWO, unitIndex(2), TWO),
                "Q"),
            qp);

    final var immutableAnnotations =
        Map.of(
            /*
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
                 */
            "PairingHeap.merge_isolated",
            Config.of(
                /*"PairingHeap/merge-debug",*/ new CombinedFunctionAnnotation(mergeSig, emptySet()))

            /*
            "PairingHeap.insert",
            Config.of(
                CombinedFunctionAnnotation.of(SmartRangeHeuristic.DEFAULT.generate("Q", 1), qp)),
            */
            /*
                     "PairingHeap.merge_pairs",
                     Config.of(
                         "PairingHeap/merge_pairs",
                         CombinedFunctionAnnotation.of(
                             SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                             SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                             // qp,
                             SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                             SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1))) // ,

            */
            /*
            "PairingHeap.del_min_via_merge_pairs",
            Config.of(
                CombinedFunctionAnnotation.of(SmartRangeHeuristic.DEFAULT.generate("Q", 1), qp)
             */

            );

    /*
    System.setProperty(
        "xyz.leutgeb.lorenz.atlas.typing.resources.proving.Prover.naive", String.valueOf(true));
    System.setProperty("xyz.leutgeb.lorenz.atlas.typing.resources.rules.W.all", String.valueOf(true));
     */

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
    final var result = program.solve(annotations, tactics, true, emptySet());
    assertTrue(result.getSolution().isPresent());
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
