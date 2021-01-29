package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.Tactics.SPLAY_TIGHTER;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class AutomationTest {
  public static Stream<Arguments> source() {
    return Stream.of(
        Arguments.of("PairingHeap.merge_pairs_isolated", SPLAY_TIGHTER),
        Arguments.of(
            "SplayHeap.partition",
            CombinedFunctionAnnotation.of(
                new Annotation(
                    List.of(Coefficient.of(1, 2)),
                    Map.of(
                        unitIndex(1), ONE, List.of(1, 0), Coefficient.of(3, 4), List.of(1, 1), ONE),
                    "Q"),
                new Annotation(List.of(Coefficient.of(1, 2)), Map.of(unitIndex(1), ONE), "Q'"),
                new Annotation(List.of(ZERO), Map.of(List.of(1, 1), Coefficient.of(1, 2)), "Qcf"),
                new Annotation(
                    List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Qcf'"))),
        Arguments.of("SplayTree.splay", SPLAY_TIGHTER),
        Arguments.of("SplayTree.splay_max", SPLAY_TIGHTER));
  }

  @ParameterizedTest
  @MethodSource("source")
  public void test(String fqn, CombinedFunctionAnnotation combinedFunctionAnnotation)
      throws UnificationError, TypeError, IOException {
    final var program = loadAndNormalizeAndInferAndUnshare(fqn);
    final var annotations = Map.of(fqn, combinedFunctionAnnotation);
    final var optionalProver = program.proveSmart(annotations, true);
    assertTrue(optionalProver.isPresent());
    var solverResult = optionalProver.get().solve();
    assertTrue(solverResult.getSolution().isPresent());
    program.mockIngest(solverResult.getSolution());
  }
}
