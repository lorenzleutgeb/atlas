package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.TestUtil.TACTICS;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE_BY_TWO;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class ModuleTest {
  static final Annotation Qp = new Annotation(List.of(ONE_BY_TWO), Map.of(unitIndex(1), ONE), "Q'");

  private static Stream<Arguments> modulesWithoutTactics() {
    return Stream.of(
        Arguments.of(Set.of("SplayTree.insert", "SplayTree.delete"), false),
        Arguments.of(
            Set.of("PairingHeap.insert_isolated", "PairingHeap.del_min_via_merge_pairs_isolated"),
            false),
        Arguments.of(Set.of("SplayHeap.insert", "SplayHeap.del_min"), false));
  }

  private static Stream<Arguments> modulesWithTactics() {
    return Stream.of(
        Arguments.of(Set.of("SplayTree.insert", "SplayTree.delete"), true),
        Arguments.of(
            Set.of("PairingHeap.insert_isolated", "PairingHeap.del_min_via_merge_pairs_isolated"),
            true),
        Arguments.of(Set.of("SplayHeap.insert", "SplayHeap.del_min"), true));
  }

  @ParameterizedTest
  @MethodSource("modulesWithTactics")
  public void test(Set<String> fqns, boolean useTactics)
      throws UnificationError, TypeError, IOException {
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqns);
    final var tactics = Map.of("SplayTree.splay_max", "auto");
    final var prover =
        program
            .proveWithTactics(
                new HashMap<>(),
                useTactics ? program.lookupTactics(tactics, TACTICS) : Collections.emptyMap(),
                true)
            .get();

    final var constraints = new HashSet<Constraint>();
    final var rightSide = Qp;
    program.forEach(
        fd ->
            constraints.addAll(
                EqualityConstraint.eq(
                    fd.getInferredSignature().getAnnotation().get().withCost.to,
                    rightSide,
                    "(fix) Q'")));

    for (var fd : program.getFunctionDefinitions().values()) {
      constraints.addAll(
          Optimization.forceRank(fd.getInferredSignature().getAnnotation().get().withCost));
    }

    final var target =
        Optimization.layeredCombo(
            program,
            // program.getRoots(),
            program.getFunctionDefinitions().keySet(),
            Optimization::rankDifference,
            Optimization::customWeightedComponentWiseDifference,
            Optimization::constantDifference,
            Optimization::abs);

    constraints.addAll(target.constraints);

    final var result = prover.solve(constraints, List.of(target.target));
    assertTrue(result.hasSolution());
    program.mockIngest(result.getSolution());
  }
}
