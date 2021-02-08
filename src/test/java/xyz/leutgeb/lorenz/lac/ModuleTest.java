package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.TestUtil.TACTICS;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE_BY_TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization.forceRank;

import java.io.IOException;
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
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class ModuleTest {
  static final Annotation Qp = new Annotation(List.of(ONE_BY_TWO), Map.of(unitIndex(1), ONE), "Q'");

  private static Stream<Arguments> modulesWithoutTactics() {
    return Stream.of(
        Arguments.of(
            Set.of("PairingHeap.insert_isolated", "PairingHeap.del_min_via_merge_pairs_isolated"),
            false),
        Arguments.of(Set.of("SplayHeap.insert", "SplayHeap.del_min"), false),
        Arguments.of(Set.of("SplayTree.insert", "SplayTree.delete"), false));
  }

  private static Stream<Arguments> modulesWithTactics() {
    return Stream.of(
        Arguments.of(
            Set.of("PairingHeap.insert_isolated", "PairingHeap.del_min_via_merge_pairs_isolated"),
            true),
        Arguments.of(Set.of("SplayHeap.insert", "SplayHeap.del_min"), true),
        Arguments.of(Set.of("SplayTree.insert", "SplayTree.delete"), true));
  }

  private static Stream<Arguments> table() {
    return Stream.of(Arguments.of(Set.of("SplayTree.splay"), false));
  }

  @ParameterizedTest
  @MethodSource({"modulesWithTactics", "modulesWithoutTactics"})
  public void test(Set<String> fqns, boolean useTactics)
      throws UnificationError, TypeError, IOException {

    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare(fqns);
    final var prover =
        program
            .proveWithTactics(
                new HashMap<>(),
                useTactics ? program.lookupTactics(emptyMap(), TACTICS) : emptyMap(),
                true)
            .get();

    final var constraints = new HashSet<Constraint>();
    // NOTE: Fixing the right side speeds things up considerably
    // constraints.addAll(program.rightSide(Qp));
    constraints.addAll(program.sameRightSide());

    final var target =
        Optimization.layeredCombo(
            program,
            // program.getRoots(),
            program.getFunctionDefinitions().keySet(),
            Optimization::rankDifference,
            Optimization::customWeightedComponentWiseDifference);

    for (var fd : program.getFunctionDefinitions().values()) {
      target.constraints.addAll(
          forceRank(fd.getInferredSignature().getAnnotation().get().withCost));
    }

    constraints.addAll(target.constraints);

    /*
    final var target = Optimization.standard(program);
    constraints.addAll(target.constraints);
     */

    final var result = prover.solve(constraints, List.of(target.target));
    assertTrue(result.hasSolution());
    program.mockIngest(result.getSolution());
  }
}
