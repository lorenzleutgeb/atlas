package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static xyz.leutgeb.lorenz.lac.TestUtil.printTable;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.THREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.zeroCoefficients;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class Tactics {
  protected static final Annotation Q =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), ONE), "Q");

  protected static final Annotation Qnew =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), TWO), "Q");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation Pp =
      new Annotation(zeroCoefficients(1), Map.of(List.of(1, 0), ONE), "P'");

  protected static final Annotation Qp = new Annotation(List.of(ONE), emptyMap(), "Q'");

  protected static final Annotation Qpnew = new Annotation(List.of(ONE), emptyMap(), "Q'");

  private static final String SPLAY_FQN = "SplayTree.splay_eq";
  private static final String INSERT_FQN = "SplayTree.insert_eq";
  private static final String MAX_FQN = "SplayTree.splay_max_eq";
  private static final String CONTAINS_FQN = "SplayTree.contains_eq";
  private static final String LINK_FQN = "PairingHeap.link";
  private static final String MERGE_FQN = "PairingHeap.merge";

  private static <K, V> Map<K, V> mutable(Map<K, V> immutable) {
    return new HashMap<>(immutable);
  }

  private static final CombinedFunctionAnnotation SPLAY_EXPECTED_OLD =
      CombinedFunctionAnnotation.of(
          Q,
          Qp,
          SmartRangeHeuristic.DEFAULT.generate("cf", 1), // P
          SmartRangeHeuristic.DEFAULT.generate("cf'", 1), // Pp
          Annotation.zero(1),
          Annotation.zero(1));

  private static final CombinedFunctionAnnotation SPLAY_EXPECTED_TACAS =
      CombinedFunctionAnnotation.of(
          new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE), "Q"),
          Qp,
          SmartRangeHeuristic.DEFAULT.generate("cf", 1), // P
          SmartRangeHeuristic.DEFAULT.generate("cf'", 1), // Pp
          Annotation.zero(1),
          Annotation.zero(1));

  private static final CombinedFunctionAnnotation SPLAY_EXPECTED_NEW =
      CombinedFunctionAnnotation.of(
          Qnew,
          Qpnew,
          Annotation.constant(1, "cf", ONE),
          Annotation.constant(1, "cf'", ONE),
          Annotation.zero(1),
          Annotation.zero(1));

  private record Config(Optional<String> tactic, Optional<CombinedFunctionAnnotation> annotation) {
    public static Config of(String tactic) {
      return new Config(Optional.ofNullable(tactic), Optional.empty());
    }

    public static Config of(String tactic, CombinedFunctionAnnotation annotation) {
      return new Config(Optional.ofNullable(tactic), Optional.ofNullable(annotation));
    }

    public static Config of(CombinedFunctionAnnotation annotation) {
      return new Config(Optional.empty(), Optional.ofNullable(annotation));
    }

    @Override
    public String toString() {
      var result = "";
      if (tactic.isPresent()) {
        result += tactic.get();
      }
      if (tactic.isPresent() && annotation.isPresent()) {
        result += " ";
      }
      if (annotation.isPresent()) {
        result += annotation.get();
      }
      return result;
    }
  }

  private static Stream<Arguments> allTactics() {
    return Stream.of(
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-light", SPLAY_EXPECTED_TACAS))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-light", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-light"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-fixing", SPLAY_EXPECTED_TACAS))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-fixing", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-fixing"))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq"))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zig", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zig", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zig"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzag", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzag", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzag"))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zag", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zag", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zag"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zagzig", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zagzig", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zagzig"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zagzag", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zagzag", SPLAY_EXPECTED_OLD))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zagzag"))),
        Arguments.of(Map.of("SplayTree.splay_max_eq", Config.of("SplayTree/splay_max_eq"))),
        Arguments.of(Map.of("SplayTree.splay_max_eq", Config.of("auto"))),
        Arguments.of(
            Map.of(
                "SplayTree.contains_eq",
                Config.of("SplayTree/contains_eq"),
                SPLAY_FQN,
                Config.of(SPLAY_EXPECTED_TACAS))),
        Arguments.of(
            Map.of(
                "SplayTree.contains_eq",
                Config.of("SplayTree/contains_eq"),
                SPLAY_FQN,
                Config.of("SplayTree/splay_eq"))),
        Arguments.of(
            Map.of(
                "SplayTree.insert_eq",
                Config.of("SplayTree/insert_eq"),
                SPLAY_FQN,
                Config.of("SplayTree/splay_eq"))),
        Arguments.of(
            Map.of(
                "SplayTree.insert_eq",
                Config.of("auto"),
                SPLAY_FQN,
                Config.of("SplayTree/splay_eq"))),
        Arguments.of(Map.of("PairingHeap.link", Config.of("PairingHeap/link"))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge",
                Config.of("PairingHeap/merge"),
                "PairingHeap.link",
                Config.of("PairingHeap/link"))));
  }

  @ParameterizedTest
  @MethodSource("allTactics")
  public void all(Map<String, Config> immutableAnnotations) throws IOException {
    final var loader = Tests.loader();

    Program program = null;
    try {
      program = loader.load(immutableAnnotations.keySet());
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    program.normalize();
    try {
      program.infer();
    } catch (UnificationError | TypeError e) {
      throw new RuntimeException(e);
    }
    program.unshare(true);
    program.analyzeSizes();

    //noinspection OptionalGetWithoutIsPresent
    final var annotations =
        immutableAnnotations.entrySet().stream()
            .filter(entry -> entry.getValue().annotation().isPresent())
            .collect(
                Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().annotation().get()));

    //noinspection OptionalGetWithoutIsPresent
    final var tactics =
        immutableAnnotations.entrySet().stream()
            .filter(entry -> entry.getValue().tactic().isPresent())
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
                            entry.getValue().tactic().get() + ".txt")));

    final var optionalProver = program.proveWithTactics(annotations, tactics);
    assertTrue(optionalProver.isPresent());

    final var prover = optionalProver.get();

    final List<UnknownCoefficient> sumCoefficients = new ArrayList<>();
    final List<UnknownCoefficient> setCountingCoefficients = new ArrayList<>();
    final List<UnknownCoefficient> pairwiseDiffCoefficients = new ArrayList<>();

    final Set<Constraint> sumConstraints = new HashSet<>();
    final Set<Constraint> setCountingConstraints = new HashSet<>();
    final Set<Constraint> pairwiseDiffConstraints = new HashSet<>();

    for (final var fqn : immutableAnnotations.keySet()) {
      if (!program.getFunctionDefinitions().containsKey(fqn)) {
        fail("Could not find function definition for '" + fqn + "'.");
      }

      final var fd = program.getFunctionDefinitions().get(fqn);

      final var sum = Optimization.sum(fd.getAnnotation());
      sumCoefficients.add(sum.coefficient());
      sumConstraints.addAll(sum.constraints());

      final var setCounting = Optimization.setCounting(fd.getAnnotation());
      setCountingCoefficients.add(setCounting.coefficient());
      setCountingConstraints.addAll(setCounting.constraints());

      final var pairwiseDiff = Optimization.pairwiseDiff(fd.getAnnotation());
      pairwiseDiffCoefficients.add(pairwiseDiff.coefficient());
      pairwiseDiffConstraints.addAll(pairwiseDiff.constraints());

      /*
      if (fd.getFullyQualifiedName().equals(SPLAY_FQN) && tactics.getOrDefault(SPLAY_FQN, Path.of("/")).toString().contains("zigzig-light")) {
        pairwiseDiffConstraints.add(new EqualityConstraint(fd.getAnnotation().from().getRankCoefficient(0), fd.getAnnotation().to().getRankCoefficient(0), "fix rk"));
      }
       */
    }

    prover.plot();
    final var solution = prover.solve();
    assertTrue(solution.isPresent());
    System.out.println(printTable(prover, solution));
    program.mockIngest(solution);
    prover.plotWithSolution(solution.get());

    /*
    if (immutableAnnotations.values().stream().anyMatch(config -> config.annotation().isEmpty())) {
      final var minimizationTargets = sumCoefficients;
      final var minSolution = prover.solve(sumConstraints, minimizationTargets, "min1");

      final var minimizationWithSets =
          union(sumConstraints, union(setCountingConstraints, pairwiseDiffConstraints));
      final var minimizationTargetsWithSets =
          append(append(setCountingCoefficients, pairwiseDiffCoefficients), sumCoefficients);
      final var minSetSolution = prover.solve(minimizationWithSets, minimizationTargetsWithSets, "min2");

      final var minSetSolutionRat =
          prover.solve(
              minimizationWithSets,
              minimizationTargetsWithSets,
              "min2r",
              ConstraintSystemSolver.Domain.RATIONAL);

      System.out.println(printTable(prover, minSolution));
      program.mockIngest(minSolution);

      System.out.println(printTable(prover, minSetSolution));
      program.mockIngest(minSetSolution);

      System.out.println(printTable(prover, minSetSolutionRat));
      program.mockIngest(minSetSolutionRat);

      // prover.plotWithSolution(minSetSolutionRat.get());
    } else {
      prover.plotWithSolution(solution.get());
    }
     */
  }
}
