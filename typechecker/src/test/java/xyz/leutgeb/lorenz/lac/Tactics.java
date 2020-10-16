package xyz.leutgeb.lorenz.lac;

import static com.google.common.collect.Sets.union;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static xyz.leutgeb.lorenz.lac.TestUtil.printTable;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.zero;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.THREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.append;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
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
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.Fraction;

public class Tactics {
  protected static final Annotation Q =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE), "Q");

  protected static final Annotation QwithConst =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), ONE), "Q");

  protected static final Annotation Qnew =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), TWO), "Q");

  protected static final Annotation Q5by2 =
      new Annotation(
          List.of(ONE),
          Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(5, 2)), unitIndex(1), ONE),
          "Q");

  protected static final Annotation Q5by2p =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), ONE), "Q'");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation P2 =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "P2");

  protected static final Annotation Prk =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Prk");

  protected static final Annotation Plnf =
      new Annotation(List.of(ZERO, ZERO, ZERO, ZERO), Map.of(List.of(1, 1, 1, 1, 0), ONE), "P");

  protected static final Annotation Qp = new Annotation(List.of(ONE), emptyMap(), "Q'");

  protected static final Annotation QpwithConst =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), ONE), "Q'");

  private static final String SPLAY_FQN = "SplayTree.splay_eq";
  private static final String INSERT_FQN = "SplayTree.insert_eq";
  private static final String MAX_FQN = "SplayTree.splay_max_eq";
  private static final String CONTAINS_FQN = "SplayTree.contains_eq";
  private static final String LINK_FQN = "PairingHeap.link";
  private static final String MERGE_FQN = "PairingHeap.merge";
  public static final String MERGE_PAIRS = "PairingHeap.merge_pairs";

  private static final CombinedFunctionAnnotation SPLAY_EXPECTED_TACAS =
      CombinedFunctionAnnotation.of(
          QwithConst, QpwithConst, P, P, P2, P2, Annotation.zero(1), Annotation.zero(1));

  private static final CombinedFunctionAnnotation SPLAY_EXPECTED_TACAS_VARCF =
      CombinedFunctionAnnotation.of(
          SPLAY_EXPECTED_TACAS.withCost().from(),
          SPLAY_EXPECTED_TACAS.withCost().to(),
          SmartRangeHeuristic.DEFAULT.generate("cf", 1),
          SmartRangeHeuristic.DEFAULT.generate("cf'", 1),
          Annotation.zero(1),
          Annotation.zero(1));

  private static CombinedFunctionAnnotation passRankAndConstant(int q10, int q02) {
    return CombinedFunctionAnnotation.of(
        new Annotation(
            List.of(ONE),
            Map.of(
                List.of(1, 0), new KnownCoefficient(q10), unitIndex(1), new KnownCoefficient(q02)),
            "Q"),
        new Annotation(List.of(ONE), Map.of(unitIndex(1), new KnownCoefficient(q02)), "Q'"),
        // P,
        // P,
        // P2,
        // P2,
        SmartRangeHeuristic.DEFAULT.generate("cf1", 1),
        SmartRangeHeuristic.DEFAULT.generate("cf1'", 1),
        /*
        SmartRangeHeuristic.DEFAULT.generate("cf2", 1),
        SmartRangeHeuristic.DEFAULT.generate("cf2'", 1),
        SmartRangeHeuristic.DEFAULT.generate("cf3", 1),
        SmartRangeHeuristic.DEFAULT.generate("cf3'", 1),
         */
        Annotation.zero(1),
        Annotation.zero(1));
  }

  private static CombinedFunctionAnnotation passRank(int q10) {
    return passRankAndConstant(q10, 0);
  }

  private static CombinedFunctionAnnotation passRankAndOne(int q10) {
    return passRankAndConstant(q10, 1);
  }

  private static final CombinedFunctionAnnotation SPLAY_TIGHT =
      CombinedFunctionAnnotation.of(
          Q5by2,
          Q5by2p,
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(3, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(3, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(1, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(1, 2))),
              "Q2cf"),
          Annotation.zero(1),
          Annotation.zero(1));

  private static final CombinedFunctionAnnotation SPLAY_HEAP_PARTITION_TIGHT =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(5, 2)), unitIndex(1), TWO),
              "Q"),
          new Annotation(List.of(ONE), Map.of(unitIndex(1), TWO), "Q'"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(3, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(3, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(1, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(1, 2))),
              "Q2cf"),
          Annotation.zero(1),
          Annotation.zero(1));

  private static CombinedFunctionAnnotation free(int from, int to) {
    String name = randomHex();
    return CombinedFunctionAnnotation.of(
        SmartRangeHeuristic.DEFAULT.generate(name, from),
        SmartRangeHeuristic.DEFAULT.generate(name + "'", to),
        SmartRangeHeuristic.DEFAULT.generate(name + "cf1", from),
        SmartRangeHeuristic.DEFAULT.generate(name + "cf1'", to),
        SmartRangeHeuristic.DEFAULT.generate(name + "cf2", from),
        SmartRangeHeuristic.DEFAULT.generate(name + "cf2'", to));
  }

  private static CombinedFunctionAnnotation perpetuumMobile(int from, int to) {
    return CombinedFunctionAnnotation.of(
        Annotation.knownConstant(from, "Q", 0),
        Annotation.knownConstant(to, "Q'", 10),
        SmartRangeHeuristic.DEFAULT.generate("cf", from),
        SmartRangeHeuristic.DEFAULT.generate("cf'", to));
  }

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

    public static Object empty() {
      return new Config(Optional.empty(), Optional.empty());
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
            Map.of(
                "PairingHeap.pass1",
                Config.of(
                    "PairingHeap/pass1",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(THREE),
                            Map.of(
                                List.of(1, 0),
                                new KnownCoefficient(2),
                                List.of(0, 2),
                                new KnownCoefficient(1)),
                            "Q"),
                        new Annotation(
                            List.of(ONE),
                            Map.of(
                                List.of(1, 0),
                                new KnownCoefficient(1),
                                List.of(0, 2),
                                new KnownCoefficient(1)),
                            "Q'"),
                        zero(1),
                        zero(1),
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "ddad"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "dda"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "ddadx"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "ddaxxx"))))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_nolink",
                Config.of("PairingHeap/merge_pairs_nolink", passRankAndOne(3)),
                "PairingHeap.pass1",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(TWO),
                            Map.of(List.of(1, 0), new KnownCoefficient(2), List.of(0, 2), ONE),
                            "Q"),
                        QpwithConst,
                        P2,
                        P2,
                        Annotation.zero(1),
                        Annotation.zero(1))),
                "PairingHeap.pass2",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(THREE),
                            Map.of(List.of(1, 0), new KnownCoefficient(4), List.of(0, 2), ONE),
                            "Q"),
                        QpwithConst,
                        P2,
                        P2,
                        Annotation.zero(1),
                        Annotation.zero(1)))

                /*
                "PairingHeap.merge",
                Config.of(
                        CombinedFunctionAnnotation.of(
                                SmartRangeHeuristic.DEFAULT.generate("Q", 2),
                                QpwithConst,
                                SmartRangeHeuristic.DEFAULT.generate("Qcf", 2),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                                Annotation.zero(2),
                                Annotation.zero(1))),


                "PairingHeap.insert",
                Config.of(
                        CombinedFunctionAnnotation.of(
                                SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                QpwithConst,
                                SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                                Annotation.zero(1),
                                Annotation.zero(1))),

                "PairingHeap.link",
                                Config.of(
                                        CombinedFunctionAnnotation.of(
                                                SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                                QpwithConst,
                                                SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                                                SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                                                Annotation.zero(1),
                                                Annotation.zero(1))
                )
                                        */
                )),
        Arguments.of(
            Map.of(
                SPLAY_FQN,
                Config.of("SplayTree/splay_eq-fiddle", SPLAY_TIGHT),
                "SplayTree.splay_max_eq",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        SPLAY_TIGHT.withCost().to(),
                        SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                        Annotation.zero(1),
                        Annotation.zero(1))),
                /*
                "SplayTree.delete_eq", Config.of(
                        CombinedFunctionAnnotation.of(
                                SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                SPLAY_EXPECTED_TACAS.withCost().to(),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                                Annotation.zero(1),
                                Annotation.zero(1)))
                "SplayTree.insert_eq", Config.of(
                        CombinedFunctionAnnotation.of(
                                SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                SPLAY_EXPECTED_TACAS.withCost().to(),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                                SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                                Annotation.zero(1),
                                Annotation.zero(1)))
                 */
                "SplayTree.contains_eq",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1), Annotation.zero(0))))),
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of("SplayHeap/partition", SPLAY_HEAP_PARTITION_TIGHT))),
        Arguments.of(
            Map.of(
                "SplayHeap.del_min",
                    Config.of(
                        CombinedFunctionAnnotation.of(
                            SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                            new Annotation(
                                List.of(ONE), Map.of(unitIndex(1), new KnownCoefficient(1)), "Q'"),
                            SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                            SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                            Annotation.zero(1),
                            Annotation.zero(1))),
                "SplayHeap.insert",
                    Config.of(
                        CombinedFunctionAnnotation.of(
                            SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                            new Annotation(
                                List.of(ONE), Map.of(unitIndex(1), new KnownCoefficient(1)), "Q'"),
                            Annotation.zero(1),
                            Annotation.zero(1))),
                "SplayHeap.partition",
                    Config.of(
                        CombinedFunctionAnnotation.of(
                            new Annotation(
                                List.of(ONE),
                                Map.of(
                                    List.of(1, 0),
                                    new KnownCoefficient(3),
                                    unitIndex(1),
                                    new KnownCoefficient(2)),
                                "Q"),
                            new Annotation(
                                List.of(ONE), Map.of(unitIndex(1), new KnownCoefficient(1)), "Q'"),
                            P,
                            P,
                            P2,
                            P2,
                            Annotation.zero(1),
                            Annotation.zero(1))))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-fiddle", SPLAY_TIGHT))),
        Arguments.of(
            Map.of(
                "PairingHeap.pass2",
                Config.of(
                    "PairingHeap/pass2",
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Q'", 1),
                        zero(1),
                        zero(1),
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'"),
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Q'", 1))))),
        Arguments.of(
            Map.of(
                "PairingHeap.pass1",
                Config.of(
                    "PairingHeap/pass1",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(THREE),
                            Map.of(
                                List.of(1, 0),
                                new KnownCoefficient(2),
                                List.of(0, 2),
                                new KnownCoefficient(1)),
                            "Q"),
                        new Annotation(
                            List.of(ONE),
                            Map.of(
                                List.of(1, 0),
                                new KnownCoefficient(1),
                                List.of(0, 2),
                                new KnownCoefficient(1)),
                            "Q'"),
                        zero(1),
                        zero(1),
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "ddad"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "dda"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "ddadx"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "ddaxxx"))))),

        // [3·p₀ + 4·p₍₁ ₀₎ + 1] → [1·p₀ + 1·p₍₁ ₀₎ + 1]
        Arguments.of(Map.of("PairingHeap.pass2", Config.of("PairingHeap/pass2"))),
        // [2·p₍₁ ₀₎ + 2·p₍₁ ₁₎ + 2·p₍₁ ₂₎] → [2·p₍₁ ₀₎]
        Arguments.of(Map.of("PairingHeap.merge", Config.empty())),
        Arguments.of(
            Map.of(
                "PairingHeap.pass1",
                Config.of(
                    "PairingHeap/pass1",
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Q'", 1),
                        zero(1),
                        zero(1),
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'"))))),
        Arguments.of(Map.of("PairingHeap.pass1", Config.empty())),
        Arguments.of(
            Map.of("PairingHeap.pass1", Config.of("PairingHeap/pass1", passRankAndConstant(2, 1)))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-fiddle", passRankAndOne(3)))),
        Arguments.of(
            Map.of("PairingHeap.pass2", Config.of("PairingHeap/pass2", passRankAndOne(1)))),
        Arguments.of(
            Map.of(
                "PairingHeap.insert",
                Config.empty(),
                "PairingHeap.merge",
                Config.empty(),
                "Tree.singleton",
                Config.empty())),
        Arguments.of(
            Map.of(
                "PairingHeap.del_min",
                Config.of("PairingHeap/del_min"),
                "PairingHeap.pass1",
                Config.of("PairingHeap/pass1"),
                "PairingHeap.pass2",
                Config.of("PairingHeap/pass2"))),
        Arguments.of(
            Map.of(
                "PairingHeap.del_min",
                Config.empty(),
                "PairingHeap.pass1",
                Config.empty(),
                "PairingHeap.pass2",
                Config.empty())),
        Arguments.of(
            Map.of(
                "PairingHeap.link",
                Config.of("PairingHeap/link"),
                "PairingHeap.pass1",
                Config.of("PairingHeap/pass1"),
                "PairingHeap.pass2",
                Config.of("PairingHeap/pass2"))),
        Arguments.of(
            Map.of(
                "PairingHeap.del_min", Config.empty(),
                "PairingHeap.pass1", Config.empty(),
                "PairingHeap.pass2", Config.empty())),
        Arguments.of(
            Map.of(
                "PairingHeap.insert", Config.empty(),
                "PairingHeap.merge", Config.empty(),
                "Tree.singleton", Config.empty())),
        Arguments.of(
            Map.of(
                "PairingHeap.link",
                Config.of("PairingHeap/link"),
                "PairingHeap.pass1",
                Config.of("PairingHeap/pass1"))),
        Arguments.of(
            Map.of(
                "PairingHeap.link",
                Config.of("PairingHeap/link"),
                "PairingHeap.pass2",
                Config.of("PairingHeap/pass2"))),
        Arguments.of(
            Map.of(
                "PairingHeap.link",
                Config.of("PairingHeap/link"),
                "PairingHeap.pass1",
                Config.of("PairingHeap/pass1"),
                "PairingHeap.pass2",
                Config.of("PairingHeap/pass2"),
                "PairingHeap.merge_pairs_via_pass",
                Config.of("PairingHeap/merge_pairs_via_pass"))),
        Arguments.of(
            Map.of(
                "PairingHeap.link",
                Config.of("PairingHeap/link"),
                MERGE_PAIRS,
                Config.of("PairingHeap/merge_pairs", SPLAY_EXPECTED_TACAS))),
        Arguments.of(
            Map.of(
                "PairingHeap.link",
                Config.of("PairingHeap/link"),
                MERGE_PAIRS,
                Config.of("PairingHeap/merge_pairs"))),
        Arguments.of(Map.of("PairingHeap.link", Config.of("PairingHeap/link"))),
        Arguments.of(
            Map.of("PairingHeap.merge_pairs_nolink", Config.of("PairingHeap/merge_pairs_nolink"))),
        Arguments.of(
            Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-fiddle", SPLAY_EXPECTED_TACAS))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-fiddle")))
        /*
        Arguments.of(
                Map.of(
                        "PairingHeap.merge",
                        Config.of("PairingHeap/merge"),
                        "PairingHeap.link",
                        Config.of("PairingHeap/link"),
                        "PairingHeap.merge_pairs",
                        Config.of("PairingHeap/merge_pairs")
                        ))
         */

        /*
        Arguments.of(
                Map.of("Scratch.air", Config.of("Scratch/air", perpetuumMobile(1, 1)))),
        Arguments.of(
                Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-minus"))),
        Arguments.of(
                Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-fixing", SPLAY_EXPECTED_TACAS))),
        Arguments.of(
                Map.of("Tree.singleton", Config.of("Tree/singleton", perpetuumMobile(0, 1)))),
        Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zig", SPLAY_EXPECTED_PASS_ONE))),
         */
        /*
        Arguments.of(
                Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig", SPLAY_TIGHT))),
        Arguments.of(
                Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light", SPLAY_TIGHT))),
         */
        /*
         */

        /*
                   Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-fixing"))),
               Arguments.of(
                   Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light", SPLAY_EXPECTED_TACAS))),
               Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light", SPLAY_EXPECTED_OLD))),
               Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-light"))),
               Arguments.of(
                   Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-light", SPLAY_EXPECTED_TACAS))),
               Arguments.of(
                   Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-light", SPLAY_EXPECTED_OLD))),
               Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-light"))),


                   Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-zagzag", SPLAY_EXPECTED_TACAS))),
                   Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-zagzag", SPLAY_EXPECTED_OLD))),
                   Arguments.of(Map.of(SPLAY_FQN, Config.of("SplayTree/splay_eq-zigzig-zagzag"))),

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
                       Config.of("SplayTree/splay_eq")))
        */
        );
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

    final var optionalProver = program.proveWithTactics(annotations, tactics, true);
    assertTrue(optionalProver.isPresent());

    final var prover = optionalProver.get();

    final Set<Constraint> hackingConstraints = new HashSet<>();

    final List<UnknownCoefficient> setCountingRankCoefficients = new ArrayList<>();
    final List<UnknownCoefficient> setCountingNonRankCoefficients = new ArrayList<>();

    final List<UnknownCoefficient> pairwiseDiffRankCoefficients = new ArrayList<>();
    final List<UnknownCoefficient> pairwiseDiffNonRankCoefficients = new ArrayList<>();

    final Set<Constraint> setCountingConstraints = new HashSet<>();
    final Set<Constraint> pairwiseDiffConstraints = new HashSet<>();

    ConstraintSystemSolver.Domain domain =
        annotations.values().stream().noneMatch(CombinedFunctionAnnotation::isNonInteger)
            ? ConstraintSystemSolver.Domain.INTEGER
            : ConstraintSystemSolver.Domain.RATIONAL;

    for (final var fqn : immutableAnnotations.keySet()) {
      if (!program.getFunctionDefinitions().containsKey(fqn)) {
        fail("Could not find function definition for '" + fqn + "'.");
      }

      final var fd = program.getFunctionDefinitions().get(fqn);

      FunctionAnnotation functionAnnotation = fd.getInferredSignature().getAnnotation().get();
      final var setCounting = Optimization.setCounting(functionAnnotation);
      if (setCounting.isPresent()) {
        setCountingRankCoefficients.addAll(setCounting.get().rankCoefficients());
        setCountingNonRankCoefficients.addAll(setCounting.get().nonRankCoefficients());
        setCountingConstraints.addAll(setCounting.get().constraints());
      }

      final var pairwiseDiff = Optimization.pairwiseDiff(functionAnnotation);
      if (pairwiseDiff.isPresent()) {
        pairwiseDiffRankCoefficients.addAll(pairwiseDiff.get().rankCoefficients());
        pairwiseDiffNonRankCoefficients.addAll(pairwiseDiff.get().nonRankCoefficients());
        pairwiseDiffConstraints.addAll(pairwiseDiff.get().constraints());
      }

      if (functionAnnotation.to().size() == 1) {
        hackingConstraints.add(
            new LessThanOrEqualConstraint(
                ONE, functionAnnotation.to().getRankCoefficient(), "(hack) force rank"));
      }

      /*
      if (fd.getFullyQualifiedName().equals(SPLAY_FQN) && tactics.getOrDefault(SPLAY_FQN, Path.of("/")).toString().contains("zigzig-light")) {
        pairwiseDiffConstraints.add(new EqualityConstraint(fd.getAnnotation().from().getRankCoefficient(0), fd.getAnnotation().to().getRankCoefficient(0), "fix rk"));
      }

      if (Set.of(SPLAY_FQN, MERGE_PAIRS).contains(fd.getFullyQualifiedName())) {
        pairwiseDiffConstraints.add(new
                EqualityConstraint(fd.getAnnotation().from().getRankCoefficient(),
        fd.getAnnotation().to().getRankCoefficient(), "hacking"));
      }
       */
    }

    // prover.plot();
    Optional<Map<Coefficient, KnownCoefficient>> solution =
        prover.solve(hackingConstraints, emptyList(), "sat", domain);

    assertTrue(solution.isPresent());
    System.out.println(printTable(prover, solution));
    program.mockIngest(solution);
    prover.plotWithSolution(solution.get());

    if (true
        || immutableAnnotations.values().stream()
            .anyMatch(config -> config.annotation().isEmpty())) {
      final var minimizationConstraints =
          union(union(setCountingConstraints, pairwiseDiffConstraints), hackingConstraints);

      final var minimizationTargets =
          append(
              append(pairwiseDiffRankCoefficients, setCountingRankCoefficients),
              append(pairwiseDiffNonRankCoefficients, setCountingNonRankCoefficients));
      final var minSolution =
          prover.solve(minimizationConstraints, minimizationTargets, "min1", domain);
      program.mockIngest(minSolution);

      final var minimizationTargetsWithSets =
          append(
              append(setCountingRankCoefficients, pairwiseDiffRankCoefficients),
              append(setCountingNonRankCoefficients, pairwiseDiffNonRankCoefficients));
      final var minSetSolution =
          prover.solve(minimizationConstraints, minimizationTargetsWithSets, "min2", domain);
      program.mockIngest(minSetSolution);

      System.out.println(printTable(prover, minSolution));
      System.out.println(printTable(prover, minSetSolution));

      if (!domain.equals(ConstraintSystemSolver.Domain.RATIONAL)) {
        final var minSetSolutionRat =
            prover.solve(
                minimizationConstraints,
                minimizationTargetsWithSets,
                "min2r",
                ConstraintSystemSolver.Domain.RATIONAL);
        program.mockIngest(minSetSolutionRat);

        final var minSetSolutionRat2 =
            prover.solve(
                minimizationConstraints,
                minimizationTargets,
                "min1r",
                ConstraintSystemSolver.Domain.RATIONAL);
        program.mockIngest(minSetSolutionRat2);

        System.out.println(printTable(prover, minSetSolutionRat));
      }
      // prover.plotWithSolution(minSetSolutionRat.get());
      /*
      } else {
        prover.plotWithSolution(solution.get());
      }
       */
    }
  }
}
