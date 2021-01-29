package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.lac.TestUtil.printTable;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.zero;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.THREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient.unknown;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;
import static xyz.leutgeb.lorenz.lac.util.Z3Support.load;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.optimiziation.Optimization;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.Fraction;

public class Tactics {
  @BeforeAll
  public static void beforeAll() {
    load();
  }

  protected static final Annotation Q =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE), "Q");

  protected static final Annotation QwithConst =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), ONE), "Q");

  protected static final Annotation Qsmall =
      new Annotation(
          List.of(ONE), Map.of(List.of(1, 1), TWO, List.of(1, 0), ONE, unitIndex(1), ONE), "Q");

  protected static final Annotation QsmallError =
      new Annotation(List.of(ONE), Map.of(List.of(1, 1), TWO, unitIndex(1), ONE), "Q'");

  protected static final Annotation Qpsmall =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), ONE), "Q'");

  protected static final Annotation Q5by2 =
      new Annotation(
          List.of(ONE),
          Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(5, 2)), unitIndex(1), ONE),
          "Q");

  protected static final Annotation Q5by2p =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), ONE), "Q'");

  protected static final Annotation Q3by2 =
      new Annotation(
          List.of(new KnownCoefficient(new Fraction(1, 2))),
          Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(3, 2)), unitIndex(1), ONE),
          "Q");

  protected static final Annotation Q3by2p =
      new Annotation(
          List.of(new KnownCoefficient(new Fraction(1, 2))), Map.of(unitIndex(1), ONE), "Q'");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation P2 =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "P2");

  protected static final Annotation P3 =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), THREE), "P3");

  protected static final Annotation Prk =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Prk");

  protected static final Annotation Plnf =
      new Annotation(List.of(ZERO, ZERO, ZERO, ZERO), Map.of(List.of(1, 1, 1, 1, 0), ONE), "P");

  protected static final Annotation Qp = new Annotation(List.of(ONE), emptyMap(), "Q'");

  protected static final Annotation QpwithConst =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), ONE), "Q'");

  private static final String INSERT_FQN = "SplayTree.insert_eq";
  private static final String MAX_FQN = "SplayTree.splay_max_eq";
  private static final String CONTAINS_FQN = "SplayTree.contains_eq";
  private static final String LINK_FQN = "PairingHeap.link";
  private static final String MERGE_FQN = "PairingHeap.merge";
  public static final String MERGE_PAIRS = "PairingHeap.merge_pairs";

  private static final CombinedFunctionAnnotation SPLAY_EXPECTED =
      CombinedFunctionAnnotation.of(QwithConst, QpwithConst, P, P);

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
        zero(1),
        zero(1));
  }

  private static CombinedFunctionAnnotation passRank(int q10) {
    return passRankAndConstant(q10, 0);
  }

  private static CombinedFunctionAnnotation passRankAndOne(int q10) {
    return passRankAndConstant(q10, 1);
  }

  private static final CombinedFunctionAnnotation SPLAY_VARIANT =
      CombinedFunctionAnnotation.of(Qsmall, Qpsmall, P, P);

  private static final CombinedFunctionAnnotation SPLAY_TOO_SMALL_ERROR =
      CombinedFunctionAnnotation.of(QsmallError, Qpsmall, P, P);

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
          zero(1),
          zero(1));

  public static final CombinedFunctionAnnotation SPLAY_TIGHTER =
      CombinedFunctionAnnotation.of(
          Q3by2,
          Q3by2p,
          // new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q2cf"),
          // new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(1, 2))),
              "Q2cf"),
          new Annotation(
              List.of(ZERO),
              Map.of(List.of(1, 0), new KnownCoefficient(new Fraction(1, 2))),
              "Q2cf"));

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
          zero(1),
          zero(1));

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

  /*
    AFTER TACAS REU
    931395 INFO Program - PairingHeap.merge_pairs_nolink; PairingHeap.link; PairingHeap.merge; PairingHeap.pass2; PairingHeap.pass1:
  931424 INFO Program - PairingHeap.merge_pairs_nolink h | [[1·p₀ + 1·p₍₁ ₀₎ + 1·p₍₁ ₁₎ + 2·p₍₁ ₂₎ + 1] → [1·p₀ + 1], {0 → 0, [1·p₀ + 1·p₍₁ ₀₎] → [1·p₍₁ ₀₎], [1·p₍₁ ₀₎] → [1·p₍₁ ₀₎]}]
  931450 INFO Program - PairingHeap.link h | [[1·p₀ + 2·p₍₁ ₀₎ + 2] → [1·p₀ + 1·p₍₁ ₀₎ + 2], {[1·p₍₁ ₀₎] → [1·p₍₁ ₀₎], [2·p₍₁ ₀₎] → [2·p₍₁ ₀₎], 0 → 0}]
  931457 INFO Program - PairingHeap.merge h1 h2 | [[1·p₀ + 1·p₁ + 60·p₍₁ ₁ ₂₎ + 4·p₍₁ ₀ ₂₎ + 1·p₍₀ ₁ ₀₎] → [1·p₀], {}]
  931481 INFO Program - PairingHeap.pass2 h | [[3·p₀ + 3·p₍₁ ₀₎ + 2·p₍₁ ₁₎ + 1] → [1·p₀ + 1·p₍₁ ₀₎ + 1], {[2·p₍₁ ₀₎] → [2·p₍₁ ₀₎], 0 → 0, 0 → 0}]
  931503 INFO Program - PairingHeap.pass1 h | [[2·p₀ + 2·p₍₁ ₀₎ + 1·p₍₁ ₁₎ + 2·p₍₁ ₂₎ + 1] → [1·p₀ + 1·p₍₁ ₀₎ + 1], {[2·p₍₁ ₀₎] → [2·p₍₁ ₀₎], [1·p₍₁ ₀₎] → [1·p₍₁ ₀₎], 0 → 0}]
     */

  private static final Annotation SPLAY_HEAP_TO =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), new KnownCoefficient(1)), "Q'");

  private static final CombinedFunctionAnnotation SPLAY_FOR_INSERT =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(Coefficient.of(1, 2)), Map.of(List.of(1, 0), TWO, unitIndex(1), ONE), "Q"),
          new Annotation(
              List.of(Coefficient.of(1, 2)),
              Map.of(List.of(1, 0), Coefficient.of(1, 2), unitIndex(1), ONE),
              "Q'"),
          new Annotation(List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Q1cf"),
          new Annotation(List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Q1cf'"));

  private static CombinedFunctionAnnotation op(Annotation base, boolean cf) {
    if (!cf) {
      return CombinedFunctionAnnotation.of(SmartRangeHeuristic.DEFAULT.generate("Q", 1), base);
    } else {
      return CombinedFunctionAnnotation.of(
          SmartRangeHeuristic.DEFAULT.generate("Q", 1),
          base,
          SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
          SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1));
    }
  }

  private static Stream<Arguments> scratch() {
    return Stream.of(
        /*
        Arguments.of(
                Map.of(
                        "Scratch.test",
                        Config.of(
                                CombinedFunctionAnnotation.of(
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1)), "Q"),
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1)), "Q'"),
                                        Annotation.zero(1),
                                        Annotation.zero(1))))),
        Arguments.of(
                Map.of(
                        "Scratch.test",
                        Config.of(
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Q"),
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Q'"),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        Annotation.zero(1),
                                        Annotation.zero(1))))),
        Arguments.of(
                Map.of(
                        "Scratch.test",
                        Config.of(
                                CombinedFunctionAnnotation.of(
                                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q"),
                                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q'"),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        Annotation.zero(1),
                                        Annotation.zero(1))))),
        // 1/2rk(t) + 2log(t) + 2 -> 1/2rk(splay t) + 1/2log(splay t) + 2
        Arguments.of(
                Map.of(
                        "Scratch.test",
                        Config.of(
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                List.of(Coefficient.of(1, 2)),
                                                Map.of(List.of(1, 0), TWO, unitIndex(1), ONE),
                                                "Q"),
                                        new Annotation(
                                                List.of(Coefficient.of(1, 2)),
                                                Map.of(List.of(1, 0), Coefficient.of(1, 2), unitIndex(1), ONE),
                                                "Q'"),
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Qcf"),
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Qcf'"),
                                        Annotation.zero(1),
                                        Annotation.zero(1))))),
        */

        /*
        Arguments.of(
            Map.of(
                "Scratch.test",
                Config.of(
                    "Scratch/test",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Q"),
                        new Annotation(List.of(ONE), emptyMap(), "Q'"))))),
         */
        );
  }

  private static Stream<Arguments> splayTree() {
    final var splayTree = ModuleTest.Qp;

    return Stream.of(
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_EXPECTED))),
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_VARIANT))),
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_TIGHT))),
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_TIGHTER))),
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay-norm", SPLAY_TIGHTER))),
        Arguments.of(Map.of("SplayTree.splay", Config.of(SPLAY_TIGHTER))),
        Arguments.of(
            Map.of("SplayTree.splay", Config.of(SPLAY_TIGHTER)),
            "SplayTree.delete",
            Config.of(),
            "SplayTree.splay_max",
            Config.of(SPLAY_TIGHTER)),
        Arguments.of(
            Map.of(
                "SplayTree.splay",
                Config.of("SplayTree/splay", SPLAY_TIGHTER),
                "SplayTree.insert",
                Config.of(
                    "SplayTree/insert",
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1), ModuleTest.Qp))
                // "SplayTree.delete", Config.of("SplayTree/delete"),
                // "SplayTree.splay_max", Config.of("SplayTree/splay_max")
                )));
  }

  private static Stream<Arguments> splayHeap() {
    return Stream.of(
        /* Does not work.
        Arguments.of(
            Map.of(
                "SplayHeap.insert",
                Config.of("SplayHeap/insert", op(splayHeap, false)),
                "SplayHeap.partition",
                Config.of("SplayHeap/partition", op(splayHeap, true)),
                "SplayHeap.del_min",
                Config.of("SplayHeap/del_min", op(splayHeap, true)))),
             */
        /* Works.
        Arguments.of(
            Map.of(
                "SplayHeap.insert",
                Config.of("SplayHeap/insert"),
                "SplayHeap.partition",
                Config.of("SplayHeap/partition"),
                "SplayHeap.del_min",
                Config.of("SplayHeap/del_min"))),
             */
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(
                    "SplayHeap/partition-norm",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(Coefficient.of(1, 2)),
                            Map.of(
                                unitIndex(1),
                                ONE,
                                List.of(1, 0),
                                Coefficient.of(3, 4),
                                List.of(1, 1),
                                ONE),
                            "Q"),
                        new Annotation(
                            List.of(Coefficient.of(1, 2)), Map.of(unitIndex(1), ONE), "Q'"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 1), Coefficient.of(1, 2)), "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), Coefficient.of(1, 2)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(
                    "SplayHeap/partition",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE,
                            Map.of(List.of(1, 1), THREE, List.of(1, 0), ONE, unitIndex(1), THREE)),
                        new Annotation(ONE, Map.of(unitIndex(1), ONE)),
                        new Annotation(ZERO, Map.of(List.of(1, 1), ONE)),
                        new Annotation(ZERO, Map.of(List.of(1, 0), ONE)) // ,
                        // new Annotation(ZERO, Map.of(List.of(1, 1), TWO)),
                        // new Annotation(ZERO, Map.of(List.of(1, 0), TWO)),
                        // zero(1),
                        // zero(1)
                        )))),

        // SplayHeap.{insert,del_min}
        // insert   6 log(|t| + 1)                &     3 log(|t| + 1) + 1
        // del_min    log(|t|) + log(|t| + 1)     &     2 log(|t|) + 1
        // 3, 4
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(
                    "SplayHeap/partition",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE,
                            Map.of(List.of(1, 1), THREE, List.of(1, 0), ONE, unitIndex(1), THREE)),
                        new Annotation(ONE, Map.of(unitIndex(1), ONE)),
                        new Annotation(ZERO, Map.of(List.of(1, 1), ONE)),
                        new Annotation(ZERO, Map.of(List.of(1, 0), ONE)) // ,
                        // new Annotation(ZERO, Map.of(List.of(1, 1), TWO)),
                        // new Annotation(ZERO, Map.of(List.of(1, 0), TWO)),
                        // zero(1),
                        // zero(1)
                        )),
                "SplayHeap.insert",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE,
                            Map.of(
                                List.of(1, 1),
                                THREE,
                                List.of(1, 0),
                                ONE,
                                List.of(1, 2),
                                THREE,
                                unitIndex(1),
                                TWO)),
                        new Annotation(ONE, Map.of(unitIndex(1), ONE)))))) // ,

        /*
        Arguments.of(
                Map.of(
                        "SplayHeap.del_min",
                        Config.of(
                                //"SplayHeap/del_min",
                                CombinedFunctionAnnotation.of(
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1), splayHeap,
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        //pairingHeap,
                                        SmartRangeHeuristic.DEFAULT.generate("Qcf1", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Qcf1'", 1),
                                        //SmartRangeHeuristic.DEFAULT.generate("Qcf2", 1),
                                        //SmartRangeHeuristic.DEFAULT.generate("Qcf2'", 1),
                                        zero(1),
                                        zero(1)
                                        )),
                        "SplayHeap.partition",
                        Config.of(
                                // "SplayHeap/partition",
                                CombinedFunctionAnnotation.of(
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                                        //pairingHeap,
                                        SmartRangeHeuristic.DEFAULT.generate("Qcf1", 1),
                                        SmartRangeHeuristic.DEFAULT.generate("Qcf1'", 1),
                                        //SmartRangeHeuristic.DEFAULT.generate("Qcf2", 1),
                                        //SmartRangeHeuristic.DEFAULT.generate("Qcf2'", 1),
                                        zero(1),
                                        zero(1)
                                )),
                        "SplayHeap.insert",
                        Config.of(
                                // "SplayHeap/insert",
                                CombinedFunctionAnnotation.of(
                                        SmartRangeHeuristic.DEFAULT.generate("Q", 1), splayHeap)))), */

        );
  }

  private static Stream<Arguments> pairingHeap() {
    final var pairingHeap = SmartRangeHeuristic.DEFAULT.generate("Q'", 1);

    return Stream.of(
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of("PairingHeap/merge_pairs_isolated"),
                "PairingHeap.del_min_via_merge_pairs_isolated",
                Config.of("PairingHeap/del_min_via_merge_pairs_isolated"),
                "PairingHeap.insert",
                Config.of())),
        // Arguments.of(Map.of("PairingHeap.pass1", Config.of("PairingHeap/pass1"))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of("PairingHeap/merge_pairs_isolated-norm", SPLAY_TIGHTER))),
        Arguments.of(
            Map.of(
                "PairingHeap.del_min_via_merge_pairs_isolated",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1), pairingHeap)),
                "PairingHeap.merge_pairs_isolated",
                Config.of(
                    "PairingHeap/merge_pairs_isolated",
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        // SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        pairingHeap,
                        SmartRangeHeuristic.DEFAULT.generate("Qcf1", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Qcf1'", 1),
                        // SmartRangeHeuristic.DEFAULT.generate("Qcf2", 1),
                        // SmartRangeHeuristic.DEFAULT.generate("Qcf2'", 1),
                        zero(1),
                        zero(1))),
                "PairingHeap.insert_isolated",
                Config.of(
                    // "PairingHeap/insert_isolated",
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1), pairingHeap)))),
        Arguments.of(
            Map.of(
                "PairingHeap.del_min_via_merge_pairs_isolated",
                Config.of(),
                "PairingHeap.merge_pairs_isolated",
                Config.of("PairingHeap/merge_pairs_isolated"))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        Annotation.constant(2, "Q", UnknownCoefficient.unknown("c")),
                        Annotation.zero(1))))),

        /*
        Arguments.of(
            Map.of(
                "PairingHeap.del_min_via_merge_pairs_isolated",
                Config.of("PairingHeap/del_min_via_merge_pairs_isolated"))),
         */
        Arguments.of(
            Map.of(
                "PairingHeap.merge_isolated",
                Config.of(
                    "PairingHeap/merge_isolated",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE, ONE),
                            Map.of(List.of(1, 1, 0), ONE, List.of(0, 0, 2), TWO),
                            "Q"),
                        new Annotation(ONE, Map.of(List.of(0, 2), ONE)),
                        zero(2),
                        zero(1))))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of(
                    "PairingHeap/merge_pairs_isolated",
                    CombinedFunctionAnnotation.of(
                        SPLAY_EXPECTED.withCost.from,
                        SPLAY_EXPECTED.withCost.to,
                        P,
                        P,
                        zero(1),
                        zero(1))))),
        Arguments.of(Map.of("PairingHeap.merge_isolated", Config.of("PairingHeap/merge_isolated"))),

        // 5
        // 3 log(|h|)
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of(
                    "PairingHeap/merge_pairs_isolated",
                    CombinedFunctionAnnotation.of(
                        SPLAY_EXPECTED.withCost.from,
                        SPLAY_EXPECTED.withCost.to,
                        P,
                        P,
                        zero(1),
                        zero(1))))),
        // pass_1 ($\dag$) & 2(\log(\size{h}+2)+\log(\size{h}+1))
        // pass_2 ($\dag$) & 2(\log(\size{h}+2)+\log(\size{h}+1))
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
                        P,
                        P,
                        P2,
                        P2,
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "PairingHeap.pass2",
                Config.of(
                    "PairingHeap/pass2",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(Coefficient.of(3)),
                            Map.of(
                                List.of(1, 0),
                                new KnownCoefficient(4),
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
                        P2,
                        P2,
                        zero(1),
                        zero(1),
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'")))))

        // -------------------------------------------------------------------------
        // PairingHeap.merge
        // N&B  :    log(|h1| + |h2| + 1) + 2
        // 6
        // Paper: 98 log(|h1| + |h2| + 1) + 3 log(|h1| + 1)
        // Worked on 2020-10-23 01:14, took 6m45s
        // Regression on 2021-01-29
        /*
          Arguments.of(
              Map.of(
                  "PairingHeap.merge",
                  Config.of(
                      "PairingHeap/merge",
                      CombinedFunctionAnnotation.of(
                          new Annotation(
                              List.of(ONE, ONE),
                              Map.of(
                                  List.of(1, 1, 2),
                                  new KnownCoefficient(98),
                                  List.of(1, 0, 1),
                                  THREE),
                              "Q"),
                          new Annotation(ONE, Map.of(unitIndex(1), ONE)),
                          new Annotation(
                              List.of(ZERO, ZERO),
                              Map.of(
                                  List.of(0, 1, 2),
                                  Coefficient.of(8),
                                  List.of(1, 0, 2),
                                  Coefficient.of(7),
                                  List.of(1, 1, 2),
                                  Coefficient.of(10)),
                              "Qcf"),
                          zero(1),
                          zero(2),
                          zero(1))))),
          Arguments.of(
              Map.of(
                  "PairingHeap.insert",
                  Config.of(
                      CombinedFunctionAnnotation.of(
                          new Annotation(List.of(ONE), Map.of(List.of(1, 2), Coefficient.of(6)), "Q"),
                          new Annotation(ONE, Map.of(unitIndex(1), ONE)))),
                  "PairingHeap.merge",
                  Config.of(
                      CombinedFunctionAnnotation.of(
                          new Annotation(
                              List.of(ONE, ONE),
                              Map.of(List.of(1, 1, 0), TWO, List.of(0, 0, 2), Coefficient.of(4)),
                              "Q"),
                          new Annotation(ONE, Map.of(List.of(0, 2), TWO)),
                          zero(2),
                          zero(1))))),
          Arguments.of(
              Map.of(
                  "PairingHeap.insert",
                  Config.of(
                      CombinedFunctionAnnotation.of(
                          new Annotation(List.of(ONE), Map.of(List.of(1, 2), Coefficient.of(6)), "Q"),
                          new Annotation(ONE, emptyMap()))),
                  "PairingHeap.merge",
                  Config.of(
                      CombinedFunctionAnnotation.of(
                          new Annotation(
                              List.of(ONE, ONE),
                              Map.of(List.of(1, 1, 0), TWO, List.of(0, 0, 2), THREE),
                              "Q"),
                          new Annotation(ONE, Map.of(List.of(0, 2), ONE)),
                          zero(2),
                          zero(1)))))
        */
        );
  }

  private static Stream<Arguments> afterTacas() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "SplayHeap.del_min_eq",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE, Map.of(List.of(1, 0), ONE, List.of(1, 1), ONE, unitIndex(1), ONE)),
                        new Annotation(ONE, Map.of(unitIndex(1), ONE)),
                        SmartRangeHeuristic.DEFAULT.generate("Qcf", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Qcf'", 1),
                        Annotation.zero(1),
                        Annotation.zero(1))))),
        // SplayTree.splay_max_eq t | [[p₀ + 1 + p₍₁ ₀₎ + p₍₁ ₁₎ + 2·p₍₁ ₂₎] → [p₀ + 1], {[p₍₁ ₀₎] →
        // [p₍₁ ₀₎], 0 → 0}]
        Arguments.of(
            Map.of(
                "SplayTree.insert",
                Config.of(
                    "SplayTree/insert",
                    CombinedFunctionAnnotation.of(
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        new Annotation(ONE, Map.of(unitIndex(1), ONE)))),
                "SplayTree.splay_eq",
                Config.of(
                    "SplayTree/splay_eq",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE, Map.of(List.of(1, 0), Coefficient.of(4), unitIndex(1), ONE)),
                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), ONE)),
                        SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Q'", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Q2", 1),
                        SmartRangeHeuristic.DEFAULT.generate("Q2'", 1),
                        zero(1),
                        zero(1))))),
        Arguments.of(
            Map.of("SplayTree.splay_max_eq", Config.of("SplayTree/splay_max_eq", SPLAY_EXPECTED))),

        // PairingHeap.merge_pairs_isolated h | [[62·p₀ + 122·p₍₁ ₀₎ + 69·p₍₁ ₁₎ + 27·p₍₁ ₂₎] → [p₀
        // + 1], {[2·p₍₁ ₀₎ + 15·p₍₁ ₁₎ + 6·p₍₁ ₂₎] → [p₍₁ ₀₎], [p₍₁ ₀₎ + 14·p₍₁ ₁₎ + 4·p₍₁ ₂₎] → 0,
        // 0 → 0}
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated", Config.of("PairingHeap/merge_pairs_isolated"))),
        Arguments.of(
            Map.of(
                "SplayHeap.del_min_eq",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE, Map.of(List.of(1, 0), ONE, List.of(1, 1), ONE, unitIndex(1), ONE)),
                        new Annotation(ONE, Map.of(unitIndex(1), ONE)),
                        P2,
                        P2,
                        Annotation.zero(1),
                        Annotation.zero(1))))),
        Arguments.of(Map.of("SplayHeap.del_min_eq", Config.of())),

        /*
              Arguments.of(
                      Map.of(
                              "RightList.cons",
                              Config.of(
                                      "RightList/cons",
                                      CombinedFunctionAnnotation.of(
                                              new Annotation(ZERO, Map.of(List.of(1, 0), ONE, unitIndex(1), Coefficient.of(1))),
                                              new Annotation(ZERO, Map.of(List.of(1, 0), ONE)),
                                              zero(1),
                                              zero(1))))),
        */

        /*
        Arguments.of(
                Map.of(
                        "RightList.cons",
                        Config.of(
                        	"RightList/cons",
                                CombinedFunctionAnnotation.of(
                                        new Annotation(ZERO, Map.of(List.of(1, 0), ONE, unitIndex(1), Coefficient.of(1))),
                                        new Annotation(ZERO, Map.of(List.of(1, 0), ONE)),
                                        zero(1),
                                        zero(1))))),
         */
        /*
        Arguments.of(
                Map.of(
                        "RightList.cons",
                        Config.of(
                                CombinedFunctionAnnotation.of(
                                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), Coefficient.of(1))),
                                        new Annotation(ONE, emptyMap()),
                                        zero(1),
                                        zero(1))))),
         */
        /*
              Arguments.of(
                      Map.of(
                              "RightList.cons_cons",
                              Config.of(
                                      //"RightList/cons_cons",
                                      CombinedFunctionAnnotation.of(
                                              new Annotation(ZERO, Map.of(List.of(1, 0), ONE, unitIndex(1), Coefficient.of(3))),
                                              new Annotation(ZERO, Map.of(List.of(1, 0), ONE)),
                                              zero(1),
                                              zero(1))))),
        */
        Arguments.of(Map.of("RightList.cons_cons", Config.of("RightList/cons_cons" /*,

                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE, Map.of(List.of(1, 0), TWO, unitIndex(1), Coefficient.of(3))),
                        new Annotation(ONE, emptyMap()),
                        zero(1),
                        zero(1))*/))),
        Arguments.of(
            Map.of(
                "RightList.cons",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), Coefficient.of(1))),
                        new Annotation(ONE, emptyMap()),
                        zero(1),
                        zero(1))))),
        Arguments.of(
            Map.of(
                "RightList.cons_cons",
                Config.of(
                    // "RightList/cons_cons",
                    CombinedFunctionAnnotation.of(
                        // constant(1, "Q", Coefficient.of(1)),
                        // zero(1),
                        // SmartRangeHeuristic.DEFAULT.generate("Q", 1),
                        new Annotation(ZERO, Map.of(List.of(1, 0), ONE, unitIndex(1), TWO)),
                        new Annotation(ZERO, Map.of(List.of(1, 0), ONE)),
                        // constant(1, "Q", ONE),
                        // constant(1, "Q", ONE),
                        // P,
                        // P
                        zero(1),
                        zero(1))))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_isolated",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE, ONE),
                            Map.of(List.of(1, 1, 0), ONE, List.of(0, 0, 2), TWO),
                            "Q"),
                        new Annotation(ONE, Map.of(List.of(0, 2), ONE)),
                        zero(2),
                        zero(1))))),
        Arguments.of(
            Map.of(
                "SplayTree.insert_test",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), ONE)),
                        new Annotation(ONE, emptyMap()))))),
        Arguments.of(
            Map.of(
                "SplayTree.insert_test",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), TWO)),
                        new Annotation(ONE, emptyMap()))))),
        Arguments.of(
            Map.of(
                "SplayTree.insert_test",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(ONE, Map.of(List.of(1, 1), ONE, unitIndex(1), ONE)),
                        new Annotation(ONE, emptyMap()))))),

        // Arguments.of(Map.of("PairingHeap.merge", Config.empty())),
        // PairingHeap.link; PairingHeap.merge:
        // PairingHeap.link h | [[1·p₀ + 1·p₍₁ ₀₎ + 1] → [1·p₀ + 1], {[1·p₍₁ ₂₎] → 0, 0 → 0}]
        // PairingHeap.merge h1 h2 | [[1·p₀ + 1·p₁ + 6·p₍₁ ₁ ₂₎ + 1·p₍₁ ₀ ₂₎] → [1·p₀], {}]
        // PairingHeap.link; PairingHeap.merge:
        // PairingHeap.link h | [[1·p₀ + 1·p₍₁ ₀₎ + 1·p₍₁ ₁₎ + 1] → [1·p₀ +
        // 1], {0 → 0, 0 → 0, 0 → 0}]
        // PairingHeap.merge h1 h2 | [[1·p₀ + 1·p₁ + 7·p₍₁ ₁ ₁₎ + 1·p₍₁ ₁ ₂₎]
        // → [1·p₀], {}]
        Arguments.of(
            Map.of(
                "PairingHeap.merge",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE, ONE),
                            Map.of(
                                List.of(1, 1, 0),
                                unknown("q110"),
                                List.of(0, 0, 2),
                                unknown("const")),
                            "Q"),
                        new Annotation(ONE, emptyMap()),
                        zero(2),
                        zero(1))))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_isolated",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE, ONE),
                            Map.of(List.of(1, 1, 1), ONE, List.of(0, 0, 2), TWO),
                            "Q"),
                        new Annotation(ONE, Map.of(List.of(0, 2), ONE)),
                        zero(2),
                        zero(1))))),
        Arguments.of(
            Map.of(
                "PairingHeap.insert_isolated",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE), Map.of(List.of(1, 1), ONE, List.of(0, 2), THREE), "Q"),
                        new Annotation(ONE, Map.of(List.of(0, 2), ONE)))))),
        Arguments.of(
            Map.of(
                "PairingHeap.insert_isolated",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE), Map.of(List.of(1, 0), ONE, List.of(0, 2), THREE), "Q"),
                        new Annotation(ONE, Map.of(List.of(0, 2), ONE)))))),
        // PairingHeap.insert_isolated x h | [[1·p₀ + 5] → [1·p₀ + 2], {}]
        // PairingHeap.insert_isolated x h | [[1·p₀ + 2·p₍₁ ₂₎] → [1·p₀], {}]
        Arguments.of(Map.of("PairingHeap.insert_isolated", Config.of())),
        // Arguments.of(Map.of("SplayHeap.partition", Config.empty())),
        Arguments.of(Map.of("PairingHeap.merge_isolated", Config.of())),
        Arguments.of(Map.of("PairingHeap.insert_isolated", Config.of())),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of("PairingHeap/merge_pairs_nolink"),
                "PairingHeap.link",
                Config.of(),
                "PairingHeap.merge",
                Config.of("PairingHeap/merge"),
                "PairingHeap.pass1",
                Config.of(
                    "PairingHeap/pass1"
                    // [2·p₀ + 2·p₍₁ ₀₎ + 1·p₍₁ ₁₎ + 2·p₍₁ ₂₎ + 1] → [1·p₀ + 1·p₍₁ ₀₎ + 1]
                    // {0 → 0, [2·p₍₁ ₀₎] → [2·p₍₁ ₀₎], [1·p₍₁ ₀₎] → [1·p₍₁ ₀₎]}
                    ),
                "PairingHeap.pass2",
                Config.of("PairingHeap/pass2")

                // "PairingHeap.del_min",
                // Config.of("PairingHeap/del_min")
                )),

        // PairingHeap with fixed annotations.
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_nolink",
                Config.of(
                    "PairingHeap/merge_pairs_nolink",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // [1·p₀ + 1·p₍₁ ₀₎ + 1·p₍₁ ₁₎ + 2·p₍₁ ₂₎ + 1] → [1·p₀ + 1]
                            ONE,
                            Map.of(
                                List.of(1, 0),
                                ONE,
                                List.of(1, 1),
                                ONE,
                                List.of(1, 2),
                                TWO,
                                unitIndex(1),
                                ONE)),
                        QpwithConst,
                        // {[1·p₍₁ ₀₎] → [1·p₍₁ ₀₎], 0 → 0}
                        new Annotation(
                            THREE,
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), ONE, List.of(1, 2), THREE)),
                        Annotation.knownConstant(1, "Qcf'", 1),
                        P,
                        P,
                        zero(1),
                        zero(1))),
                "PairingHeap.link",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        // [[1·p₀ + 2·p₍₁ ₀₎ + 2] → [1·p₀ + 1·p₍₁ ₀₎ + 2]
                        new Annotation(ONE, Map.of(List.of(1, 0), TWO, unitIndex(1), TWO)),
                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), TWO)),
                        // {0 → 0, [1·p₍₁ ₀₎] → [1·p₍₁ ₀₎], [2·p₍₁ ₀₎] → [2·p₍₁ ₀₎]}
                        zero(1),
                        zero(1),
                        P,
                        P,
                        P2,
                        P2)),
                "PairingHeap.merge",
                Config.of(
                    "PairingHeap/merge",
                    CombinedFunctionAnnotation.of(
                        // [1·p₀ + 1·p₁ + 1·p₍₁ ₁ ₁₎ + 8·p₍₀ ₁ ₂₎ + 57·p₍₁ ₁ ₂₎ + 5·p₍₁ ₀ ₂₎ + 2·p₍₀
                        // ₁ ₀₎] → [1·p₀]
                        new Annotation(
                            List.of(ONE, ONE),
                            Map.of(
                                List.of(1, 1, 1),
                                ONE,
                                List.of(0, 1, 2),
                                Coefficient.of(8),
                                List.of(1, 1, 2),
                                Coefficient.of(57),
                                List.of(1, 0, 2),
                                Coefficient.of(5),
                                List.of(0, 1, 0),
                                TWO),
                            "Q"),
                        new Annotation(ONE, emptyMap()))),
                "PairingHeap.pass1",
                Config.of(
                    "PairingHeap/pass1",
                    // [2·p₀ + 2·p₍₁ ₀₎ + 1·p₍₁ ₁₎ + 2·p₍₁ ₂₎ + 1] → [1·p₀ + 1·p₍₁ ₀₎ + 1]
                    // {0 → 0, [2·p₍₁ ₀₎] → [2·p₍₁ ₀₎], [1·p₍₁ ₀₎] → [1·p₍₁ ₀₎]}
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            TWO,
                            Map.of(
                                List.of(1, 0),
                                TWO,
                                List.of(1, 1),
                                ONE,
                                List.of(1, 2),
                                TWO,
                                unitIndex(1),
                                ONE)),
                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), ONE)),
                        P,
                        P,
                        P2,
                        P2,
                        zero(1),
                        zero(1))),
                "PairingHeap.pass2",
                Config.of(
                    "PairingHeap/pass2",
                    CombinedFunctionAnnotation.of(
                        // [3·p₀ + 3·p₍₁ ₀₎ + 2·p₍₁ ₁₎ + 1] → [1·p₀ + 1·p₍₁ ₀₎ + 1]
                        new Annotation(
                            THREE,
                            Map.of(List.of(1, 0), THREE, List.of(1, 1), TWO, unitIndex(1), ONE)),
                        new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), ONE)),
                        // {[2·p₍₁ ₀₎] → [2·p₍₁ ₀₎], 0 → 0}
                        new Annotation(ZERO, Map.of(List.of(1, 1), TWO)),
                        new Annotation(ZERO, Map.of(List.of(1, 0), TWO)),
                        P2,
                        P2,
                        zero(1),
                        zero(1))))));
  }

  @ParameterizedTest
  @MethodSource({"scratch", "splayTree", "splayHeap", "pairingHeap"})
  public void all(Map<String, Config> immutableAnnotations)
      throws UnificationError, TypeError, IOException {
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

    final Set<Constraint> hackingConstraints = new HashSet<>();

    final List<List<Coefficient>> targetSums = new ArrayList<>();

    final Set<Constraint> optimizationConstraints = new HashSet<>();

    // System.out.println("domain: " + domain);

    var multiTarget =
        Optimization.combine(
            program,
            immutableAnnotations.keySet(),
            Optimization::squareWeightedComponentWiseDifference);

    /* SplayTree.delete

    var multiTarget =
        Optimization.componentWiseDifferenceAndSum(
            program
                .getFunctionDefinitions()
                .get("SplayTree.delete")
                .getInferredSignature()
                .getAnnotation()
                .get()
                .withCost).get();

    multiTarget.constraints.add(Optimization.forceRank(program.getFunctionDefinitions().get("SplayTree.delete").getInferredSignature().getAnnotation().get().withCost).get());
     */

    // prover.plot();

    var checkSat = false;

    if (checkSat) {
      var solverResult = prover.solve(multiTarget.constraints, emptyList(), "sat");
      assertTrue(solverResult.getSolution().isPresent());
      System.out.println(printTable(prover, solverResult.getSolution()));
      program.mockIngest(solverResult.getSolution());
    }
    // prover.plotWithSolution(solution.get());

    if (immutableAnnotations.values().stream().anyMatch(Config::isUnknown) || !checkSat) {
      final var minSetSolutionRat =
          prover.solve(
              multiTarget.constraints,
              List.of(multiTarget.target),
              "minq",
              ConstraintSystemSolver.Domain.RATIONAL);
      program.mockIngest(minSetSolutionRat.getSolution());
      System.out.println(printTable(prover, minSetSolutionRat.getSolution()));
      assertTrue(minSetSolutionRat.getSolution().isPresent());
    }
  }
}
