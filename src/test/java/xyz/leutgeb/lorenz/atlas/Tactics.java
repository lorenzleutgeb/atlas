package xyz.leutgeb.lorenz.atlas;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.ModuleTest.Qp;
import static xyz.leutgeb.lorenz.atlas.TestUtil.loadAndNormalizeAndInferAndUnshare;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.zero;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient.known;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.*;
import static xyz.leutgeb.lorenz.atlas.util.Z3Support.load;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hipparchus.fraction.Fraction;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.slf4j.impl.SimpleLogger;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;

// @Disabled
public class Tactics {
  static {
    System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "warn");
    System.setProperty(SimpleLogger.LOG_KEY_PREFIX + "xyz.leutgeb.lorenz", "debug");
    System.setProperty(SimpleLogger.SHOW_SHORT_LOG_NAME_KEY, Boolean.TRUE.toString());
    System.setProperty(SimpleLogger.SHOW_THREAD_NAME_KEY, Boolean.FALSE.toString());
  }

  @BeforeAll
  public static void beforeAll() {
    load();
  }

  protected static final Annotation QwithConst =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), ONE), "Q");

  protected static final Annotation Qsmall =
      new Annotation(
          List.of(ONE), Map.of(List.of(1, 1), TWO, List.of(1, 0), ONE, unitIndex(1), ONE), "Q");

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
          List.of(ONE_BY_TWO), Map.of(List.of(1, 0), THREE_BY_TWO, unitIndex(1), ONE), "Q");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation P2 =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "P2");

  protected static final Annotation QpwithConst =
      new Annotation(List.of(ONE), Map.of(unitIndex(1), ONE), "Q'");

  private static final CombinedFunctionAnnotation SPLAY_OLD =
      CombinedFunctionAnnotation.of(QwithConst, QpwithConst, P, P);

  private static final CombinedFunctionAnnotation SPLAY_VARIANT =
      CombinedFunctionAnnotation.of(Qsmall, Qpsmall, P, P);

  private static final CombinedFunctionAnnotation SPLAY_INTERMEDIATE =
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

  public static final CombinedFunctionAnnotation SPLAY_EXPECTED =
      CombinedFunctionAnnotation.of(
          Q3by2,
          Qp,
          SmartRangeHeuristic.DEFAULT.generate("x1", 1),
          SmartRangeHeuristic.DEFAULT.generate("x2", 1));

  public static final CombinedFunctionAnnotation SPLAY_MAX_EXPECTED =
      CombinedFunctionAnnotation.of(
          Q3by2,
          Qp,
          SmartRangeHeuristic.DEFAULT.generate("x1x", 1),
          SmartRangeHeuristic.DEFAULT.generate("x2x", 1));

  public static final CombinedFunctionAnnotation SPLAY_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE_BY_TWO), Map.of(unitIndex(1), FIVE_BY_TWO, List.of(1, 0), TWO), "Q"),
          Qp);

  public static final CombinedFunctionAnnotation SPLAY_DELETE_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE_BY_TWO), Map.of(unitIndex(1), THREE, List.of(1, 0), FIVE_BY_TWO), "Q"),
          Qp);

  public static final CombinedFunctionAnnotation RAND_SPLAY_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(THREE_BY_FOUR), Map.of(unitIndex(1), ONE, List.of(1, 0), known(9, 8)), "Q"),
          new Annotation(List.of(THREE_BY_FOUR), Map.of(unitIndex(1), ONE), "Q'"),
          new FunctionAnnotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8))));

  public static final CombinedFunctionAnnotation RAND_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(THREE_BY_FOUR),
              Map.of(
                  unitIndex(1), ONE,
                  List.of(1, 0), known(15, 16),
                  List.of(1, 1), known(3, 4)),
              "Q"),
          new Annotation(List.of(THREE_BY_FOUR), Map.of(unitIndex(1), ONE), "Q'"),
          new Annotation(List.of(ZERO), Map.of(List.of(1, 1), known(3, 8)), "Q"),
          new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Q"));

  private static Stream<Arguments> scratch() {
    return Stream.of(
        Arguments.of(
            Map.of("RandSplayTree.splay", Config.of("RandSplayTree/splay", RAND_SPLAY_EXPECTED))),
        Arguments.of(
            Map.of(
                "RandSplayTree.insert", Config.of("RandSplayTree/insert", RAND_INSERT_EXPECTED))),
        Arguments.of(
            Map.of(
                "RandSplayTree.splay_max_only",
                    Config.of("RandSplayTree/splay_max_only", RAND_SPLAY_EXPECTED),
                "RandSplayTree.delete", Config.of("RandSplayTree/delete", RAND_SPLAY_EXPECTED))),
        Arguments.of(
            Map.of(
                "Scratch.test9",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(Coefficient.unknown("x1"), Coefficient.unknown("x1")),
                            Map.of(
                                unitIndex(2),
                                Coefficient.unknown("x2"),
                                List.of(1, 0, 0),
                                Coefficient.unknown("x3"),
                                List.of(0, 1, 0),
                                Coefficient.unknown("x5"),
                                List.of(1, 1, 0),
                                Coefficient.unknown("x6")),
                            "Qrec"),
                        new Annotation(
                            List.of(Coefficient.unknown("x1")),
                            Map.of(unitIndex(1), ZERO),
                            "Qrec'"),
                        SmartRangeHeuristic.DEFAULT.generate(2),
                        SmartRangeHeuristic.DEFAULT.generate(1))))),
        Arguments.of(
            Map.of(
                "Scratch.test8",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(Coefficient.unknown("x1")),
                            Map.of(
                                unitIndex(1),
                                Coefficient.unknown("x2"),
                                List.of(1, 0),
                                Coefficient.unknown("x3")),
                            "Qrec"),
                        new Annotation(
                            List.of(Coefficient.unknown("x1")),
                            Map.of(unitIndex(1), ZERO),
                            "Qrec'"),
                        SmartRangeHeuristic.DEFAULT.generate(1),
                        SmartRangeHeuristic.DEFAULT.generate(1))))),

        /*
        Arguments.of(
                Map.of(
                        "RandSplayTree.splay_max",
                        Config.of(
                                "RandSplayTree/splay_max"))),
                                */

        /*
        Arguments.of(
                Map.of(
                        "RandSplayTree.splay_all_zigzig",
                        Config.of(
                                "auto",
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                List.of(THREE_BY_FOUR),
                                                Map.of(unitIndex(1), ONE, List.of(1, 0), known(9, 8)),
                                                "Qrec"),
                                        new Annotation(List.of(THREE_BY_FOUR), Map.of(unitIndex(1), ONE), "Qrec'"),
                                        SmartRangeHeuristic.DEFAULT.generate(1),
                                        SmartRangeHeuristic.DEFAULT.generate(1))))),
         */

        /*
        Arguments.of(
                Map.of(
                        "RandSplayTree.splay_max",
                        Config.of(
                                "RandSplayTree/splay_max",
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                List.of(Coefficient.unknown("rk1")),
                                                Map.of(unitIndex(1), Coefficient.unknown("foo"), List.of(1, 0), Coefficient.unknown("logc")),
                                                "Qrec"),
                                        new Annotation(List.of(Coefficient.unknown("rk1")), Map.of(unitIndex(1), Coefficient.unknown("bar")), "Qrec'"),
                                        SmartRangeHeuristic.DEFAULT.generate(1),
                                        SmartRangeHeuristic.DEFAULT.generate(1))))),
         */

        /*
        Arguments.of(
                Map.of(
                        "RandSplayTree.splay_max",
                        Config.of(
                                "RandSplayTree/splay_max",
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                List.of(Coefficient.unknown("rk1")),
                                                Map.of(unitIndex(1), Coefficient.unknown("foo"), List.of(1, 0), Coefficient.unknown("logc")),
                                                "Qrec"),
                                        new Annotation(List.of(Coefficient.unknown("rk1")), Map.of(unitIndex(1), Coefficient.unknown("bar")), "Qrec'"),
                                        SmartRangeHeuristic.DEFAULT.generate(1),
                                        SmartRangeHeuristic.DEFAULT.generate(1))))),
         */

        Arguments.of(
            Map.of(
                "RandSplayTree.splay",
                Config.of(
                    "RandSplayTree/splay",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(THREE_BY_FOUR),
                            Map.of(unitIndex(1), ONE, List.of(1, 0), known(9, 8)),
                            "Qrec"),
                        new Annotation(List.of(THREE_BY_FOUR), Map.of(unitIndex(1), ONE), "Qrec'"),
                        SmartRangeHeuristic.DEFAULT.generate(1),
                        SmartRangeHeuristic.DEFAULT.generate(1))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        zero(1),
                        zero(1),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf'"),
                        SmartRangeHeuristic.DEFAULT.generate(1),
                        SmartRangeHeuristic.DEFAULT.generate(1))),
                "Scratch.id2",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // c
                            List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q'"))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ZERO), Map.of(), "Qid"),
                        new Annotation(List.of(ZERO), Map.of(), "Qid'"),
                        new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Qidcf'"))),
                "Scratch.test3",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // c
                            List.of(ONE),
                            Map.of(
                                List.of(1, 0), ONE,
                                unitIndex(1), ONE),
                            "Q"),
                        new Annotation(List.of(ONE), Map.of(), "Q'"))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        zero(1),
                        zero(1),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf'"))),
                "Scratch.test4",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // c
                            List.of(ONE),
                            Map.of(
                                List.of(1, 0), ONE,
                                unitIndex(1), ONE),
                            "Qtest"),
                        new Annotation(List.of(ONE), Map.of(), "Qtest'"))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ONE), Map.of(), "Qid"),
                        new Annotation(List.of(ONE), Map.of(), "Qid'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf'"))),
                "Scratch.test2",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // b c
                            List.of(ONE, ONE),
                            Map.of(
                                List.of(1, 1, 0), ONE,
                                List.of(1, 0, 0), ONE,
                                List.of(0, 1, 0), ONE,
                                unitIndex(2), ONE),
                            "Qtest"),
                        new Annotation(List.of(ONE), Map.of(), "Qtest'"))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ZERO), Map.of(), "Qid"),
                        new Annotation(List.of(ZERO), Map.of(), "Qid'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf'"))),
                "Scratch.test6",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // b c
                            List.of(ZERO, ZERO),
                            Map.of(
                                List.of(1, 1, 0), ONE
                                // List.of(0, 1, 1, 0), ONE,
                                // List.of(1, 0, 0, 0), ONE,
                                // List.of(0, 0, 1, 0), ONE,
                                // unitIndex(3), TWO),
                                ),
                            "Qtest"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qtest'"))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ZERO), Map.of(), "Qid"),
                        new Annotation(List.of(ZERO), Map.of(), "Qid'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf'"))),
                "Scratch.test5",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // a b c
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(
                                List.of(1, 1, 1, 0), ONE
                                // List.of(0, 1, 1, 0), ONE,
                                // List.of(1, 0, 0, 0), ONE,
                                // List.of(0, 0, 1, 0), ONE,
                                // unitIndex(3), TWO),
                                ),
                            "Qtest"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qtest'"))))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ONE), Map.of(), "Qid"),
                        new Annotation(List.of(ONE), Map.of(), "Qid'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qidcf'"))),
                "Scratch.test",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // a b c
                            List.of(ONE, ONE, ONE),
                            Map.of(
                                List.of(1, 1, 1, 0), ONE,
                                List.of(0, 1, 1, 0), ONE,
                                List.of(1, 0, 0, 0), ONE,
                                List.of(0, 1, 0, 0), ONE,
                                List.of(0, 0, 1, 0), ONE,
                                unitIndex(3), ONE),
                            "Qtest"),
                        new Annotation(List.of(ONE), Map.of(), "Qtest'"))))),
        Arguments.of(Map.of("Scratch.f2", Config.of("auto"))),
        // UNSAT: Arguments.of(Map.of("Scratch.f3", Config.of("auto"))),
        Arguments.of(
            Map.of(
                "Scratch.id1",
                Config.of(
                    "Scratch/id1",
                    CombinedFunctionAnnotation.of(
                        Annotation.zero(1), Annotation.zero(1),
                        Annotation.zero(1), Annotation.zero(1))),
                "Scratch.id2",
                Config.of(
                    "Scratch/id2",
                    CombinedFunctionAnnotation.of(
                        Annotation.knownConstant(1, "Q", 1), Annotation.knownConstant(1, "Qp", 1))),
                "Scratch.id3",
                Config.of(
                    "Scratch/id3",
                    CombinedFunctionAnnotation.of(
                        Annotation.knownConstant(1, "Q", 1), Annotation.knownConstant(1, "Qp", 0))),
                "Scratch.id4",
                Config.of("Scratch/id4"))),
        Arguments.of(
            Map.of("Scratch.id1", Config.of("Scratch/id1")),
            CombinedFunctionAnnotation.of(
                Annotation.constant(1, "Q", ONE),
                Annotation.constant(1, "Q'", ONE),
                Annotation.zero(1),
                Annotation.zero(1))),
        Arguments.of(
            Map.of(
                "Scratch.f5",
                Config.of(
                    "Scratch/f5",
                    CombinedFunctionAnnotation.of(
                        Annotation.zero(1),
                        Annotation.zero(1),
                        Annotation.zero(1),
                        new Annotation(List.of(ZERO), Map.of(unitIndex(1), ONE_BY_TWO), "Q'"))))),
        Arguments.of(Map.of("Scratch.f7", Config.of("auto"))),
        Arguments.of(Map.of("Rand.f", Config.of("Rand/f"))),
        Arguments.of(Map.of("Rand.g", Config.of("Rand/f"))),
        Arguments.of(Map.of("Rand.h", Config.of("Rand/h"))),
        Arguments.of(Map.of("Rand.flip", Config.of("Rand/flip"))));
  }

  private static Stream<Arguments> todo() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs",
                Config.of(
                    "PairingHeap/merge_pairs",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE_BY_TWO),
                            Map.of(List.of(1, 0), THREE_BY_TWO, unitIndex(1), THREE),
                            "Q"),
                        SmartRangeHeuristic.DEFAULT.generate("Qp", 1),
                        SmartRangeHeuristic.DEFAULT.generate("x1", 1),
                        SmartRangeHeuristic.DEFAULT.generate("x2", 1))),
                "PairingHeap.link",
                Config.of("PairingHeap/link"))),

        // pass_1 ($\dag$) & 2(\log(\size{h}+2)+\log(\size{h}+1))
        // pass_2 ($\dag$) & 2(\log(\size{h}+2)+\log(\size{h}+1))
        Arguments.of(
            Map.of(
                "PairingHeap.pass1",
                Config.of(
                    "PairingHeap/pass1",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(THREE), Map.of(List.of(1, 0), TWO, List.of(0, 2), ONE), "Q"),
                        new Annotation(
                            List.of(ONE), Map.of(List.of(1, 0), ONE, List.of(0, 2), ONE), "Q'"),
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
                            List.of(known(3)),
                            Map.of(List.of(1, 0), known(4), List.of(0, 2), ONE),
                            "Q"),
                        new Annotation(
                            List.of(ONE), Map.of(List.of(1, 0), ONE, List.of(0, 2), ONE), "Q'"),
                        P2,
                        P2,
                        zero(1),
                        zero(1),
                        new Annotation(
                            List.of(ZERO),
                            Map.of(List.of(1, 0), TWO, List.of(1, 1), TWO, List.of(1, 2), TWO),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of("PairingHeap/merge_pairs_isolated"),
                "PairingHeap.link",
                Config.of(),
                "PairingHeap.merge",
                Config.of("PairingHeap/merge"),
                "PairingHeap.pass1",
                Config.of("PairingHeap/pass1"),
                "PairingHeap.pass2",
                Config.of("PairingHeap/pass2"))));
  }

  private static Stream<Arguments> splayTree() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "SplayTree.splay", Config.of("SplayTree/splay", SPLAY_EXPECTED),
                "SplayTree.splay_max", Config.of("SplayTree/splay_max", SPLAY_MAX_EXPECTED),
                "SplayTree.insert", Config.of("SplayTree/insert", SPLAY_INSERT_EXPECTED),
                "SplayTree.delete", Config.of("SplayTree/delete", SPLAY_DELETE_EXPECTED))),
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_OLD))),
        Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_VARIANT))),
        Arguments.of(Map.of("SplayTree.splay", Config.of(SPLAY_EXPECTED))),
        Arguments.of(Map.of("SplayTree.splay_max", Config.of(SPLAY_EXPECTED))));
  }

  private static Stream<Arguments> splayHeap() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE_BY_TWO),
                            Map.of(
                                unitIndex(1), ONE, List.of(1, 0), known(3, 4), List.of(1, 1), ONE),
                            "Q"),
                        new Annotation(List.of(ONE_BY_TWO), Map.of(unitIndex(1), ONE), "Q'"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 1), ONE_BY_TWO), "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), ONE_BY_TWO), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(
                    "SplayHeap/partition",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE_BY_TWO,
                            Map.of(
                                List.of(1, 1), ONE, List.of(1, 0), known(3, 4), unitIndex(1), ONE)),
                        Qp,
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 1), ONE_BY_TWO), "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE_BY_TWO), "Qcf'"))),
                "SplayHeap.insert",
                Config.of(
                    "SplayHeap/insert",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE_BY_TWO,
                            Map.of(
                                List.of(1, 1),
                                ONE,
                                List.of(1, 0),
                                known(3, 4),
                                unitIndex(1),
                                FIVE_BY_TWO)),
                        Qp)),
                "SplayHeap.del_min",
                Config.of(
                    "SplayHeap/del_min",
                    CombinedFunctionAnnotation.of(
                        new Annotation(ONE_BY_TWO, Map.of(List.of(1, 0), ONE, unitIndex(1), ONE)),
                        Qp,
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 1), ONE_BY_TWO), "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), ONE_BY_TWO), "Qcf'"))))) // ,
        /*
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(
                    "SplayHeap/partition-nosize" // ,
                    /*
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            ONE_BY_TWO,
                            Map.of(
                                List.of(1, 1),
                                ONE,
                                List.of(1, 0),
                                Coefficient.of(3, 4),
                                unitIndex(1),
                                ONE)),
                        Qp,
                        SmartRangeHeuristic.DEFAULT.generate("q", 1),
                           SmartRangeHeuristic.DEFAULT.generate("q1", 1)
                           // new Annotation(List.of(ZERO), Map.of(List.of(1, 1), ONE_BY_TWO), "Qcf"),
                           // new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE_BY_TWO), "Qcf'")
                    )
            */
        );
  }

  private static Stream<Arguments> pairingHeap() {
    return Stream.of(
        Arguments.of(Map.of("PairingHeap.merge_pairs_isolated", Config.of(SPLAY_EXPECTED))),
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of("PairingHeap/merge_pairs_isolated", SPLAY_EXPECTED),
                "PairingHeap.del_min_via_merge_pairs_isolated",
                Config.of(
                    "PairingHeap/del_min_via_merge_pairs_isolated",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE_BY_TWO),
                            Map.of(unitIndex(1), TWO, List.of(1, 0), ONE),
                            "Q"),
                        Qp,
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE_BY_TWO), "P"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE_BY_TWO), "P"))),
                "PairingHeap.insert_isolated",
                Config.of(
                    "PairingHeap/insert_isolated",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(ONE_BY_TWO),
                            Map.of(unitIndex(1), TWO, List.of(1, 0), ONE_BY_TWO),
                            "Q"),
                        Qp)))),
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
        Arguments.of(Map.of("PairingHeap.insert_isolated", Config.of())),
        Arguments.of(Map.of("PairingHeap.merge_isolated", Config.of())),
        Arguments.of(Map.of("PairingHeap.insert_isolated", Config.of()))
        /* Regressions?

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


            // -------------------------------------------------------------------------
        // PairingHeap.merge
        // N&B  :    log(|h1| + |h2| + 1) + 2
        // 6
        // Paper: 98 log(|h1| + |h2| + 1) + 3 log(|h1| + 1)
        // Worked on 2020-10-23 01:14, took 6m45s
        // Regression on 2021-01-29
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

  @ParameterizedTest
  @MethodSource({"scratch", "splayTree", "splayHeap", "pairingHeap"})
  // @MethodSource({"todo"})
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

    final Set<Constraint> additional =
        Set.of(
            // new InequalityConstraint(Coefficient.unknown("x1"), ZERO, "(outside)"),
            // new LessThanOrEqualConstraint(Coefficient.unknown("x2"), TWO, "(outside)")
            );

    final var result =
        program.solve(
            annotations,
            tactics,
            true /*!annotations.isEmpty()*/,
            false,
            additional /*emptySet()*/);
    assertTrue(result.isSatisfiable());

    program.printAllInferredSignaturesInOrder(System.out);

    var checkSat = true;

    if (checkSat) {
      // var solverResult = prover.solve(multiTarget.constraints, emptyList(), "sat");
      // assertTrue(solverResult.getSolution().isPresent());
      // System.out.println(printTable(prover, solverResult.getSolution()));
      // program.mockIngest(solverResult.getSolution());
      // prover.plotWithSolution(solverResult.getSolution().get());
    }

    /*
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
      // prover.plotWithSolution(minSetSolutionRat.getSolution().get());
      System.out.println(printTable(prover, minSetSolutionRat.getSolution()));
    }
     */
  }
}
