package xyz.leutgeb.lorenz.atlas;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.ModuleTest.Qp;
import static xyz.leutgeb.lorenz.atlas.TestUtil.*;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.*;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient.known;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient.unknown;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.*;
import static xyz.leutgeb.lorenz.atlas.util.Z3Support.load;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.slf4j.impl.SimpleLogger;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.heuristics.SmartRangeHeuristic;

// @Disabled
public class Tactics {
  static {
    System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "debug");
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

  protected static final Annotation Q3by2 =
      new Annotation(
          List.of(ONE_BY_TWO), Map.of(List.of(1, 0), THREE_BY_TWO, unitIndex(1), ONE), "Q");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation P2 =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "P2");

  private static FunctionAnnotation logPlusOneToLog(Coefficient c) {
    return new FunctionAnnotation(
        new Annotation(List.of(ZERO), Map.of(List.of(1, 1), c), "Qcf"),
        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), c), "Qcf'"));
  }

  static FunctionAnnotation logToLog(Coefficient c) {
    return new FunctionAnnotation(
        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), c), "Qcf"),
        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), c), "Qcf'"));
  }

  private static Annotation logOnly(Coefficient c) {
    return new Annotation(List.of(ZERO), Map.of(List.of(1, 0), c), "Q");
  }

  public static final CombinedFunctionAnnotation SPLAYTREE_SPLAY_EXPECTED =
      CombinedFunctionAnnotation.of(Q3by2, Qp, logToLog(ONE_BY_TWO));

  public static final CombinedFunctionAnnotation SPLAYTREE_SPLAY_MAX_EXPECTED =
      SPLAYTREE_SPLAY_EXPECTED;

  public static final CombinedFunctionAnnotation SPLAYTREE_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE_BY_TWO), Map.of(unitIndex(1), known(3, 2), List.of(1, 0), TWO), "Q"),
          Qp);

  public static final CombinedFunctionAnnotation SPLAYTREE_DELETE_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE_BY_TWO), Map.of(unitIndex(1), THREE, List.of(1, 0), FIVE_BY_TWO), "Q"),
          Qp);

  public static final CombinedFunctionAnnotation RAND_SPLAYTREE_SPLAY_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(THREE_BY_FOUR),
              Map.of(unitIndex(1), known(3, 4), List.of(1, 0), known(9, 8)),
              "Q"),
          new Annotation(List.of(THREE_BY_FOUR), Map.of(unitIndex(1), known(3, 4)), "Q'"),
          logToLog(known(3, 8)));

  public static final CombinedFunctionAnnotation RAND_SPLAYTREE_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(known(3, 4)),
              Map.of(
                  unitIndex(1), known(3, 4),
                  List.of(1, 0), known(3, 4),
                  List.of(1, 1), known(3, 4)),
              "Q"),
          new Annotation(List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
          logPlusOneToLog(known(3, 8)));

  public static final CombinedFunctionAnnotation RAND_SPLAYHEAP_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(
                  new Annotation(List.of(known(3, 4)), Map.of(unitIndex(1), known(1, 2)), "Qp")
                      .getRankCoefficient()),
              Map.of(
                  unitIndex(1),
                  known(1, 2),
                  List.of(1, 0),
                  known(3, 4),
                  List.of(1, 1),
                  known(3, 4)),
              "Qi"),
          new Annotation(List.of(known(3, 4)), Map.of(unitIndex(1), known(1, 2)), "Qp"),
          logPlusOneToLog(known(3, 8)));

  public static final CombinedFunctionAnnotation RAND_SPLAYHEAP_DELETE_MIN_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(known(3, 4)),
              Map.of(unitIndex(1), known(1, 2), List.of(1, 0), known(3, 4)),
              "Qd"),
          new Annotation(List.of(known(3, 4)), Map.of(unitIndex(1), known(1, 2)), "Qp"),
          new FunctionAnnotation(
              new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf")));

  public static final CombinedFunctionAnnotation SPLAYHEAP_PARTITION_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              ONE_BY_TWO,
              Map.of(List.of(1, 0), known(1, 2), List.of(1, 1), ONE, unitIndex(1), ONE)),
          Qp,
          logPlusOneToLog(ONE_BY_TWO));

  public static final CombinedFunctionAnnotation SPLAYHEAP_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              ONE_BY_TWO,
              Map.of(List.of(1, 1), ONE, List.of(1, 0), known(1, 2), unitIndex(1), known(5, 2))),
          Qp);

  public static final CombinedFunctionAnnotation SPLAYHEAP_DELETE_MIN_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(ONE_BY_TWO, Map.of(List.of(1, 0), ONE, unitIndex(1), ONE)),
          Qp,
          logPlusOneToLog(ONE_BY_TWO));

  public static final CombinedFunctionAnnotation PAIRINGHEAP_MERGE_PAIRS_ISOLATED_EXPECTED =
      SPLAYTREE_SPLAY_EXPECTED;

  public static final CombinedFunctionAnnotation
      PAIRINGHEAP_DELETE_MIN_VIA_MERGE_PAIRS_ISOLATED_EXPECTED =
          CombinedFunctionAnnotation.of(
              new Annotation(
                  List.of(ONE_BY_TWO), Map.of(unitIndex(1), TWO, List.of(1, 0), ONE), "Q"),
              Qp);

  public static final CombinedFunctionAnnotation PAIRINGHEAP_INSERT_ISOLATED_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE_BY_TWO), Map.of(unitIndex(1), TWO, List.of(1, 0), ONE_BY_TWO), "Q"),
          Qp);

  public static final CombinedFunctionAnnotation PAIRINGHEAP_MERGE_ISOLATED_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ONE_BY_TWO, ONE_BY_TWO),
              Map.of(List.of(1, 1, 0), ONE_BY_TWO, unitIndex(2), THREE_BY_TWO),
              "Q"),
          Qp);

  private static Annotation rkOnly(Coefficient c) {
    return new Annotation(List.of(c), Map.of(), "rkonly");
  }

  public static final CombinedFunctionAnnotation RAND_TREE_DESCEND_EXPECTED =
      CombinedFunctionAnnotation.of(logOnly(ONE), zero(1));

  public static final CombinedFunctionAnnotation RAND_SEARCHTREE_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(known(1, 2)),
              Map.of(unitIndex(1), known(1, 2), List.of(1, 0), known(3, 2)),
              "Q"),
          rkOnly(ONE_BY_TWO),
          logPlusOneToLog(known(1, 2)));

  public static final CombinedFunctionAnnotation RAND_SEARCHTREE_DELETE_MAX_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(List.of(known(1, 2)), Map.of(List.of(1, 0), known(3, 2)), "Q"),
          rkOnly(ONE_BY_TWO),
          logToLog(known(1, 4)));

  public static final CombinedFunctionAnnotation RAND_SEARCHTREE_DELETE_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(known(1, 2)),
              Map.of(List.of(1, 0), known(3, 2), unitIndex(1), known(1, 2)),
              "Q"),
          rkOnly(ONE_BY_TWO));

  public static final CombinedFunctionAnnotation RAND_MELDABLEHEAP_MELD_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(
              List.of(ZERO, ZERO),
              Map.of(
                  List.of(1, 0, 0), ONE,
                  List.of(0, 1, 0), ONE),
              "Q"),
          zero(1),
          zero(2),
          zero(1));

  public static final CombinedFunctionAnnotation RAND_MELDABLEHEAP_INSERT_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE, unitIndex(1), ONE), "Q"),
          zero(1),
          zero(1),
          zero(1));

  public static final CombinedFunctionAnnotation RAND_MELDABLEHEAP_DELETE_MIN_EXPECTED =
      CombinedFunctionAnnotation.of(
          new Annotation(List.of(ZERO), Map.of(List.of(1, 0), TWO), "Q"),
          zero(1),
          zero(1),
          zero(1));

  private static Stream<Arguments> coinSearchTree() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "CoinSearchTree.insert", Config.of(RAND_SEARCHTREE_INSERT_EXPECTED),
                "CoinSearchTree.delete", Config.of(RAND_SEARCHTREE_DELETE_EXPECTED))));
  }

  private static Stream<Arguments> tree() {
    return Stream.of(Arguments.of(Map.of("Tree.descend", Config.of(RAND_TREE_DESCEND_EXPECTED))));
  }

  private static Stream<Arguments> randTree() {
    return Stream.of(
        Arguments.of(Map.of("RandTree.descend", Config.of(RAND_TREE_DESCEND_EXPECTED))));
  }

  private static Stream<Arguments> scratch() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "Scratch.testcf2",
                Config.of(CombinedFunctionAnnotation.of(zero(1), zero(1), logPlusOneToLog(ONE))),
                "Scratch.dummyx",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, -1), ONE), "Q"),
                        zero(1))),
                "Scratch.testm1",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, 0), ONE), "Qx"),
                        zero(1))))),
        Arguments.of(
            Map.of(
                /*
                "Scratch.testcf3",
                Config.of("auto", CombinedFunctionAnnotation.of(
                        knownConstant(0, "Q", 1),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Q'")
                )),
                "Scratch.testcf2",
                Config.of("auto", CombinedFunctionAnnotation.of(
                        logPlusOneToLog(ONE)
                )),
                 */
                "Scratch.testcf",
                Config.of("Scratch/testcf", CombinedFunctionAnnotation.of(logPlusOneToLog(ONE))))),
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
                "Scratch.test9",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(unknown("x1"), unknown("x1")),
                            Map.of(
                                unitIndex(2),
                                unknown("x2"),
                                List.of(1, 0, 0),
                                unknown("x3"),
                                List.of(0, 1, 0),
                                unknown("x5"),
                                List.of(1, 1, 0),
                                unknown("x6")),
                            "Qrec"),
                        new Annotation(List.of(unknown("x1")), Map.of(unitIndex(1), ZERO), "Qrec'"),
                        SmartRangeHeuristic.DEFAULT.generate(2),
                        SmartRangeHeuristic.DEFAULT.generate(1))))),
        Arguments.of(
            Map.of(
                "Scratch.test8",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(unknown("x1")),
                            Map.of(unitIndex(1), unknown("x2"), List.of(1, 0), unknown("x3")),
                            "Qrec"),
                        new Annotation(List.of(unknown("x1")), Map.of(unitIndex(1), ZERO), "Qrec'"),
                        SmartRangeHeuristic.DEFAULT.generate(1),
                        SmartRangeHeuristic.DEFAULT.generate(1))))),

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
        Arguments.of(Map.of("Rand.flip", Config.of("Rand/flip"))),
        Arguments.of(
            Map.of(
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), known(3, 4),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzig_rec",
                Config.of(
                    "Scratch/insert_zigzig_rec",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), known(3, 2), // unknown("c1"),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)
                                // bl br cr
                                /*
                                List.of(1, 0, 0, 1), known(9, 8),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 1, 0), known(3, 4)
                                 */
                                ),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.insert_zigzig_build",
                Config.of(
                    "Scratch/insert_zigzig_build",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // al ar br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(4), ONE,
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 0, 0, 0), known(3, 8)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.node_builder_2",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                // al ar br cr
                                unitIndex(4), known(3, 4),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0, 0), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.node_builder_1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                // al ar br cr
                                unitIndex(4), known(5, 4),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 4),
                                List.of(0, 1, 1, 1, 0), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_zigzig_coin",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // al ar br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(4), Coefficient.unknown("c1"),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 0, 0, 0), known(3, 8)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(unitIndex(1), Coefficient.unknown("c2")),
                            "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzig_rec",
                Config.of(
                    // "Scratch/insert_zigzig_rec",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), unknown("c1"),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.node_builder_2",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                // al ar br cr
                                unitIndex(4), known(3, 4),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0, 0), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.node_builder_1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                // al ar br cr
                                unitIndex(4), known(5, 4),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 4),
                                List.of(0, 1, 1, 1, 0), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_zigzig_build",
                Config.of(
                    "Scratch/insert_zigzig_build",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // al ar br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(4), Coefficient.unknown("c1"),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 0, 0, 0), known(3, 8)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(unitIndex(1), Coefficient.unknown("c2")),
                            "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.node_builder_2",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                // al ar br cr
                                unitIndex(4), known(3, 4),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0, 0), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.node_builder_1",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                // al ar br cr
                                unitIndex(4), known(5, 4),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 4),
                                List.of(0, 1, 1, 1, 0), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                /*
                "Scratch.insert_zigzig_coin",
                Config.of(
                        "hole",
                        CombinedFunctionAnnotation.of(
                                new Annotation(
                                        // al ar br cr
                                        List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                                        Map.of(
                                                unitIndex(4), Coefficient.unknown("c1"),
                                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                                List.of(1, 1, 0, 0, 0), known(3, 8)
                                        ),
                                        "Q"),
                                new Annotation(
                                        List.of(known(3, 4)),
                                        Map.of(unitIndex(1), Coefficient.unknown("c2")),
                                        "Q'"),
                                new Annotation(
                                       List.of(ZERO, ZERO, ZERO, ZERO),
                                        Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                                        "Qcf"),
                                new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                */

                "Scratch.insert_zigzig_rec",
                Config.of(
                    "Scratch/insert_zigzig_rec",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), unknown("c1"),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_zigzig_coin",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // al ar br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(4), Coefficient.unknown("c1"),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 0, 0, 0), known(3, 8)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(unitIndex(1), Coefficient.unknown("c2")),
                            "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_zigzig_coin",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // al ar br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(4), Coefficient.unknown("c1"),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 0, 0, 0), known(3, 8)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(unitIndex(1), Coefficient.unknown("c2")),
                            "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzig_rec_coin",
                Config.of(
                    "Scratch/insert_zigzig_rec_coin",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                            // bl + cr) + 3/4log(br + bl + cr + 1)
                            // bl br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), unknown("c1"),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),

        /*
        Arguments.of(
                Map.of(
                        "Scratch.insert_zigzig_coin",
                        Config.of(
                                // "Scratch/insert_zigzig_coin",
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                // al ar br cr
                                                List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                                                Map.of(
                                                        unitIndex(4), Coefficient.unknown("c1"),
                                                        List.of(1, 0, 0, 0, 0), known(3, 4),
                                                        List.of(0, 1, 0, 0, 0), known(3, 4),
                                                        List.of(0, 0, 1, 0, 0), known(3, 4),
                                                        List.of(0, 0, 0, 1, 0), known(3, 4),
                                                        List.of(1, 1, 1, 1, -1), known(3, 4),
                                                        List.of(0, 0, 1, 1, 0), known(3, 8),
                                                        List.of(1, 1, 0, 0, 0), known(3, 8)),
                                                "Q"),
                                        new Annotation(
                                                List.of(known(3, 4)),
                                                Map.of(unitIndex(1), Coefficient.unknown("c2")),
                                                "Q'"),
                                        new Annotation(
                                                List.of(ZERO, ZERO, ZERO, ZERO),
                                                Map.of(List.of(1, 1, 1, 1, 0), known(3, 8)),
                                                "Qcf"),
                                        new Annotation(
                                                List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
         */

        Arguments.of(
            Map.of(
                "Scratch.testx",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        zero(3),
                        zero(1),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO), Map.of(List.of(1, 1, 1, 1), ONE), "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qcf'"))),
                "Scratch.id",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        zero(1),
                        zero(1),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 1), ONE), "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzig_match_cl",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                            // bl + cr) + 3/4log(br + bl + cr + 1)
                            // bl br cr
                            List.of(known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(2), unknown("c1"),
                                List.of(1, 0, 0), known(3, 4),
                                List.of(0, 1, 0), known(3, 4),
                                List.of(1, 1, 0), known(3, 4),
                                List.of(1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO), Map.of(List.of(1, 1, 1), known(3, 8)), "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "RandSplayTree.insert",
                Config.of(
                    "auto",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_zigzig_coin",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // al ar br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(4), Coefficient.unknown("c1"),
                                List.of(1, 0, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1, -1), known(3, 4),
                                List.of(0, 0, 1, 1, 0), known(3, 8),
                                List.of(1, 1, 0, 0, 0), known(3, 8)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(unitIndex(1), Coefficient.unknown("c2")),
                            "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))),
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzig_rec_coin",
                Config.of(
                    "Scratch/insert_zigzig_rec_coin",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                            // bl + cr) + 3/4log(br + bl + cr + 1)
                            // bl br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), unknown("c1"),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        SmartRangeHeuristic.DEFAULT.generate(3),
                        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'")
                        /*
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                         */
                        )))),
        Arguments.of(
            Map.of(
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), unknown("c1"),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzig_match",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                            // bl + cr) + 3/4log(br + bl + cr + 1)
                            // bl br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), unknown("c1"),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),
        Arguments.of(
            Map.of(
                "Scratch.insert_zigzig_match_t_empty",
                Config.of(
                    // "RandSplayTree/insert_zigzig_match_t_empty",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), known(3, 4),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8))
                        // zero(1),
                        // zero(1)
                        )),
                "Scratch.insert_zigzig_match",
                Config.of(
                    /*
                    CombinedFunctionAnnotation.of(
                            new Annotation(
                                    // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                                    // bl + cr) + 3/4log(br + bl + cr + 1)
                                    // bl br cr
                                    List.of(known(3, 4), known(3, 4)),
                                    Map.of(
                                            unitIndex(2), known(3, 4),
                                            List.of(1, 0, 0), known(3, 4),
                                            List.of(0, 1, 0), known(3, 4),
                                            List.of(1, 1, 0), known(3, 4),
                                            List.of(1, 1, 1), known(3, 4)),
                                    "Q"),
                            new Annotation(
                                    List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                            new Annotation(
                                    List.of(ZERO, ZERO),
                                    Map.of(
                                            List.of(1, 1, 1), known(3, 8)
                                    ),
                                    "Qcf"
                            )
                     */

                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                            // bl + cr) + 3/4log(br + bl + cr + 1)
                            // bl br cr
                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(3), known(3, 4),
                                List.of(1, 0, 0, 0), known(3, 4),
                                List.of(0, 1, 0, 0), known(3, 4),
                                List.of(0, 0, 1, 0), known(3, 4),
                                List.of(1, 1, 0, 0), known(3, 4),
                                List.of(1, 1, 1, 0), known(3, 4),
                                List.of(1, 1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        new Annotation(
                            List.of(ZERO, ZERO, ZERO),
                            Map.of(List.of(1, 1, 1, 1), known(3, 8)),
                            "Qcf"),
                        new Annotation(
                            List.of(ZERO), Map.of(List.of(1, 0), known(3, 8)), "Qcf'"))))),

        /*
                    Arguments.of(
                            Map.of(
                                    "Scratch.insert_dummy",
                                    Config.of(
                                            "hole",
                                            CombinedFunctionAnnotation.of(
                                                    new Annotation(
                                                            List.of(known(3, 4)),
                                                            Map.of(
                                                                    unitIndex(1), unknown("c1"),
                                                                    List.of(1, 0), known(3, 4),
                                                                    List.of(1, 1), known(3, 4)),
                                                            "Q"),
                                                    new Annotation(
                                                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                                                    logPlusOneToLog(known(3, 8)))),
                                    "Scratch.insert_zigzig_match",
                                    Config.of(
                                            // "RandSplayTree/insert_zigzig_rec",
                                            CombinedFunctionAnnotation.of(
                                                    new Annotation(
                                                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                                                            // bl + cr) + 3/4log(br + bl + cr + 1)
                                                            // bl br cr
                                                            List.of(known(3, 4), known(3, 4), known(3, 4)),
                                                            Map.of(
                                                                    unitIndex(3), unknown("c1"),
                                                                    List.of(1, 0, 0, 0), known(3, 4),
                                                                    List.of(0, 1, 0, 0), known(3, 4),
                                                                    List.of(0, 0, 1, 0), known(3, 4),
                                                                    List.of(1, 1, 0, 0), known(3, 4),
                                                                    List.of(1, 1, 1, 0), known(3, 4),
                                                                    List.of(1, 1, 1, 1), known(3, 4)),
                                                            "Q"),
                                                    new Annotation(
                                                            List.of(known(3, 4)), Map.of(unitIndex(1), unknown("c2")), "Q'"),
                                                    SmartRangeHeuristic.DEFAULT.generate(3),
                                                    SmartRangeHeuristic.DEFAULT.generate(1))))
                                                    ),
        */

        /*

                    Arguments.of(
                            Map.of(
                                    "Scratch.insert_dummy",
                                    Config.of(
                                            "hole",
                                            CombinedFunctionAnnotation.of(
                                                    new Annotation(
                                                            List.of(known(3, 4)),
                                                            Map.of(
                                                                    unitIndex(1), unknown("c1"),
                                                                    List.of(1, 0), known(3, 4),
                                                                    List.of(1, 1), known(3, 4)),
                                                            "Q"),
                                                    new Annotation(
                                                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                                                    logPlusOneToLog(known(3, 8)))),
                                    "Scratch.insert_zigzig_match_t",
                                    Config.of(
                                            //"RandSplayTree/insert_zigzig_match_t",
                                            CombinedFunctionAnnotation.of(
                                                    new Annotation(
                                                            List.of(known(3, 4)),
                                                            Map.of(
                                                                    unitIndex(1), unknown("c1"),
                                                                    List.of(1, 0), known(3, 4),
                                                                    List.of(1, 1), known(3, 4)),
                                                            "Q"),
                                                    new Annotation(
                                                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                                                    logPlusOneToLog(known(3, 8))
                                            ))
                            )),



        */

        /*
        Arguments.of(
                Map.of(
                        "Scratch.insert",
                        Config.of(
                                "auto",
                                CombinedFunctionAnnotation.of(
                                        new Annotation(
                                                List.of(known(3, 4)),
                                                Map.of(
                                                        unitIndex(1), known(3, 4),
                                                        List.of(1, 0), known(3, 4),
                                                        List.of(1, 1), known(3, 4)),
                                                "Q"),
                                        new Annotation(List.of(known(3, 4)), Map.of(
                                                unitIndex(1), known(3, 4)
                                                ), "Q'"),
                                        logPlusOneToLog(known(3, 8)))))),
         */
        Arguments.of(
            Map.of(
                "Scratch.id_match_match",
                Config.of(
                    "Scratch/id_match_match",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), known(3, 4),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), known(3, 4),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q'"))))),

        // insert_zigzag
        // 1/2 + 3/4rk(ar) + 3/4rk(bl) + 3/4rk(cr) + 3/4log(al) + 3/4log(ar) + 3/4log(bl) +
        // 3/4log(al + ar + bl) + 3/4log(al + ar + bl + cr)

        /*
                    Arguments.of(
                            Map.of(
                                    "RandSplayTree.insert",
                                    Config.of("auto",
                                            CombinedFunctionAnnotation.of(
                                                    new Annotation(
                                                            List.of(known(3, 4)),
                                                            Map.of(
                                                                    unitIndex(1), Coefficient.unknown("c1"),
                                                                    List.of(1, 0), known(3, 4),
                                                                    List.of(1, 1), known(3, 4)),
                                                            "Q"),
                                                    new Annotation(List.of(known(3, 4)), Map.of(

                                                            unitIndex(1), Coefficient.unknown("c2")


                                                    ), "Q'"),
                                                    SmartRangeHeuristic.DEFAULT.generate(1),
                                                    SmartRangeHeuristic.DEFAULT.generate(1))))
                            ),
        */

        Arguments.of(
            Map.of(
                "Scratch.insert_dummy",
                Config.of(
                    "hole",
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            List.of(known(3, 4)),
                            Map.of(
                                unitIndex(1), known(3, 4),
                                List.of(1, 0), known(3, 4),
                                List.of(1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), known(3, 4)), "Q'"),
                        logPlusOneToLog(known(3, 8)))),
                "Scratch.insert_zigzag_match_cl",
                Config.of(
                    CombinedFunctionAnnotation.of(
                        new Annotation(
                            // 3/4log(cr) + 3/4log(br + bl) + 3/4log(br) + 3/4log(bl) + 3/4log(br +
                            // bl + cr) + 3/4log(br + bl + cr + 1)
                            // bl br cr
                            List.of(known(3, 4), known(3, 4)),
                            Map.of(
                                unitIndex(2), unknown("c1"),
                                List.of(1, 0, 0), known(3, 4),
                                List.of(0, 1, 0), known(3, 4),
                                List.of(1, 1, 0), known(3, 4),
                                List.of(1, 1, 1), known(3, 4)),
                            "Q"),
                        new Annotation(
                            List.of(known(3, 4)), Map.of(unitIndex(1), unknown("c2")), "Q'"))))

            /*
                   new Annotation(
                           List.of(known(3, 4), known(3, 4)),
                           Map.of(
                                   // cl cr
                                   unitIndex(2), known(1),
                                   List.of(1, 0, 0), known(3, 4),
                                   List.of(0, 1, 0), known(3, 4),
                                   List.of(1, 1, 0), known(3, 4),
                                   List.of(1, 1, 1), known(3, 4)
                           ),
                           "Q"),
                   new Annotation(List.of(known(3, 4)), Map.of(), "Q'"),
                   SmartRangeHeuristic.DEFAULT.generate(2),
                   SmartRangeHeuristic.DEFAULT.generate(1))))

                   ),
            */

            ));
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

  private static Stream<Arguments> randSplayTree() {
    return Stream.of(
        Arguments.of(
            Map.of(
                // "RandSplayTree.splay_max",
                // Config.of("RandSplayTree/splay_max", RAND_SPLAYTREE_SPLAY_EXPECTED),
                // "RandSplayTree.delete",
                // Config.of("RandSplayTree/delete", RAND_SPLAYTREE_SPLAY_EXPECTED),
                "RandSplayTree.insert", Config.of(RAND_SPLAYTREE_INSERT_EXPECTED) // ,
                // "RandSplayTree.splay",
                // Config.of("RandSplayTree/splay", RAND_SPLAYTREE_SPLAY_EXPECTED)
                )));
  }

  private static Stream<Arguments> randSplayHeap() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "RandSplayHeap.insert",
                Config.of(RAND_SPLAYHEAP_INSERT_EXPECTED),
                "RandSplayHeap.delete_min",
                Config.of(RAND_SPLAYHEAP_DELETE_MIN_EXPECTED))));
  }

  private static Stream<Arguments> randMeldableHeap() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "RandMeldableHeap.meld",
                Config.of(RAND_MELDABLEHEAP_MELD_EXPECTED),
                "RandMeldableHeap.insert",
                Config.of(RAND_MELDABLEHEAP_INSERT_EXPECTED),
                "RandMeldableHeap.delete_min",
                Config.of(RAND_MELDABLEHEAP_DELETE_MIN_EXPECTED))));
  }

  private static Stream<Arguments> splayTree() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "SplayTree.splay", Config.of("SplayTree/splay", SPLAYTREE_SPLAY_EXPECTED),
                "SplayTree.splay_max", Config.of("SplayTree/splay_max", SPLAYTREE_SPLAY_EXPECTED),
                "SplayTree.insert", Config.of("SplayTree/insert", SPLAYTREE_INSERT_EXPECTED),
                "SplayTree.delete", Config.of("SplayTree/delete", SPLAYTREE_DELETE_EXPECTED)))
        // Arguments.of(Map.of("SplayTree.splay", Config.of("SplayTree/splay", SPLAY_OLD)))
        );
  }

  private static Stream<Arguments> splayHeap() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "SplayHeap.partition",
                Config.of(SPLAYHEAP_PARTITION_EXPECTED),
                "SplayHeap.insert",
                Config.of(SPLAYHEAP_INSERT_EXPECTED),
                "SplayHeap.delete_min",
                Config.of(SPLAYHEAP_DELETE_MIN_EXPECTED))));
  }

  private static Stream<Arguments> negative() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "Scratch.singleton",
                Config.of(
                    CombinedFunctionAnnotation.of(zero(0), constant(1, "Q'", known(1, 2)))))));
  }

  private static Stream<Arguments> pairingHeap() {
    return Stream.of(
        Arguments.of(
            Map.of(
                "PairingHeap.merge_pairs_isolated",
                Config.of(PAIRINGHEAP_MERGE_PAIRS_ISOLATED_EXPECTED),
                "PairingHeap.delete_min_via_merge_pairs_isolated",
                Config.of(PAIRINGHEAP_DELETE_MIN_VIA_MERGE_PAIRS_ISOLATED_EXPECTED),
                "PairingHeap.insert_isolated",
                Config.of(PAIRINGHEAP_INSERT_ISOLATED_EXPECTED),
                "PairingHeap.merge_isolated",
                Config.of(PAIRINGHEAP_MERGE_ISOLATED_EXPECTED))) // ,
        /*
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
         */
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
  @MethodSource({
    // "negative",
    // "scratch",
    // "randTree",
    // "randSplayHeap",
    // "randSplayTree",
    // "randMeldableHeap",
    "coinSearchTree",
    // "splayTree",
    // "splayHeap",
    // "pairingHeap",
  })
  public void all(Map<String, Config> immutableAnnotations) {
    final var program = loadAndNormalizeAndInferAndUnshare(immutableAnnotations.keySet());
    final var infer = true;
    final var result =
        program.solve(
            extractAnnotations(immutableAnnotations),
            new HashMap<>(),
            infer,
            true,
            !infer,
            Set.of());
    assertTrue(result.isSatisfiable());

    System.out.println("Annotated:");
    program.printAllAnnotatedBoundsInOrder(System.out);
    System.out.println("Solved:");
    program.printAllInferredBoundsInOrder(System.out);
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
