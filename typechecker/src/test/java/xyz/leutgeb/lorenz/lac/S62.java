package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyMap;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.THREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.ones;
import static xyz.leutgeb.lorenz.lac.util.Util.zeroCoefficients;

import java.util.List;
import java.util.Map;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;

public class S62 {
  protected static final AnnotationHeuristic HEURISTIC = SmartRangeHeuristic.DEFAULT;
  protected static final String FQN = "SplayTree.splay";

  protected static final Annotation Q =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), ONE), "Q");

  protected static final Annotation Qplus1 =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), TWO), "Q");

  protected static final Annotation Qp = new Annotation(List.of(ONE), emptyMap(), "Q'");

  protected static final Annotation Q1 =
      new Annotation(
          List.of(ONE, ONE),
          Map.of(
              List.of(0, 1, 0), ONE,
              List.of(1, 1, 0), THREE,
              List.of(1, 0, 0), ONE,
              unitIndex(2), ONE),
          "Q1");

  // TODO
  protected static final Annotation Q1shared =
      new Annotation(
          List.of(ONE, ONE),
          Map.of(
              List.of(0, 1, 0), ONE,
              List.of(1, 1, 0), THREE,
              List.of(1, 0, 0), ONE,
              unitIndex(2), ONE),
          "Q1shared");

  protected static final Annotation Q2 =
      new Annotation(
          List.of(ONE, ONE, ONE),
          Map.of(
              List.of(0, 0, 1, 0), ONE,
              List.of(0, 1, 0, 0), ONE,
              List.of(1, 0, 0, 0), ONE,
              List.of(0, 1, 1, 0), ONE,
              List.of(1, 1, 1, 0), THREE,
              unitIndex(3), ONE),
          "Q2");

  protected static final Annotation Q3 =
      new Annotation(
          List.of(ONE, ONE, ONE),
          Map.of(
              List.of(0, 0, 1, 0), ONE,
              List.of(0, 1, 0, 0), THREE,
              List.of(1, 0, 0, 0), ONE,
              List.of(1, 0, 1, 0), ONE,
              List.of(1, 1, 1, 0), ONE,
              unitIndex(3), TWO),
          "Q3");

  protected static final Annotation Q4 =
      new Annotation(
          List.of(ONE, ONE, ONE),
          ones(List.of(0, 1, 0, 0), List.of(1, 0, 0, 0), List.of(1, 1, 0, 0), List.of(1, 1, 1, 0)),
          "Q4");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation Pp =
      new Annotation(zeroCoefficients(1), Map.of(List.of(1, 0), ONE), "P'");

  protected static final Annotation P1110 = P;

  protected static final Annotation P1110p = Pp;

  protected static final Annotation P1 =
      new Annotation(zeroCoefficients(2), Map.of(List.of(1, 1, 0), ONE), "P1");

  protected static final Annotation P2 =
      new Annotation(zeroCoefficients(3), Map.of(List.of(1, 1, 1, 0), ONE), "P2");

  protected static final Annotation P3 =
      new Annotation(zeroCoefficients(3), Map.of(List.of(1, 1, 1, 0), ONE), "P3");

  protected static final Annotation P4 =
      new Annotation(zeroCoefficients(4), ones(List.of(1, 1, 1, 1, 0)), "P4");
}
