package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.*;
import static xyz.leutgeb.lorenz.lac.Assertions.assertAnnotationEquals;
import static xyz.leutgeb.lorenz.lac.Assertions.assertContextEquals;
import static xyz.leutgeb.lorenz.lac.Tests.ATREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable.ALPHA;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.jgrapht.graph.DirectedMultigraph;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.Tuple;
import xyz.leutgeb.lorenz.lac.ast.sources.Predefined;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTree;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTreeCf;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Node;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;

public class S61 {
  private static final String NAME = "lnf";

  private static final AnnotationHeuristic HEURISTIC = SmartRangeHeuristic.DEFAULT;

  private static final Annotation Q =
      new Annotation(
          List.of(ONE, ONE, ONE, ONE),
          Map.of(
              List.of(0, 0, 0, 1, 0), ONE,
              List.of(0, 0, 1, 0, 0), ONE,
              List.of(0, 1, 0, 0, 0), ONE,
              List.of(1, 0, 0, 0, 0), ONE,
              List.of(1, 1, 0, 0, 0), ONE,
              List.of(1, 1, 1, 0, 0), ONE),
          "Q");

  private static final Annotation Qp = new Annotation(List.of(ONE), emptyMap(), "Q'");

  private static final Annotation Q1 =
      new Annotation(
          List.of(ONE, ONE, ONE),
          Map.of(
              List.of(0, 0, 1, 0), ONE,
              List.of(0, 1, 0, 0), ONE,
              List.of(1, 0, 0, 0), ONE,
              List.of(1, 1, 0, 0), ONE,
              List.of(1, 1, 1, 0), ONE),
          "Q1");

  private static final Annotation Q1p =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Q1'");

  private static final Annotation Q2 =
      new Annotation(
          List.of(ONE, ONE),
          Map.of(
              List.of(1, 0, 0), ONE,
              List.of(0, 1, 0), ONE),
          "Q2");

  private static final Annotation Q3 =
      new Annotation(
          List.of(ONE, ONE),
          Map.of(
              List.of(1, 1, 0), ONE,
              List.of(1, 0, 0), ONE,
              List.of(0, 1, 0), ONE),
          "Q3");

  private static final Annotation Q3p =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), ONE), "Q3'");

  private static final Annotation Q4 =
      new Annotation(
          List.of(ONE, ONE),
          Map.of(
              List.of(1, 0, 0), ONE,
              List.of(0, 1, 0), ONE,
              List.of(1, 1, 0), ONE),
          "Q4");

  private static final Annotation P110 =
      new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, 0), ONE), "P110");

  private static final Annotation P110p =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P110'");

  @Test
  public void lnf() {
    final var surroundingIntro = new Intro("Test." + NAME, null);

    final var tpp = new Identifier(Predefined.INSTANCE, "t''", ATREE, surroundingIntro);
    final var tppp = new Identifier(Predefined.INSTANCE, "t'''", ATREE, surroundingIntro);
    final var ar = new Identifier(Predefined.INSTANCE, "ar", ATREE, surroundingIntro);
    final var br = new Identifier(Predefined.INSTANCE, "br", ATREE, surroundingIntro);
    final var al = new Identifier(Predefined.INSTANCE, "al", ATREE, surroundingIntro);
    final var cr = new Identifier(Predefined.INSTANCE, "cr", ATREE, surroundingIntro);
    final var c = new Identifier(Predefined.INSTANCE, "c", ALPHA, surroundingIntro);
    final var b = new Identifier(Predefined.INSTANCE, "b", ALPHA, surroundingIntro);
    final var a = new Identifier(Predefined.INSTANCE, "a", ALPHA, surroundingIntro);

    final var nodeBrCCr = new Tuple(Predefined.INSTANCE, List.of(br, c, cr), ATREE);
    final var nodeArBTPrimePrimePrime = new Tuple(Predefined.INSTANCE, List.of(ar, b, tppp), ATREE);
    final var nodeAlATPrimePrime = new Tuple(Predefined.INSTANCE, List.of(al, a, tpp));

    // Then the surrounding let expressions.
    final var ePrime =
        new LetExpression(Predefined.INSTANCE, tppp, nodeBrCCr, nodeArBTPrimePrimePrime, ATREE);
    final var e = new LetExpression(Predefined.INSTANCE, tpp, ePrime, nodeAlATPrimePrime, ATREE);

    final var globals =
        new AnnotatingGlobals(emptyMap(), emptyMap(), new DirectedMultigraph<>(SizeEdge.class));

    final var constraints = new HashSet<Constraint>();

    final var qv = List.of(br.getName(), cr.getName(), ar.getName(), al.getName());
    final var rootObligation = new Obligation(qv, Q, e, HEURISTIC.generate(e));

    final var eResult = LetTree.apply(rootObligation, globals);
    assertEquals(2, eResult.getObligations().size());
    assertEquals(ePrime, eResult.getObligations().get(0).getExpression());
    assertEquals(nodeAlATPrimePrime, eResult.getObligations().get(1).getExpression());
    eResult.collectInto(constraints);

    final var nodeAlATPrimePrimeResult = Node.apply(eResult.getObligations().get(1), globals);
    assertEquals(1, nodeAlATPrimePrimeResult.getObligations().size());
    assertTrue(nodeAlATPrimePrimeResult.getObligations().get(0).isNothing());
    nodeAlATPrimePrimeResult.collectInto(constraints);

    final var ePrimeResult = LetTreeCf.apply(eResult.getObligations().get(0), globals);
    assertEquals(3, ePrimeResult.getObligations().size());
    assertEquals(nodeBrCCr, ePrimeResult.getObligations().get(0).getExpression());
    assertEquals(nodeArBTPrimePrimePrime, ePrimeResult.getObligations().get(1).getExpression());
    assertEquals(nodeBrCCr, ePrimeResult.getObligations().get(2).getExpression());
    ePrimeResult.collectInto(constraints);

    final var nodeBrCCrResult = Node.apply(ePrimeResult.getObligations().get(0), globals);
    assertEquals(1, nodeBrCCrResult.getObligations().size());
    assertTrue(nodeBrCCrResult.getObligations().get(0).isNothing());
    nodeBrCCrResult.collectInto(constraints);

    final var nodeArBTPrimePrimePrimeResult =
        Node.apply(ePrimeResult.getObligations().get(1), globals);
    assertEquals(1, nodeArBTPrimePrimePrimeResult.getObligations().size());
    assertTrue(nodeArBTPrimePrimePrimeResult.getObligations().get(0).isNothing());
    nodeArBTPrimePrimePrimeResult.collectInto(constraints);

    Optional<Map<Coefficient, KnownCoefficient>> optionalSolution =
        ConstraintSystemSolver.solve(constraints, NAME);

    assertTrue(optionalSolution.isPresent());

    final Map<Coefficient, KnownCoefficient> solution = optionalSolution.get();

    final var q = rootObligation.getContext().substitute(solution);
    final var qp = rootObligation.getAnnotation().substitute(solution);
    final var q1 = eResult.getObligations().get(0).getContext().substitute(solution);
    final var q1p = eResult.getObligations().get(0).getAnnotation().substitute(solution);
    final var q2 = eResult.getObligations().get(1).getContext().substitute(solution);
    final var q2p = eResult.getObligations().get(1).getAnnotation().substitute(solution);
    final var q3 = ePrimeResult.getObligations().get(0).getContext().substitute(solution);
    final var q3p = ePrimeResult.getObligations().get(0).getAnnotation().substitute(solution);
    final var q4 = ePrimeResult.getObligations().get(1).getContext().substitute(solution);
    final var q4p = ePrimeResult.getObligations().get(1).getAnnotation().substitute(solution);
    final var p110 = ePrimeResult.getObligations().get(2).getContext().substitute(solution);
    final var p110p = ePrimeResult.getObligations().get(2).getAnnotation().substitute(solution);

    final var brAndCr = List.of(br.getName(), cr.getName());

    assertAll(
        () -> assertAnnotationEquals(Q, q.getAnnotation()),
        () -> assertAnnotationEquals(Qp, qp),
        () -> assertContextEquals(List.of(br.getName(), cr.getName(), ar.getName()), Q1, q1),
        () -> assertAnnotationEquals(Q1p, q1p),
        () -> assertContextEquals(List.of(al.getName(), tpp.getName()), Q2, q2),
        () -> assertAnnotationEquals(Qp, q2p),
        () -> assertAnnotationEquals(Q3p, q3p),
        () -> assertContextEquals(brAndCr, Q3, q3),
        () -> assertContextEquals(List.of(ar.getName(), tppp.getName()), Q4, q4),
        () -> assertAnnotationEquals(Q1p, q4p),
        () -> assertContextEquals(brAndCr, P110, p110),
        () -> assertAnnotationEquals(P110p, p110p));
  }
}
