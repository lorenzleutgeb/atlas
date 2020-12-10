package xyz.leutgeb.lorenz.lac.rules;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.jgrapht.graph.DirectedMultigraph;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.lac.S62;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

@DisplayName("(w) Rule")
public class WTest {
  @Test
  public void constant() {
    Obligation o =
        new Obligation(
            Collections.singletonList(Identifier.get("_", null)),
            Annotation.constant(1, "two", TWO),
            Identifier.get("a", null),
            Annotation.zero(1));
    final var result = W.INSTANCE.apply(o, AnnotatingGlobals.empty());
    assertEquals(1, result.getObligations().size());

    Set<Constraint> constraints = new HashSet<>();
    result.collectInto(constraints);

    constraints.addAll(
        EqualityConstraint.eq(
            result.getObligations().get(0).getContext().getAnnotation(),
            Annotation.constant(1, "expected", ONE),
            "test"));

    final var solverResult = ConstraintSystemSolver.solve(constraints);
    assertTrue(solverResult.hasSolution());
  }

  @CsvSource({
    "0,0,0,0,true",
    "0,1,0,1,true",
    "0,1,1,0,false",
    "0,1,1,1,true",
    "0,2,0,2,true",
    "1,0,0,1,true",
    "1,1,0,1,false",
  })
  @ParameterizedTest
  void withGtKnowledge(
      int a1, int a2, int b1, int b2, boolean expectedSolution, TestInfo testInfo) {
    final var smallerTree = Identifier.predefinedTree("smallerTree");
    final var biggerTree = Identifier.predefinedTree("biggerTree");

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(smallerTree);
    sizeAnalysis.addVertex(biggerTree);
    sizeAnalysis.addEdge(biggerTree, smallerTree, SizeEdge.gt());

    final Annotation smallerPotential =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(
                List.of(0, 1, 0), new KnownCoefficient(a2), // biggerTree
                List.of(1, 0, 0), new KnownCoefficient(a1) // smallerTree
                ),
            "smallerPotential");

    final Annotation biggerPotential =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(
                List.of(0, 1, 0), new KnownCoefficient(b2), // biggerTree
                List.of(1, 0, 0), new KnownCoefficient(b1) // smallerTree
                ),
            "biggerPotential");

    final var solverResult =
        ConstraintSystemSolver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(smallerTree, biggerTree),
                    smallerPotential,
                    biggerPotential,
                    sizeAnalysis)));

    assertEquals(expectedSolution, solverResult.hasSolution());
  }

  @Test
  void lemma17() {
    final var cr = Identifier.predefinedTree("cr");
    final var bl = Identifier.predefinedTree("bl");
    final var br = Identifier.predefinedTree("br");

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(cr);
    sizeAnalysis.addVertex(bl);
    sizeAnalysis.addVertex(br);
    // sizeAnalysis.addEdge(t, cr, SizeEdge.gt());

    // P <= Q
    final Annotation P = S62.Q3;
    final Annotation Q = S62.Q2;

    final var solverResult =
        ConstraintSystemSolver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(cr, bl, br), P, Q, sizeAnalysis)));

    assertTrue(solverResult.hasSolution());
  }

  @Test
  void lemma17simple() {
    final var x = Identifier.predefinedTree("x");
    final var y = Identifier.predefinedTree("y");

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);
    sizeAnalysis.addVertex(y);

    // P <= Q
    final Annotation P =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(List.of(1, 0, 0), ONE, List.of(0, 1, 0), ONE, unitIndex(2), TWO),
            "P");
    final Annotation Q = new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, 0), TWO), "Q");

    final var solverResult =
        ConstraintSystemSolver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(List.of(x, y), P, Q, sizeAnalysis)));

    assertTrue(solverResult.hasSolution());
  }

  @Test
  void lemmaPlus1Y() {
    final var x = Identifier.predefinedTree("x");
    final var y = Identifier.predefinedTree("y");

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);
    sizeAnalysis.addVertex(Identifier.LEAF);
    sizeAnalysis.addEdge(x, Identifier.LEAF, SizeEdge.eq());

    // P <= Q
    final Annotation P = new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 0, 1), ONE), "P");
    final Annotation Q =
        new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 0, 0), ONE, unitIndex(2), ONE), "Q");

    final var solverResult =
        ConstraintSystemSolver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(List.of(x, y), P, Q, sizeAnalysis)));

    assertTrue(solverResult.hasSolution());
  }

  @Test
  void lemmaPlus1() {
    final var x = Identifier.predefinedTree("x");

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);

    // P <= Q
    final Annotation P =
        new Annotation(
            List.of(ZERO),
            Map.of(
                List.of(0, 1), ZERO,
                // List.of(0, 2), ZERO,
                // List.of(1, 0), ZERO,
                List.of(1, 1), ONE,
                List.of(1, 2), ZERO),
            "P");
    final Annotation Q =
        new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE, unitIndex(1), ONE), "Q");

    final var solverResult =
        ConstraintSystemSolver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(List.of(x), P, Q, sizeAnalysis)));

    assertTrue(solverResult.hasSolution());
  }

  private static Stream<Arguments> pairs() {
    return Stream.of(
        Arguments.of(
            new Annotation(ZERO, Map.of(List.of(1, 0), TWO)),
            new Annotation(
                ZERO, Map.of(List.of(1, 2), TWO, List.of(1, 0), ONE, unitIndex(1), TWO))),
        Arguments.of(
            new Annotation(ONE, emptyMap()),
            new Annotation(ONE, Map.of(List.of(1, 0), ONE, unitIndex(1), TWO))),
        Arguments.of(
            new Annotation(ONE, emptyMap()),
            new Annotation(ONE, Map.of(List.of(1, 1), ONE, unitIndex(1), ONE))));
  }

  @ParameterizedTest(name = "Φ({0}) ≤ Φ({1})")
  @MethodSource("pairs")
  void bug(Annotation p, Annotation q) {
    // P <= Q
    final var solverResult =
        ConstraintSystemSolver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(Identifier.DUMMY_TREE_ALPHA),
                    p,
                    q,
                    new DirectedMultigraph<>(SizeEdge.class))));

    assertTrue(solverResult.hasSolution());
  }
}
