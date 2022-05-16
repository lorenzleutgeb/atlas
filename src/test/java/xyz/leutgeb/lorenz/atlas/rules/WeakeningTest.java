package xyz.leutgeb.lorenz.atlas.rules;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient.known;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.*;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.hipparchus.fraction.Fraction;
import org.jgrapht.graph.DirectedMultigraph;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.atlas.typing.resources.rules.W;
import xyz.leutgeb.lorenz.atlas.typing.resources.solving.Solver;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;

@DisplayName("Weakening")
public class WeakeningTest {
  @Test
  public void constant() {
    Obligation o =
        new Obligation(
            Collections.singletonList(IdentifierExpression.get("_", null)),
            Annotation.constant(1, "two", TWO),
            IdentifierExpression.get("a", null),
            Annotation.zero(1),
            true,
            true);
    final var result = W.INSTANCE.apply(o, AnnotatingGlobals.empty());
    assertEquals(1, result.obligations().size());

    Set<Constraint> constraints = new HashSet<>();
    result.collectInto(constraints);

    constraints.addAll(
        EqualityConstraint.eq(
            result.obligations().get(0).getContext().getAnnotation(),
            Annotation.constant(1, "expected", ONE),
            "test"));

    final var solverResult = Solver.solve(constraints);
    assertTrue(solverResult.isSatisfiable());
  }

  @CsvSource({
    "0,0,0,0,true",
    "0,1,0,1,true",
    "0,1,1,0,false",
    "0,1,1,1,true",
    "0,2,0,2,true",
    "1,1,0,1,false",
    // as of 2021-02-01, (w:size) is restricted to knowledge about |t| = 1, and |t| < |t'| is
    // ignored
    // "1,0,0,1,true",
  })
  @ParameterizedTest
  void withGtKnowledge(int a1, int a2, int b1, int b2, boolean expectedSolution) {
    final var smallerTree = IdentifierExpression.predefinedTree("smallerTree");
    final var biggerTree = IdentifierExpression.predefinedTree("biggerTree");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(smallerTree);
    sizeAnalysis.addVertex(biggerTree);
    sizeAnalysis.addEdge(biggerTree, smallerTree, SizeEdge.gt());

    final Annotation smallerPotential =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(
                List.of(0, 1, 0), Coefficient.known(a2), // biggerTree
                List.of(1, 0, 0), Coefficient.known(a1) // smallerTree
                ),
            "smallerPotential");

    final Annotation biggerPotential =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(
                List.of(0, 1, 0), Coefficient.known(b2), // biggerTree
                List.of(1, 0, 0), Coefficient.known(b1) // smallerTree
                ),
            "biggerPotential");

    final var solverResult =
        Solver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(smallerTree, biggerTree),
                    smallerPotential,
                    biggerPotential,
                    sizeAnalysis,
                    Map.of("size", "true"))));

    assertEquals(expectedSolution, solverResult.isSatisfiable());
  }

  @Test
  @Disabled("needs P := Q3 and Q := Q2")
  void lemma17() {
    final var cr = IdentifierExpression.predefinedTree("cr");
    final var bl = IdentifierExpression.predefinedTree("bl");
    final var br = IdentifierExpression.predefinedTree("br");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(cr);
    sizeAnalysis.addVertex(bl);
    sizeAnalysis.addVertex(br);

    // P <= Q
    final Annotation P = Annotation.zero(3);
    final Annotation Q = Annotation.zero(3);

    final var solverResult =
        Solver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(cr, bl, br), P, Q, sizeAnalysis, emptyMap())));

    assertTrue(solverResult.isSatisfiable());
  }

  @Test
  void builder2() {
    final var x = IdentifierExpression.predefinedTree("x");
    final var y = IdentifierExpression.predefinedTree("y");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);
    sizeAnalysis.addVertex(y);

    final Annotation Q =
        new Annotation(
            List.of(known(3, 4), known(3, 4)),
            Map.of(
                // x y
                List.of(0, 1, 0), known(3, 4),
                List.of(1, 0, 0), known(3, 4),
                List.of(1, 1, 0), known(3, 4),
                List.of(1, 1, 1), known(3, 4),
                List.of(1, 1, 2), known(3, 4),
                unitIndex(2), known(6, 4)),
            "Q");

    // Q
    //   3/4 log(y) + 3/4 log(x) + 3/4 log(x+y) + 3/4 log(x+y+1) + 3/4 log(x+y+2) + 6/4
    //
    // >=         (using 3/4 log(x+y+2) >= 3/4 by mono)
    //
    //   3/4 log(y) + 3/4 log(x) + 3/4 log(x+y) + 3/4 log(x+y+1) + ______________ + 9/4
    // P

    final Annotation P =
        new Annotation(
            List.of(known(3, 4), known(3, 4)),
            Map.of(
                // x y
                List.of(0, 1, 0), known(3, 4),
                List.of(1, 0, 0), known(3, 4),
                List.of(1, 1, 0), known(3, 4),
                List.of(1, 1, 1), known(3, 4),
                // HOLE
                unitIndex(2), known(9, 4)),
            "Q");

    final var solverResult =
        Solver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(x, y), P, Q, sizeAnalysis, Map.of("mono", "true"))));

    assertTrue(solverResult.isSatisfiable());
  }

  @Test
  void builder() {
    final var x = IdentifierExpression.predefinedTree("x");
    final var y = IdentifierExpression.predefinedTree("y");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);
    sizeAnalysis.addVertex(y);

    final Annotation Q =
        new Annotation(
            List.of(known(3, 4), known(3, 4)),
            Map.of(
                // x y
                List.of(0, 1, 0), known(3, 4),
                List.of(0, 1, 1), known(3, 4),
                List.of(1, 0, 0), known(3, 4),
                List.of(1, 1, 0), known(3, 4),
                List.of(1, 1, 1), known(3, 4),
                unitIndex(2), known(6, 4)),
            "Q");

    // Q
    //   3/4 log(y) + 3/4 log(x) + 3/4 log(y+1) + 3/4 log(x+y) + 3/4 log(x+y+1) + 6/4
    //
    // >=         (using 3/4 log(y+1) >= 3/4 by mono)
    //
    //   3/4 log(y) + 3/4 log(x) + ____________ + 3/4 log(x+y) + 3/4 log(x+y+1) + 9/4
    // P

    final Annotation P =
        new Annotation(
            List.of(known(3, 4), known(3, 4)),
            Map.of(
                // x y
                List.of(0, 1, 0), known(3, 4),
                // HOLE
                List.of(1, 0, 0), known(3, 4),
                List.of(1, 1, 1), known(3, 4),
                List.of(1, 1, 0), known(3, 4),
                unitIndex(2), known(9, 4)),
            "Q");

    final var solverResult =
        Solver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(x, y), P, Q, sizeAnalysis, Map.of("mono", "true"))));

    assertTrue(solverResult.isSatisfiable());
  }

  @Test
  void probabilsticZigZig() {
    final var t = IdentifierExpression.predefinedTree("t");
    final var br = IdentifierExpression.predefinedTree("br");
    final var cr = IdentifierExpression.predefinedTree("cr");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(t);
    sizeAnalysis.addVertex(br);
    sizeAnalysis.addVertex(cr);

    final Annotation Q =
        new Annotation(
            List.of(THREE_BY_FOUR, THREE_BY_FOUR, THREE_BY_FOUR),
            Map.of(
                List.of(0, 0, 1, 0), THREE_BY_FOUR,
                List.of(1, 1, 0, 0), THREE_BY_FOUR,
                List.of(0, 1, 0, 0), THREE_BY_FOUR,
                List.of(1, 0, 0, 0), THREE_BY_FOUR,
                List.of(1, 1, 1, 0), new KnownCoefficient(new Fraction(9, 8)),
                unitIndex(3), ONE),
            "Q");

    // Q >= P

    final Annotation P =
        new Annotation(
            List.of(THREE_BY_FOUR, THREE_BY_FOUR, THREE_BY_FOUR),
            Map.of(
                List.of(0, 0, 1, 0), THREE_BY_FOUR,
                List.of(1, 1, 0, 0), THREE_BY_FOUR,
                List.of(0, 1, 0, 0), THREE_BY_FOUR,
                List.of(0, 1, 1, 0), new KnownCoefficient(new Fraction(3, 8)),
                List.of(1, 0, 0, 0), new KnownCoefficient(new Fraction(9, 8)),
                List.of(1, 1, 1, 0), new KnownCoefficient(new Fraction(3, 8)),
                unitIndex(3), new KnownCoefficient(new Fraction(7, 4))),
            "P");

    final var solverResult =
        Solver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(t, br, cr), P, Q, sizeAnalysis, Map.of("l2xy", "true"))));

    assertTrue(solverResult.isSatisfiable());
  }

  @Test
  void lemma17simple() {
    final var x = IdentifierExpression.predefinedTree("x");
    final var y = IdentifierExpression.predefinedTree("y");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
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
        Solver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(x, y), P, Q, sizeAnalysis, Map.of("l2xy", "true"))));

    assertTrue(solverResult.isSatisfiable());
  }

  @Test
  void lemmaPlus1Y() {
    final var x = IdentifierExpression.predefinedTree("x");
    final var y = IdentifierExpression.predefinedTree("y");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);
    sizeAnalysis.addVertex(IdentifierExpression.leaf());
    sizeAnalysis.addEdge(x, IdentifierExpression.leaf(), SizeEdge.eq());

    // P <= Q
    final Annotation P = new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, 0), ONE), "P");
    final Annotation Q =
        new Annotation(List.of(ZERO, ZERO), Map.of(List.of(0, 1, 0), ONE, unitIndex(2), ONE), "Q");

    final var solverResult =
        Solver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(x, y), P, Q, sizeAnalysis, Map.of("lp1y", "true"))));

    assertTrue(solverResult.isSatisfiable());
  }

  @Test
  void lemmaPlus1() {
    final var x = IdentifierExpression.predefinedTree("x");

    final var sizeAnalysis = new DirectedMultigraph<IdentifierExpression, SizeEdge>(SizeEdge.class);
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
        Solver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(x), P, Q, sizeAnalysis, Map.of("lp1", "true"))));

    assertTrue(solverResult.isSatisfiable());
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
        Solver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(IdentifierExpression.predefinedTree("_")),
                    p,
                    q,
                    new DirectedMultigraph<>(SizeEdge.class),
                    Map.of("mono", "true"))));

    assertTrue(solverResult.isSatisfiable());
  }
}
