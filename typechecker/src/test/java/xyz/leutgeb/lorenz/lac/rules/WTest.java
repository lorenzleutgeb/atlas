package xyz.leutgeb.lorenz.lac.rules;

import static com.google.common.collect.Sets.difference;
import static com.google.common.collect.Sets.symmetricDifference;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
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
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
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

    final var solution = ConstraintSystemSolver.solve(constraints, "test");
    assertTrue(solution.isPresent());
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

    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(smallerTree, biggerTree),
                    smallerPotential,
                    biggerPotential,
                    sizeAnalysis)),
            testInfo.getDisplayName());

    assertEquals(expectedSolution, solution.isPresent());
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

    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(cr, bl, br), P, Q, sizeAnalysis)));

    assertTrue(solution.isPresent());
    System.out.println(solution.get());
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

    final var solution =
        ConstraintSystemSolver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(List.of(x, y), P, Q, sizeAnalysis)));

    assertTrue(solution.isPresent());
    System.out.println(solution.get());
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

    final var solution =
        ConstraintSystemSolver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(List.of(x, y), P, Q, sizeAnalysis)));

    assertTrue(solution.isPresent());
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

    final var solution =
        ConstraintSystemSolver.solve(
            Set.copyOf(
                W.compareCoefficientsLessOrEqualUsingFarkas(List.of(x), P, Q, sizeAnalysis)));

    assertTrue(solution.isPresent());
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
    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(
                W.compareCoefficientsLessOrEqualUsingFarkas(
                    List.of(Identifier.DUMMY_TREE_ALPHA),
                    p,
                    q,
                    new DirectedMultigraph<>(SizeEdge.class))));

    assertTrue(solution.isPresent());
  }

  @Test
  void monotonicityNew() {
    final var numbers =
        SmartRangeHeuristic.DEFAULT.generate(2).collect(Collectors.toUnmodifiableList());
    final var pairs = Lists.cartesianProduct(numbers, numbers);

    final var knowLt = Set.of(new W.LessThan<>(0, 1));
    final var knowEq = Set.of(new W.Equal<>(0, 1));

    System.out.println("NO");

    final var resultAllWithout =
        Set.copyOf(W.lessThanOrEqualNew(numbers, emptySet(), emptySet(), emptySet()));
    // resultAllWithout.stream().forEach(pair -> System.out.println(" all: " + pair));
    final var resultEachWithout =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), emptySet(), emptySet(), Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            // .peek(pair -> System.out.println("each: " + pair))
            .collect(Collectors.toSet());

    System.out.println("each - all: " + difference(resultEachWithout, resultAllWithout));
    System.out.println("all - each: " + difference(resultAllWithout, resultEachWithout));
    System.out.println(" symdiff  :" + symmetricDifference(resultAllWithout, resultEachWithout));
    assertEquals(resultEachWithout, resultAllWithout);

    System.out.println("LT");

    final var resultAllWithLt =
        Set.copyOf(W.lessThanOrEqualNew(numbers, knowLt, emptySet(), emptySet()));
    final var resultEachWithLt =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), knowLt, emptySet(), Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            // .peek(pair -> System.out.println("each: " + pair))
            .collect(Collectors.toSet());

    assertEquals(resultAllWithLt, (resultEachWithLt));
    System.out.println("symmdiff: " + symmetricDifference(resultEachWithLt, resultEachWithout));
    System.out.println(
        "    diff: "
            + Sets.difference(Set.copyOf(resultEachWithLt), Set.copyOf(resultEachWithout)));
    assertTrue(resultEachWithLt.containsAll(resultEachWithout));

    /*
    final var resultZ3With = W.lessThan(numbers, knowLt, emptySet());
    resultZ3With.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    System.out.println("EQ");

    final var resultEachWithEq =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), emptySet(), knowEq, Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            // .peek(pair -> System.out.println("each: " + pair.smaller + " < " + pair.larger))
            .collect(Collectors.toSet());

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithout, resultEachWithEq));
    // System.out.println(
    //    "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq),
    // Set.copyOf(resultEachWithout)));
    assertTrue(resultEachWithEq.containsAll(resultEachWithout));

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithLt, resultEachWithEq));
    // System.out.println(
    // "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq), Set.copyOf(resultEachWithLt)));

    System.out.println("lt - eq: " + difference(resultEachWithLt, resultEachWithEq));
    System.out.println("eq - lt: " + difference(resultEachWithEq, resultEachWithLt));
    // assertTrue(resultEachWithEq.containsAll(resultEachWithLt));

    /*
    final var resultAllWithEq = W.lessThan(numbers, emptySet(), knowEq);
    resultAllWithEq.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    // System.out.println(Sets.difference(Set.copyOf(resultZ3With), resultWith));
    // System.out.println("Missing these: " + Sets.difference(resultWith,
    // Set.copyOf(resultZ3With)));

    // System.out.println(Sets.difference(Set.copyOf(resultZ3Without), resultWithout));
    // System.out.println(Sets.difference(resultWithout, Set.copyOf(resultZ3Without)));

    // assertEquals(Set.copyOf(resultWithout), Set.copyOf(resultZ3Without));
    // assertEquals(Set.copyOf(resultWith), Set.copyOf(resultZ3With));

    /*
    for (var pair : resultWith) {
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWith.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }

    for (var pair : resultWithout) {
      if (!resultWith.contains(pair)) {
        Assertions.fail("Missing result: " + pair.toString());
      }
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWithout.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }
     */

    // System.out.println(Sets.difference(resultWith, resultWithout));

    // Assertions.assertTrue(resultWith.size() > resultWithout.size());
  }

  @Test
  void monotonicity() {
    final var numbers =
        SmartRangeHeuristic.DEFAULT.generate(2).collect(Collectors.toUnmodifiableList());
    final var pairs = Lists.cartesianProduct(numbers, numbers);

    final var knowLt = Set.of(new W.LessThan<>(0, 1));
    final var knowEq = Set.of(new W.Equal<>(0, 1));

    System.out.println("NO");

    // final var resultAllWithout = Set.copyOf(W.lessThanOrEqual(numbers, emptySet(), emptySet()));
    // resultAllWithout.stream().forEach(pair -> System.out.println(" all: " + pair));
    final var resultEachWithout =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), emptySet(), emptySet(), Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            // .peek(pair -> System.out.println("each: " + pair))
            .collect(Collectors.toSet());

    // System.out.println("each - all: " + difference(resultEachWithout, resultAllWithout));
    // System.out.println("all - each: " + difference(resultAllWithout, resultEachWithout));
    // System.out.println(" symdiff  :" + symmetricDifference(resultAllWithout, resultEachWithout));
    // assertEquals(resultEachWithout, resultAllWithout);

    System.out.println("LT");

    final var resultEachWithLt =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), knowLt, emptySet(), Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            // .peek(pair -> System.out.println("each: " + pair))
            .collect(Collectors.toSet());

    System.out.println("symmdiff: " + symmetricDifference(resultEachWithLt, resultEachWithout));
    System.out.println(
        "    diff: "
            + Sets.difference(Set.copyOf(resultEachWithLt), Set.copyOf(resultEachWithout)));
    assertTrue(resultEachWithLt.containsAll(resultEachWithout));

    /*
    final var resultZ3With = W.lessThan(numbers, knowLt, emptySet());
    resultZ3With.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    System.out.println("EQ");

    final var resultEachWithEq =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), emptySet(), knowEq, Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            // .peek(pair -> System.out.println("each: " + pair.smaller + " < " + pair.larger))
            .collect(Collectors.toSet());

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithout, resultEachWithEq));
    // System.out.println(
    //    "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq),
    // Set.copyOf(resultEachWithout)));
    assertTrue(resultEachWithEq.containsAll(resultEachWithout));

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithLt, resultEachWithEq));
    // System.out.println(
    // "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq), Set.copyOf(resultEachWithLt)));

    System.out.println("lt - eq: " + difference(resultEachWithLt, resultEachWithEq));
    System.out.println("eq - lt: " + difference(resultEachWithEq, resultEachWithLt));
    // assertTrue(resultEachWithEq.containsAll(resultEachWithLt));

    /*
    final var resultAllWithEq = W.lessThan(numbers, emptySet(), knowEq);
    resultAllWithEq.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    // System.out.println(Sets.difference(Set.copyOf(resultZ3With), resultWith));
    // System.out.println("Missing these: " + Sets.difference(resultWith,
    // Set.copyOf(resultZ3With)));

    // System.out.println(Sets.difference(Set.copyOf(resultZ3Without), resultWithout));
    // System.out.println(Sets.difference(resultWithout, Set.copyOf(resultZ3Without)));

    // assertEquals(Set.copyOf(resultWithout), Set.copyOf(resultZ3Without));
    // assertEquals(Set.copyOf(resultWith), Set.copyOf(resultZ3With));

    /*
    for (var pair : resultWith) {
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWith.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }

    for (var pair : resultWithout) {
      if (!resultWith.contains(pair)) {
        Assertions.fail("Missing result: " + pair.toString());
      }
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWithout.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }
     */

    // System.out.println(Sets.difference(resultWith, resultWithout));

    // Assertions.assertTrue(resultWith.size() > resultWithout.size());
  }

  @Test
  void monotonicitySingle() {
    final var numbers =
        SmartRangeHeuristic.DEFAULT.generate(1).collect(Collectors.toUnmodifiableList());
    final var pairs = Lists.cartesianProduct(numbers, numbers);

    System.out.println("NO");

    // final var resultAllWithout = Set.copyOf(W.lessThanOrEqual(numbers, emptySet(), emptySet()));
    // resultAllWithout.stream().forEach(pair -> System.out.println(" all: " + pair));
    final var resultEachWithout =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), emptySet(), emptySet(), Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            .peek(pair -> System.out.println("each: " + pair))
            .collect(Collectors.toSet());

    // System.out.println("each - all: " + difference(resultEachWithout, resultAllWithout));
    // System.out.println("all - each: " + difference(resultAllWithout, resultEachWithout));
    // System.out.println(" symdiff  :" + symmetricDifference(resultAllWithout, resultEachWithout));
    // assertEquals(resultEachWithout, resultAllWithout);

    System.out.println("LT");

    /*
    final var resultZ3With = W.lessThan(numbers, knowLt, emptySet());
    resultZ3With.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    System.out.println("EQ");

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithout, resultEachWithEq));
    // System.out.println(
    //    "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq),
    // Set.copyOf(resultEachWithout)));

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithLt, resultEachWithEq));
    // System.out.println(
    // "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq), Set.copyOf(resultEachWithLt)));

    /*
    final var resultAllWithEq = W.lessThan(numbers, emptySet(), knowEq);
    resultAllWithEq.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    // System.out.println(Sets.difference(Set.copyOf(resultZ3With), resultWith));
    // System.out.println("Missing these: " + Sets.difference(resultWith,
    // Set.copyOf(resultZ3With)));

    // System.out.println(Sets.difference(Set.copyOf(resultZ3Without), resultWithout));
    // System.out.println(Sets.difference(resultWithout, Set.copyOf(resultZ3Without)));

    // assertEquals(Set.copyOf(resultWithout), Set.copyOf(resultZ3Without));
    // assertEquals(Set.copyOf(resultWith), Set.copyOf(resultZ3With));

    /*
    for (var pair : resultWith) {
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWith.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }

    for (var pair : resultWithout) {
      if (!resultWith.contains(pair)) {
        Assertions.fail("Missing result: " + pair.toString());
      }
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWithout.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }
     */

    // System.out.println(Sets.difference(resultWith, resultWithout));

    // Assertions.assertTrue(resultWith.size() > resultWithout.size());
  }

  @Test
  void monotonicityNone() {
    final var numbers =
        SmartRangeHeuristic.DEFAULT.generate(0).collect(Collectors.toUnmodifiableList());
    final var pairs = Lists.cartesianProduct(numbers, numbers);

    System.out.println("NO");

    // final var resultAllWithout = Set.copyOf(W.lessThanOrEqual(numbers, emptySet(), emptySet()));
    // resultAllWithout.stream().forEach(pair -> System.out.println(" all: " + pair));
    final var resultEachWithout =
        pairs.stream()
            .filter(
                pair ->
                    W.lessThanOrEqual(
                        pair.get(0), pair.get(1), emptySet(), emptySet(), Collections.emptySet()))
            .map(list -> new W.LessThanOrEqual<>(list.get(0), list.get(1)))
            .peek(pair -> System.out.println("each: " + pair))
            .collect(Collectors.toSet());

    // System.out.println("each - all: " + difference(resultEachWithout, resultAllWithout));
    // System.out.println("all - each: " + difference(resultAllWithout, resultEachWithout));
    // System.out.println(" symdiff  :" + symmetricDifference(resultAllWithout, resultEachWithout));
    // assertEquals(resultEachWithout, resultAllWithout);

    System.out.println("LT");

    /*
    final var resultZ3With = W.lessThan(numbers, knowLt, emptySet());
    resultZ3With.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    System.out.println("EQ");

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithout, resultEachWithEq));
    // System.out.println(
    //    "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq),
    // Set.copyOf(resultEachWithout)));

    // System.out.println("symmdiff: " + symmetricDifference(resultEachWithLt, resultEachWithEq));
    // System.out.println(
    // "    diff: " + Sets.difference(Set.copyOf(resultEachWithEq), Set.copyOf(resultEachWithLt)));

    /*
    final var resultAllWithEq = W.lessThan(numbers, emptySet(), knowEq);
    resultAllWithEq.stream()
        .forEach(pair -> System.out.println(" all: " + pair.smaller + " < " + pair.larger));
     */

    // System.out.println(Sets.difference(Set.copyOf(resultZ3With), resultWith));
    // System.out.println("Missing these: " + Sets.difference(resultWith,
    // Set.copyOf(resultZ3With)));

    // System.out.println(Sets.difference(Set.copyOf(resultZ3Without), resultWithout));
    // System.out.println(Sets.difference(resultWithout, Set.copyOf(resultZ3Without)));

    // assertEquals(Set.copyOf(resultWithout), Set.copyOf(resultZ3Without));
    // assertEquals(Set.copyOf(resultWith), Set.copyOf(resultZ3With));

    /*
    for (var pair : resultWith) {
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWith.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }

    for (var pair : resultWithout) {
      if (!resultWith.contains(pair)) {
        Assertions.fail("Missing result: " + pair.toString());
      }
      final var swapped = List.of(pair.get(1), pair.get(0));
      if (resultWithout.contains(swapped)) {
        if (!pair.equals(swapped)) {
          // assertEquals(swapped, pair);
          Assertions.fail(pair.toString());
        }
      }
    }
     */

    // System.out.println(Sets.difference(resultWith, resultWithout));

    // Assertions.assertTrue(resultWith.size() > resultWithout.size());
  }
}
