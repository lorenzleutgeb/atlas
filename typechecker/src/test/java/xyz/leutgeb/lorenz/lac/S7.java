package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.Tests.ATREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable.ALPHA;

import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import org.jgrapht.graph.DirectedMultigraph;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.Intro;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.NodeExpression;
import xyz.leutgeb.lorenz.lac.ast.sources.Predefined;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

/**
 * f cl a cr = let t = (cl, a, cr) in leaf
 *
 * <p>thus
 *
 * <p>|t| ≥ |cr|
 *
 * <p>log(|t|) -> x log(|cr|) -> y
 */
public class S7 {
  private static final File OUT = new File("out");

  /**
   * The 1st two columns are the coefficients in Q for cr and t, the 2nd two columns are the
   * coefficients in P for cr and t.
   *
   * <p>The first coefficient multiplies the *smaller* potential, on the *
   */
  @CsvSource({
    "1,0,0,1,false", // log(|t|)  <= log(|cr|)
    "0,1,1,0,true", // log(|cr|) <= log(|t|)
    "1,1,0,1,true", // log(|cr|) <= log(|cr| + |t|)
    "0,1,1,1,false", // log(|cr| + |t|) <= log(|cr|)
    "0,0,0,0,true", // log(0)    <= log(0)
    "0,1,0,1,true", // log(|t|)  <= log(|t|)
    "0,2,0,2,true", // log(2|t|) <= log(2|t|)
  })
  @ParameterizedTest
  void direct(int a1, int a2, int b1, int b2, boolean expectedSolution) {
    String NAME = "Test.weakenDirect" + a1 + a2 + b1 + b2;
    final var surroundingIntro = new Intro(NAME, null);

    final var bigger = new Identifier(Predefined.INSTANCE, "bigger", ATREE, surroundingIntro);
    final var smaller = new Identifier(Predefined.INSTANCE, "smaller", ATREE, surroundingIntro);

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(bigger);
    sizeAnalysis.addVertex(smaller);
    sizeAnalysis.addEdge(bigger, smaller, SizeEdge.gt());

    final var v = List.of(bigger, smaller);

    // bigger
    final Annotation Q =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(
                List.of(0, 1, 0), new KnownCoefficient(a1), // smaller
                List.of(1, 0, 0), new KnownCoefficient(a2) // bigger
                ),
            "Q");

    // smaller
    final Annotation P =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(
                List.of(0, 1, 0), new KnownCoefficient(b1), // smaller
                List.of(1, 0, 0), new KnownCoefficient(b2) // bigger
                ),
            "P");

    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(W.compareCoefficientsLessOrEqualUsingFarkas(v, P, Q, sizeAnalysis)),
            NAME);

    assertEquals(expectedSolution, solution.isPresent());

    if (expectedSolution) {
      System.out.println(solution.get());
    }
  }

  @Test
  void weaken() throws IOException {
    final var surroundingIntro = new Intro("Test.weaken", null);

    final var t = new Identifier(Predefined.INSTANCE, "t", ATREE, surroundingIntro);
    final var a = new Identifier(Predefined.INSTANCE, "a", ALPHA, surroundingIntro);
    final var cr = new Identifier(Predefined.INSTANCE, "cr", ATREE, surroundingIntro);
    final var cl = new Identifier(Predefined.INSTANCE, "cl", ATREE, surroundingIntro);

    // final var crDupl = new Tuple(Predefined.INSTANCE, List.of(cr, a, cr), ATREE);
    final var nodeClACr = new NodeExpression(Predefined.INSTANCE, List.of(cl, a, cr), ATREE);
    final var nodeTACr = new NodeExpression(Predefined.INSTANCE, List.of(t, a, cr), ATREE);
    // final var nodeTACrDupl = new Tuple(Predefined.INSTANCE, List.of(t, a, crDupl), ATREE);

    // Then the surrounding let expressions.
    // final var e = new LetExpression(Predefined.INSTANCE, t, nodeClACr, LEAF, ATREE);
    final var eshared = new LetExpression(Predefined.INSTANCE, t, nodeClACr, nodeTACr, ATREE);
    // final var esharedDup =
    //    new LetExpression(Predefined.INSTANCE, t, nodeClACr, nodeTACrDupl, ATREE);
    final var e = eshared.unshare(new IntIdGenerator(), true);

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    e.analyzeSizes(sizeAnalysis);

    final var exporter = Tests.exporter();
    final var exp = exporter.transform(sizeAnalysis);
    final var viz = Graphviz.fromGraph(exp);
    viz.render(Format.SVG).toOutputStream(new PrintStream(new File(OUT, "weaken-sizes.svg")));

    final var globals = new AnnotatingGlobals(emptyMap(), sizeAnalysis);

    final var qv = List.of(cr, cl);

    Prover p = new Prover("weakening", globals);
    /*
    p.apply(
        new Obligation(qv, HEURISTIC.generate("qgen", 2), e, HEURISTIC.generate("qpgen", 1)),
        W::apply);
     */
    // p.setWeaken(true);

    // The idea is to look for the node expression, but we cannot use equals because of unsharing.
    // Predicate<Obligation> boundary = obligation -> nodeTACr.equals(obligation.getExpression());
    Predicate<Obligation> boundary =
        obligation -> {
          final var obligationExpression = obligation.getExpression();
          if (obligationExpression == null) {
            return false;
          }
          if (!(obligationExpression instanceof NodeExpression)) {
            return false;
          }
          return ((NodeExpression) obligationExpression).getLeft().equals(t);
        };

    final var root =
        new Obligation(
            qv,
            Annotation.knownConstant(2, "lel", 1),
            // HEURISTIC.generate("qgen", 2),
            e,
            Annotation.zero(1)
            // HEURISTIC.generate("qpgen", 1)
            );

    final var remainingObligations = p.proveUntil(root, boundary);
    assertEquals(1, remainingObligations.size());

    p.prove(p.apply(remainingObligations.get(0), W.INSTANCE));
    // p.prove(remainingObligations);

    // Would it make sense to add outside constraints to assert that some coefficients are bigger?
    var solution = p.solve();
    assertTrue(solution.isPresent());
  }

  @Test
  void lemma17() {
    String NAME = "Test.weakenLemma";
    final var surroundingIntro = new Intro(NAME, null);

    final var cr = new Identifier(Predefined.INSTANCE, "cr", ATREE, surroundingIntro);
    final var bl = new Identifier(Predefined.INSTANCE, "bl", ATREE, surroundingIntro);
    final var br = new Identifier(Predefined.INSTANCE, "br", ATREE, surroundingIntro);

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(cr);
    sizeAnalysis.addVertex(bl);
    sizeAnalysis.addVertex(br);
    // sizeAnalysis.addEdge(t, cr, SizeEdge.gt());

    final var v = List.of(cr, bl, br);

    // P <= Q
    final Annotation P = S62.Q3;
    final Annotation Q = S62.Q2;

    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(W.compareCoefficientsLessOrEqualUsingFarkas(v, P, Q, sizeAnalysis)),
            NAME);

    assertTrue(solution.isPresent());
    System.out.println(solution.get());
  }

  @Test
  void lemma17simple() {
    String NAME = "Test.weakenLemmaSimple";
    final var surroundingIntro = new Intro(NAME, null);

    final var x = new Identifier(Predefined.INSTANCE, "x", ATREE, surroundingIntro);
    final var y = new Identifier(Predefined.INSTANCE, "y", ATREE, surroundingIntro);

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);
    sizeAnalysis.addVertex(x);
    sizeAnalysis.addVertex(y);

    final var v = List.of(x, y);

    // P <= Q
    final Annotation P =
        new Annotation(
            List.of(ZERO, ZERO),
            Map.of(List.of(1, 0, 0), ONE, List.of(0, 1, 0), ONE, unitIndex(2), TWO),
            "P");
    final Annotation Q = new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, 0), TWO), "Q");

    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(W.compareCoefficientsLessOrEqualUsingFarkas(v, P, Q, sizeAnalysis)),
            NAME);

    assertTrue(solution.isPresent());
    System.out.println(solution.get());
  }

  @Test
  void bug() {
    String NAME = "Test.bug";
    final var surroundingIntro = new Intro(NAME, null);

    final var x = new Identifier(Predefined.INSTANCE, "x", ATREE, surroundingIntro);

    final var sizeAnalysis = new DirectedMultigraph<Identifier, SizeEdge>(SizeEdge.class);

    final var v = List.of(x);

    // P <= Q
    final Annotation P =
        new Annotation(
            List.of(ZERO),
            Map.of(List.of(1, 2), TWO, List.of(0, 1, 0), ONE, unitIndex(2), TWO),
            "P");
    final Annotation Q = new Annotation(List.of(ZERO, ZERO), Map.of(List.of(1, 1, 0), TWO), "Q");

    final var solution =
        ConstraintSystemSolver.solve(
            new HashSet<>(W.compareCoefficientsLessOrEqualUsingFarkas(v, P, Q, sizeAnalysis)),
            NAME);

    assertTrue(solution.isPresent());
    System.out.println(solution.get());
  }
}
