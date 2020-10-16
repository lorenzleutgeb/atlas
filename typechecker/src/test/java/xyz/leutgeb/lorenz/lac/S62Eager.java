package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.Assertions.assertContextEqualsByPrefixes;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import org.jgrapht.graph.DirectedMultigraph;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.ast.ShareExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTreeCfSimple;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemException;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

/**
 * In this test case we want to check that the coefficients given in the paper are also a solution
 * of the implementation. However, since the (w) rule is not yet implemented we do this in two
 * steps.
 *
 * <p>In the first step, we check the typing judgement "below" the application of (w), i.e.
 *
 * <p>Γ, cr : T, bl : T, br : T | Q_3 |- e_3 : T | Q'
 *
 * <p>In the second step we check the typing judgement "above" the application of (w), i.e.
 *
 * <p>Γ, cr : T, bl : T, br : T | Q_2 |- e_3 : T | Q'
 *
 * <p>The weakening from Q_2 to Q_3 is the part that we want to skip for now.
 */
@Disabled
public class S62Eager extends S62 {
  private static final FunctionDefinition SPLAY;

  private static final IfThenElseExpression E1;
  private static final IfThenElseExpression E2;
  private static final IfThenElseExpression E3;
  private static final IfThenElseExpression INTERMEDIATE;
  private static final LetExpression E4;
  private static final LetExpression Tp;

  private static final DirectedMultigraph<Identifier, SizeEdge> SIZE_ANALYSIS =
      new DirectedMultigraph<>(SizeEdge.class);

  static {
    final var loader = Tests.loader();
    final Program program;
    try {
      program = loader.load(FQN);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    program.normalize();
    try {
      program.infer();
    } catch (UnificationError | TypeError e) {
      throw new RuntimeException(e);
    }
    program.unshare(false);
    SPLAY = program.getFunctionDefinitions().get(FQN);
    SPLAY.getBody().analyzeSizes(SIZE_ANALYSIS);

    try (FileOutputStream outputStream = new FileOutputStream("out/splay-unshared-eager.ml")) {
      SPLAY.printTo(new PrintStream(outputStream));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    // NOTE: e1 is only defined as a helper to get to e2 and subsequently to e3.
    E1 =
        (IfThenElseExpression)
            ((ShareExpression)
                    ((ShareExpression) ((MatchExpression) SPLAY.getBody()).getNode()).getScope())
                .getScope();

    // NOTE: e2 is only defined as a helper to get to e3.
    E2 =
        (IfThenElseExpression)
            ((ShareExpression) ((ShareExpression) (E1.getFalsy())).getScope()).getScope();

    E3 =
        (IfThenElseExpression)
            ((ShareExpression)
                    ((ShareExpression)
                            ((ShareExpression)
                                    ((MatchExpression)
                                            ((ShareExpression) (E2.getTruthy())).getScope())
                                        .getNode())
                                .getScope())
                        .getScope())
                .getScope();

    INTERMEDIATE =
        (IfThenElseExpression)
            ((ShareExpression)
                    ((ShareExpression) ((ShareExpression) E3.getFalsy()).getScope()).getScope())
                .getScope();

    E4 =
        (LetExpression)
            ((LetExpression)
                    ((MatchExpression)
                            ((ShareExpression)
                                    ((ShareExpression) INTERMEDIATE.getTruthy()).getScope())
                                .getScope())
                        .getNode())
                .getBody();

    Tp = (LetExpression) ((MatchExpression) (((E4))).getBody()).getNode();
  }

  @Test
  public void zigzigAbove() throws IOException, ConstraintSystemException {
    final var globals =
        new AnnotatingGlobals(
            Map.of(
                FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Q, Qp /*HEURISTIC.generate("Qpunknown", 1)*/),
                    Set.of(
                        new FunctionAnnotation(P, Pp),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            SIZE_ANALYSIS,
            HEURISTIC);

    // Originally, the idea was of course to start proving from e3, but in the implementation,
    // the context of e3 is much bigger than in the paper, so instead, we start from e4.

    // In the context of e4 there is a sharing of br, a sharing of cr (these are just like in the
    // paper), and
    // a derived variable instead of bl, because we have matched on bl and reconstructed it from
    // (bll, blx, blr).
    List<Identifier> contextIds;
    List<Identifier> varsInContextForE4 = new ArrayList<>(((Expression) E4).freeVariables());
    varsInContextForE4.sort((a, b) -> a.getName().compareToIgnoreCase(b.getName()));
    Collections.reverse(varsInContextForE4);
    assertEquals(3, varsInContextForE4.size());
    assertTrue(varsInContextForE4.get(1).getName().startsWith("cr"));
    assertTrue(varsInContextForE4.get(2).getName().startsWith("br"));
    contextIds =
        List.of(varsInContextForE4.get(1), varsInContextForE4.get(0), varsInContextForE4.get(2));

    final var rootObligation =
        new Obligation(
            new AnnotatingContext(contextIds, Q3),
            E4,
            globals.getSignature(FQN).withCost().to(),
            1);

    final Prover prover = new Prover("splay-above", globals, Paths.get("out"));

    final var e4result = prover.apply(rootObligation, LetTreeCfSimple.INSTANCE);

    final var q4 = e4result.get(1).getContext();
    prover.prove(e4result);

    prover.plot();

    final var constraints = prover.getAccumulatedConstraints();
    final var solution = ConstraintSystemSolver.solve(constraints, "splay");
    try {
      Constraint.plot("splay-above", constraints, Path.of("out"));
    } catch (Exception e) {
      e.printStackTrace();
    }

    assertTrue(solution.isPresent());
    assertContextEqualsByPrefixes(List.of("cr", "br", "z"), Q4, q4.substitute(solution.get()));
  }

  @Test
  public void zigzigBelow() throws ConstraintSystemException {
    final var globals =
        new AnnotatingGlobals(
            // TODO(lorenz.leutgeb): Maybe don't even initialize, since we do not expect to handle
            // (app)?
            Map.of(
                FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Q, Qp),
                    Set.of(
                        new FunctionAnnotation(P, Pp),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            SIZE_ANALYSIS,
            HEURISTIC);

    final Predicate<Obligation> expressionEqualsE3 = x -> E3.equals(x.getExpression());
    final Predicate<Obligation> expressionEqualsE4 = x -> E4.equals(x.getExpression());
    final Predicate<Obligation> expressionEqualsE1Truthy =
        x -> E1.getTruthy().equals(x.getExpression());
    final Predicate<Obligation> expressionEqualsE2Falsy =
        x -> E2.getFalsy().equals(x.getExpression());
    final Predicate<Obligation> expressionEqualsIfAltBFalsy =
        x -> INTERMEDIATE.getFalsy().equals(x.getExpression());
    final Predicate<Obligation> outOfScope =
        expressionEqualsE2Falsy.or(expressionEqualsIfAltBFalsy).or(expressionEqualsE1Truthy);

    final Prover prover = new Prover("splay-below", globals, Paths.get("out"));
    prover.setWeakenAggressively(true);
    final var rootObligation =
        new Obligation(
            new AnnotatingContext(
                SPLAY.treeLikeArguments(), globals.getSignature(FQN).withCost().from()),
            SPLAY.getBody(),
            globals.getSignature(FQN).withCost().to(),
            1);
    final var first = prover.proveUntil(rootObligation, expressionEqualsE3.or(outOfScope));
    assertEquals(3, first.size());
    assertEquals(E3, first.get(1).getExpression());

    final var constraints = prover.getAccumulatedConstraints();
    final var solution = ConstraintSystemSolver.solve(constraints, "splay-below");
    assertTrue(solution.isPresent());
    final var assertions = new ArrayList<Executable>();
    for (Obligation o : prover.getProof()) {
      if (o.getExpression() == null) {
        continue;
      }
      if (o.getExpression().equals(E1)) {
        assertions.add(
            () ->
                // Does not work because br cl br are shared.
                assertContextEqualsByPrefixes(
                    List.of("cl", "cr"), Q1, o.getContext().substitute(solution.get())));
      } else if (o.getExpression().equals(E3)) {
        assertions.add(
            () ->
                // Does not work because br cr bl are shared.
                assertContextEqualsByPrefixes(
                    List.of("cr", "bl", "br"), Q2, o.getContext().substitute(solution.get())));
      }
    }
    assertAll(assertions);
  }

  @Test
  public void costFree() throws IOException, ConstraintSystemException {
    final var globals =
        new AnnotatingGlobals(
            Map.of(
                FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Q, Qp),
                    Set.of(
                        new FunctionAnnotation(P, Pp),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            SIZE_ANALYSIS,
            HEURISTIC);

    final var rootObligation =
        new Obligation(new AnnotatingContext(SPLAY.treeLikeArguments(), P), SPLAY.getBody(), Pp, 0);

    final Prover prover = new Prover("splay-cf", globals, Paths.get("out"));
    prover.prove(rootObligation);

    prover.plot();
    final var constraints = prover.getAccumulatedConstraints();
    final var solution = ConstraintSystemSolver.solve(constraints, "splay-cf");
    assertTrue(solution.isPresent());

    final var assertions = new ArrayList<Executable>();
    for (Obligation o : prover.getProof()) {
      if (o.getExpression() == null) {
        continue;
      }
      if (o.getExpression().equals(E1)) {
        assertions.add(
            () -> {
              /*
              In the lazy case we can do:

                assertContextEqualsByIdSubstrings(
                    List.of("cl", "cr"), P1, o.getContext().substitute(solution.get()))

              However, in the eager case there will be two shared variables for cl and two shared variables
              for cr.
              */
            });
      } else if (o.getExpression().equals(E3)) {
        assertions.add(
            () -> {
              /*
              In the lazy case we can do:

                assertContextEqualsByIdSubstrings(
                    List.of("cr", "bl", "br"), P2, o.getContext().substitute(solution.get()))

              However, in the eager case there will be two shared variables for cl and two shared variables
              for br and two shared variables for bl.
               */
            });
      } else if (o.getExpression().equals(E4)) {
        assertions.add(
            () ->
                assertContextEqualsByPrefixes(
                    List.of("cr", "z", "br"), P2, o.getContext().substitute(solution.get())));
      } else if (o.getExpression().equals(Tp)) {
        assertions.add(
            () ->
                assertContextEqualsByPrefixes(
                    List.of("cr", "br", "al", "ar"),
                    P4,
                    o.getContext().substitute(solution.get())));
      }
    }
    assertAll(assertions);
  }
}
