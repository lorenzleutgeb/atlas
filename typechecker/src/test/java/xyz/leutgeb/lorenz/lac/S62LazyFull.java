package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.Assertions.assertAnnotationEquals;
import static xyz.leutgeb.lorenz.lac.Assertions.assertContextEquals;
import static xyz.leutgeb.lorenz.lac.Assertions.assertContextEqualsByPrefixes;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.StreamSupport;
import org.jgrapht.graph.DirectedMultigraph;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTreeCfSimple;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
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
public class S62LazyFull extends S62 {
  private static final FunctionDefinition SPLAY;

  private static final IfThenElseExpression E1;
  private static final IfThenElseExpression E2;
  private static final IfThenElseExpression E3;
  private static final IfThenElseExpression E3symm;
  private static final IfThenElseExpression INTERMEDIATE;
  private static final LetExpression E4;
  private static final MatchExpression E5;
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
    program.unshare(true);

    SPLAY = program.getFunctionDefinitions().get(FQN);
    SPLAY.getBody().analyzeSizes(SIZE_ANALYSIS);

    try (FileOutputStream outputStream = new FileOutputStream("out/splay-unshared-lazy.ml")) {
      SPLAY.printTo(new PrintStream(outputStream));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    E1 = (IfThenElseExpression) ((((MatchExpression) SPLAY.getBody()).getNode()));
    E2 = (IfThenElseExpression) E1.getFalsy();
    E3 = ((IfThenElseExpression) ((MatchExpression) E2.getTruthy()).getNode());
    E3symm = ((IfThenElseExpression) ((MatchExpression) E2.getFalsy()).getNode());
    INTERMEDIATE = (IfThenElseExpression) E3.getFalsy();
    E4 =
        ((LetExpression)
            ((LetExpression) ((MatchExpression) INTERMEDIATE.getTruthy()).getNode()).getBody());
    E5 = (MatchExpression) E4.getBody();
    Tp = (LetExpression) E5.getNode();
  }

  @Test
  public void full() {
    final var globals =
        new AnnotatingGlobals(
            // We need to populate the signature, since we'll apply (app), like in the "above" test.
            Map.of(FQN, new FunctionAnnotation(Q, Qp)),
            Map.of(
                FQN,
                Set.of(
                    new FunctionAnnotation(P, Pp),
                    new FunctionAnnotation(
                        Annotation.zero(1, "cfargszero"), Annotation.zero(1, "cfreturnzero")))),
            SIZE_ANALYSIS,
            HEURISTIC);

    final Predicate<Obligation> isE3 = x -> E3.equals(x.getExpression());
    final Predicate<Obligation> isE3symm = x -> E3symm.equals(x.getExpression());
    final Predicate<Obligation> boundary = isE3.or(isE3symm);

    // Instantiate prover and enable weakening.
    final Prover prover = new Prover("splay-full", globals);
    prover.setWeakenBeforeTerminal(true);

    // Partial proof up to the boundary.
    var remainingObligations =
        prover.proveUntil(
            new Obligation(SPLAY.treeLikeArguments(), Q, SPLAY.getBody(), Qp, 1), boundary);
    assertEquals(2, remainingObligations.size());
    assertEquals(E3, remainingObligations.get(0).getExpression());

    // This case will not be shown.
    assertEquals(E3symm, remainingObligations.get(1).getExpression());

    // Apply (w).
    final var weakeningResult = prover.apply(remainingObligations.get(0), W.INSTANCE);

    // Previous root obligation from "above", now checked to
    // somehow match up with the result of weakening
    //
    //     new Obligation(List.of("cr", "bl", "br"), Q3, E3, Qp)

    assertEquals(1, weakeningResult.size());
    assertEquals(E3, weakeningResult.get(0).getExpression());

    remainingObligations =
        prover.proveUntil(
            weakeningResult.get(0), obligation -> E4.equals(obligation.getExpression()));

    assertEquals(1, remainingObligations.size());
    assertEquals(E4, remainingObligations.get(0).getExpression());

    final var e4result = prover.apply(remainingObligations.get(0), LetTreeCfSimple.INSTANCE);

    final var q4 = e4result.get(1).getContext();
    prover.prove(e4result);
    // prover.plot();

    final var solution = prover.solve();
    assertTrue(solution.isPresent());

    assertAll(
        StreamSupport.stream(prover.getProof().spliterator(), false)
            .filter(o -> Objects.nonNull(o.getExpression()))
            .map(
                o -> {
                  if (o.getExpression().equals(E1)) {
                    return (Executable)
                        () ->
                            assertContextEquals(
                                AnnotatingContext.reorderByName(
                                    o.getExpression().freeVariables(), List.of("cl", "cr")),
                                Q1,
                                o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(E3)) {
                    return (Executable)
                        () ->
                            assertContextEquals(
                                AnnotatingContext.reorderByName(
                                    o.getExpression().freeVariables(), List.of("cr", "bl", "br")),
                                Q2,
                                o.getContext().substitute(solution.get()));
                  }
                  return null;
                })
            .filter(Objects::nonNull));
    assertAll(
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "br", "∂"), Q4, q4.substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""), P1110, e4result.get(2).getContext().substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""), P1110, e4result.get(3).getContext().substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""), P1110, e4result.get(4).getContext().substitute(solution.get())));
  }

  @Test
  public void costFree() {
    final var globals =
        new AnnotatingGlobals(
            Map.of(FQN, new FunctionAnnotation(Q, Qp)),
            Map.of(
                FQN,
                Set.of(
                    new FunctionAnnotation(P, Pp),
                    new FunctionAnnotation(
                        Annotation.zero(1, "cfargszero"), Annotation.zero(1, "cfreturnzero")))),
            SIZE_ANALYSIS,
            HEURISTIC);

    final Prover prover = new Prover("splay-cf", globals);
    prover.prove(
        new Obligation(
            SPLAY.treeLikeArguments(), P, SPLAY.getBody(), HEURISTIC.generate("unknown P'", 1), 0));

    final var solution = prover.solve();
    assertTrue(solution.isPresent());

    assertAll(
        StreamSupport.stream(prover.getProof().spliterator(), false)
            .filter(o -> Objects.nonNull(o.getExpression()))
            .map(
                o -> {
                  if (o.getExpression().equals(SPLAY.getBody())) {
                    return (Executable)
                        () -> {
                          assertContextEqualsByPrefixes(
                              List.of("t"), P, o.getContext().substitute(solution.get()));
                          assertAnnotationEquals(Pp, o.getAnnotation().substitute(solution.get()));
                        };
                  }
                  if (o.getExpression().equals(E1)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cl", "cr"), P1, o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(E3)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "bl", "br"),
                                P2,
                                o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(E4)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "∂", "br"),
                                P2,
                                o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(E5)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "∂"),
                                P3,
                                o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(Tp)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "al", "ar"),
                                P4,
                                o.getContext().substitute(solution.get()));
                  }
                  return null;
                })
            .filter(Objects::nonNull));
  }
}
