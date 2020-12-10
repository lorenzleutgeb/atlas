package xyz.leutgeb.lorenz.lac;

import static java.util.Collections.emptyMap;
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
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTree;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTreeCfSimple;
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
public class S62Lazy extends S62 {
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
  public void zigzigAbovePartial() throws IOException {
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

    final Prover prover = new Prover("splay-above", globals);
    // prover.setWeaken(true);

    final var q4v = AnnotatingContext.reorderByName(E4.freeVariables(), List.of("cr", "z₆", "br"));

    Obligation rootObligation = new Obligation(q4v, Q3, E4, Qp);
    final var e4obligation = rootObligation;

    final var e4result = prover.apply(e4obligation, LetTreeCfSimple.INSTANCE);

    final var q4 = e4result.get(1).getContext();
    /*
    We do need cf at
    via sec lnf
    (let:tree:cf): br, cr, z₆ | letz₆ΔR #12  ⊦₁  let z₅ ≔ SplayTree.splay a z₆ in □ | Q'
    via sec splay

    We do not need cf at
    br, cr, al, ar | match z₅ extend [al, ar] #48  ⊦₁  let z₈ ≔ (br, c, cr) in □ | Q'

    (let:tree:cf): ar, al, z₈ | letcf z₈ ΔR #50  ⊦₁  let z₇ ≔ (ar, b, z₈) in (al, a', z₇) | Q'


    In s61 we apply tree:cf to
    br, cr, ar | lett''ΓP #1  ⊦₁  let t''' ≔ (br, c, cr) in (ar, b, t''') | lettree t'' P' #3
     */
    prover.setWeakenAggressively(true);
    var intermediate1 =
        prover.proveUntil(e4result, o -> o.getExpression() instanceof LetExpression);
    prover.setWeakenAggressively(false);
    var intermediate2 = prover.apply(intermediate1.get(0), LetTree.INSTANCE);
    prover.prove(intermediate2);
    prover.plot();

    final var solution = prover.solve();
    assertTrue(solution.getSolution().isPresent());
    assertAll(
        () -> {
          assertContextEquals(
              q4v, Q3, rootObligation.getContext().substitute(solution.getSolution().get()));

          System.out.println("ok1");
        },
        () -> {
          assertContextEqualsByPrefixes(
              List.of("cr", "z" /* "bl" */, "br"),
              Q3,
              StreamSupport.stream(prover.getProof().spliterator(), false)
                  .filter(
                      obligation ->
                          ((LetExpression) ((MatchExpression) INTERMEDIATE.getTruthy()).getNode())
                              .getBody()
                              .equals(obligation.getExpression()))
                  .findFirst()
                  .get()
                  .getContext()
                  .substitute(solution.getSolution().get()));
          System.out.println("ok4");
        },
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "z" /* "bl" */, "br"),
                Q3,
                e4obligation.getContext().substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "br", "z" /* "x" */),
                Q4,
                q4.substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""),
                P1110,
                e4result.get(2).getContext().substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""),
                P1110,
                e4result.get(3).getContext().substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""),
                P1110,
                e4result.get(4).getContext().substitute(solution.getSolution().get())));
  }

  @Test
  public void zigzigAbove() throws IOException {
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

    final Prover prover = new Prover("splay-above", globals);
    // prover.setWeaken(true);

    final var q3v = AnnotatingContext.reorderByName(E3.freeVariables(), List.of("cr", "bl", "br"));

    // To guide the prover to the zigzig case.
    final Predicate<Obligation> isE4 = obligation -> E4.equals(obligation.getExpression());
    final Predicate<Obligation> isE3Truthy =
        obligation -> E3.getTruthy().equals(obligation.getExpression());
    final Predicate<Obligation> isIntermediateFalsy =
        obligation -> INTERMEDIATE.getFalsy().equals(obligation.getExpression());
    final Predicate<Obligation> isIntermediateTruthyLeaf =
        obligation ->
            ((MatchExpression) INTERMEDIATE.getTruthy())
                .getLeaf()
                .equals(obligation.getExpression());

    Obligation rootObligation = new Obligation(q3v, Q3, E3, Qp);
    final var remainingObligations =
        prover.proveUntil(
            rootObligation,
            isE4.or(isE3Truthy).or(isIntermediateFalsy).or(isIntermediateTruthyLeaf));
    assertEquals(4, remainingObligations.size());
    assertEquals(E4, remainingObligations.get(2).getExpression());
    final var e4obligation = remainingObligations.get(2);

    final var e4result = prover.apply(e4obligation, LetTreeCfSimple.INSTANCE);

    final var q4 = e4result.get(1).getContext();
    /*
    We do need cf at
    via sec lnf
    (let:tree:cf): br, cr, z₆ | letz₆ΔR #12  ⊦₁  let z₅ ≔ SplayTree.splay a z₆ in □ | Q'
    via sec splay

    We do not need cf at
    br, cr, al, ar | match z₅ extend [al, ar] #48  ⊦₁  let z₈ ≔ (br, c, cr) in □ | Q'

    (let:tree:cf): ar, al, z₈ | letcf z₈ ΔR #50  ⊦₁  let z₇ ≔ (ar, b, z₈) in (al, a', z₇) | Q'


    In s61 we apply tree:cf to
    br, cr, ar | lett''ΓP #1  ⊦₁  let t''' ≔ (br, c, cr) in (ar, b, t''') | lettree t'' P' #3
     */
    prover.setWeakenAggressively(true);
    var intermediate1 =
        prover.proveUntil(e4result, o -> o.getExpression() instanceof LetExpression);
    prover.setWeakenAggressively(false);
    var intermediate2 = prover.apply(intermediate1.get(0), LetTree.INSTANCE);
    prover.prove(intermediate2);
    prover.plot();

    final var solution = prover.solve();
    assertTrue(solution.getSolution().isPresent());
    assertAll(
        () -> {
          assertContextEquals(
              q3v, Q3, rootObligation.getContext().substitute(solution.getSolution().get()));

          System.out.println("ok1");
        },
        () -> {
          assertContextEquals(
              q3v,
              Q3,
              StreamSupport.stream(prover.getProof().spliterator(), false)
                  .filter(obligation -> E3.getFalsy().equals(obligation.getExpression()))
                  .findFirst()
                  .get()
                  .getContext()
                  .substitute(solution.getSolution().get()));
          System.out.println("ok2");
        },
        () -> {
          assertContextEquals(
              q3v,
              Q3,
              StreamSupport.stream(prover.getProof().spliterator(), false)
                  .filter(obligation -> INTERMEDIATE.getTruthy().equals(obligation.getExpression()))
                  .findFirst()
                  .get()
                  .getContext()
                  .substitute(solution.getSolution().get()));
          System.out.println("ok3");
        },
        () -> {
          assertContextEqualsByPrefixes(
              List.of("cr", "z" /* "bl" */, "br"),
              Q3,
              StreamSupport.stream(prover.getProof().spliterator(), false)
                  .filter(
                      obligation ->
                          ((LetExpression) ((MatchExpression) INTERMEDIATE.getTruthy()).getNode())
                              .getBody()
                              .equals(obligation.getExpression()))
                  .findFirst()
                  .get()
                  .getContext()
                  .substitute(solution.getSolution().get()));
          System.out.println("ok4");
        },
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "z" /* "bl" */, "br"),
                Q3,
                e4obligation.getContext().substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "br", "z" /* "x" */),
                Q4,
                q4.substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""),
                P1110,
                e4result.get(2).getContext().substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""),
                P1110,
                e4result.get(3).getContext().substitute(solution.getSolution().get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of(""),
                P1110,
                e4result.get(4).getContext().substitute(solution.getSolution().get())));
  }

  @Test
  public void zigzigBelow() throws IOException {
    final var globals =
        new AnnotatingGlobals(
            // We can use empty maps here, since we will never apply (app).
            emptyMap(), SIZE_ANALYSIS, HEURISTIC);

    final Predicate<Obligation> isE3 = x -> E3.equals(x.getExpression());
    final Predicate<Obligation> isE3symm = x -> E3symm.equals(x.getExpression());
    final Predicate<Obligation> boundary = isE3.or(isE3symm);

    // Instantiate prover and enable weakening.
    final Prover prover = new Prover("splay-below", globals);
    prover.setWeakenAggressively(true);

    // Partial proof up to the boundary.
    final var remainingObligations =
        prover.proveUntil(
            new Obligation(SPLAY.treeLikeArguments(), Q, SPLAY.getBody(), Qp, 1), boundary);
    assertEquals(2, remainingObligations.size());
    assertEquals(E3, remainingObligations.get(0).getExpression());
    assertEquals(E3symm, remainingObligations.get(1).getExpression());

    // Solve, and assert that there is a solution.
    prover.plot();
    final var solution = prover.solve();
    assertTrue(solution.getSolution().isPresent());

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
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E3)) {
                    return (Executable)
                        () ->
                            assertContextEquals(
                                AnnotatingContext.reorderByName(
                                    o.getExpression().freeVariables(), List.of("cr", "bl", "br")),
                                Q2,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  return null;
                })
            .filter(Objects::nonNull));
  }

  @Test
  public void costFree() {
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

    final Prover prover = new Prover("splay-cf", globals);
    prover.prove(
        new Obligation(
            SPLAY.treeLikeArguments(), P, SPLAY.getBody(), HEURISTIC.generate("unknown P'", 1), 0));

    final var solution = prover.solve();
    assertTrue(solution.getSolution().isPresent());

    assertAll(
        StreamSupport.stream(prover.getProof().spliterator(), false)
            .filter(o -> Objects.nonNull(o.getExpression()))
            .map(
                o -> {
                  if (o.getExpression().equals(SPLAY.getBody())) {
                    return (Executable)
                        () -> {
                          assertContextEqualsByPrefixes(
                              List.of("t"),
                              P,
                              o.getContext().substitute(solution.getSolution().get()));
                          assertAnnotationEquals(
                              Pp, o.getAnnotation().substitute(solution.getSolution().get()));
                        };
                  }
                  if (o.getExpression().equals(E1)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cl", "cr"),
                                P1,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E3)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "bl", "br"),
                                P2,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E4)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "z", "br"),
                                P2,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E5)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "z"),
                                P3,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(Tp)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "al", "ar"),
                                P4,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  return null;
                })
            .filter(Objects::nonNull));
  }

  @Test
  public void zigzigCostFreeAbovePartial() throws IOException {
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

    final Prover prover = new Prover("splay-above", globals);
    // prover.setWeaken(true);

    final var q4v = AnnotatingContext.reorderByName(E4.freeVariables(), List.of("cr", "z₆", "br"));

    Obligation rootObligation = new Obligation(q4v, P2, E4, Pp);
    prover.prove(rootObligation);
    prover.plot();

    final var solution = prover.solve();
    assertTrue(solution.getSolution().isPresent());

    assertAll(
        StreamSupport.stream(prover.getProof().spliterator(), false)
            .filter(o -> Objects.nonNull(o.getExpression()))
            .map(
                o -> {
                  if (o.getExpression().equals(SPLAY.getBody())) {
                    return (Executable)
                        () -> {
                          assertContextEqualsByPrefixes(
                              List.of("t"),
                              P,
                              o.getContext().substitute(solution.getSolution().get()));
                          assertAnnotationEquals(
                              Pp, o.getAnnotation().substitute(solution.getSolution().get()));
                        };
                  }
                  if (o.getExpression().equals(E1)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cl", "cr"),
                                P1,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E3)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "bl", "br"),
                                P2,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E4)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "z", "br"),
                                P2,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(E5)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "z"),
                                P3,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  if (o.getExpression().equals(Tp)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "al", "ar"),
                                P4,
                                o.getContext().substitute(solution.getSolution().get()));
                  }
                  return null;
                })
            .filter(Objects::nonNull));
  }
}
