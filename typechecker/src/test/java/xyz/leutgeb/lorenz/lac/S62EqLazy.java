package xyz.leutgeb.lorenz.lac;

import static com.google.common.collect.Sets.union;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.Assertions.assertAnnotationEquals;
import static xyz.leutgeb.lorenz.lac.Assertions.assertContextEquals;
import static xyz.leutgeb.lorenz.lac.Assertions.assertContextEqualsByPrefixes;
import static xyz.leutgeb.lorenz.lac.TestUtil.fromProver;
import static xyz.leutgeb.lorenz.lac.TestUtil.printTable;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint.eq;
import static xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint.eqSoft;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import xyz.leutgeb.lorenz.lac.antlr.TacticLexer;
import xyz.leutgeb.lorenz.lac.antlr.TacticParser;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.TacticVisitorImpl;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Ite;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTree;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTreeCfSimple;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

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
public class S62EqLazy extends S62 {
  private static final String SPLAY_FQN = "SplayTree.splay_eq";
  private static final String INSERT_FQN = "SplayTree.insert_eq";
  private static final String MAX_FQN = "SplayTree.splay_max_eq";
  private static final String CONTAINS_FQN = "SplayTree.contains_eq";
  private static final String LINK_FQN = "PairingHeap.link";
  private static final String MERGE_FQN = "PairingHeap.merge";

  private static final FunctionDefinition SPLAY;

  private static final IfThenElseExpression E1;
  private static final IfThenElseExpression E2;
  private static final IfThenElseExpression E3;
  private static final IfThenElseExpression INTERMEDIATE;
  private static final LetExpression E4;
  private static final LetExpression E4symm;
  private static final MatchExpression E5;
  private static final LetExpression Tp;

  private static final Program PROGRAM;

  static {
    final var loader = Tests.loader();
    try {
      PROGRAM =
          loader.load(Set.of(SPLAY_FQN, INSERT_FQN, MAX_FQN, CONTAINS_FQN, LINK_FQN, MERGE_FQN));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    PROGRAM.normalize();
    try {
      PROGRAM.infer();
    } catch (UnificationError | TypeError e) {
      throw new RuntimeException(e);
    }
    PROGRAM.unshare(true);
    PROGRAM.analyzeSizes();

    SPLAY = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);

    E1 = (IfThenElseExpression) ((((MatchExpression) SPLAY.getBody()).getNode()));
    E2 = (IfThenElseExpression) E1.getFalsy();
    E3 = ((IfThenElseExpression) ((MatchExpression) E2.getTruthy()).getNode());
    INTERMEDIATE = (IfThenElseExpression) E3.getFalsy();
    E4 = (LetExpression) ((IfThenElseExpression) INTERMEDIATE.getTruthy()).getFalsy();
    E4symm = (LetExpression) ((IfThenElseExpression) INTERMEDIATE.getFalsy()).getFalsy();
    E5 = (MatchExpression) E4.getBody();
    Tp = (LetExpression) E5.getNode();
  }

  @Test
  public void zigzigAbove() throws IOException {
    // For experimentation:
    // final var Qvar = HEURISTIC.generate("Qvar", 1);
    // final var Qvar = Annotation.zero(1, "Qzero");
    final var Qvar = Q;
    // final var Qpvar = HEURISTIC.generate("Qpvar", 1);
    // final var Qpvar = Annotation.zero(1, "Qpzero");
    final var Qpvar = Qp;
    // final var Q3var = HEURISTIC.generate("Q3var", 3);
    // final var Q3var = Annotation.zero(3, "Q3var");
    final var Q3var = Q3;

    final Coefficient Qvarsum = new UnknownCoefficient("Qsum");
    final Coefficient Qpvarsum = new UnknownCoefficient("Qpsum");
    final var sumConstraints = new HashSet<Constraint>();
    sumConstraints.add(Qvar.sumAllCoefficients(Qvarsum));
    sumConstraints.add(Qpvar.sumAllCoefficients(Qpvarsum));
    final var target = new UnknownCoefficient("target");
    sumConstraints.add(new EqualsSumConstraint(target, List.of(Qvarsum, Qpvarsum), "sum target"));

    final var fd = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);

    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Qvar, Qpvar),
                    Set.of(
                        new FunctionAnnotation(P, Pp),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            fd.getSizeAnalysis(),
            HEURISTIC);

    final Prover prover = new Prover("splay_eq-above", globals);
    final var q3v = AnnotatingContext.reorderByName(E3.freeVariables(), List.of("cr", "bl", "br"));

    final Predicate<Obligation> isE4 = obligation -> E4.equals(obligation.getExpression());
    final Predicate<Obligation> isE4symm = obligation -> E4symm.equals(obligation.getExpression());
    final Predicate<Obligation> isE3Truthy =
        obligation -> E3.getTruthy().equals(obligation.getExpression());
    final Predicate<Obligation> isIntermediateFalsy =
        obligation -> INTERMEDIATE.getFalsy().equals(obligation.getExpression());

    Obligation rootObligation = new Obligation(q3v, Q3var, E3, Qpvar);
    final var remainingObligations =
        prover.proveUntil(rootObligation, isE4.or(isE3Truthy).or(isE4symm).or(isIntermediateFalsy));
    assertEquals(3, remainingObligations.size());
    assertTrue(isE3Truthy.test(remainingObligations.get(0)));
    assertTrue(isE4.test(remainingObligations.get(1)));
    assertTrue(isIntermediateFalsy.test(remainingObligations.get(2)));

    final var e4obligation = remainingObligations.get(1);
    final var e4result = prover.apply(e4obligation, LetTreeCfSimple.INSTANCE);
    assertEquals(5, e4result.size());
    prover.prove(e4result.get(0));
    prover.prove(e4result.subList(2, e4result.size()));

    var intermediate1 =
        prover.proveUntil(e4result.get(1), o -> o.getExpression() instanceof LetExpression);
    assertEquals(1, intermediate1.size());
    var intermediate2 = prover.apply(prover.weaken(intermediate1.get(0)), LetTree.INSTANCE);

    assertEquals(2, intermediate2.size());
    prover.prove(intermediate2.get(1));
    prover.prove(prover.apply(intermediate2.get(0), LetTreeCfSimple.INSTANCE));
    prover.plot();

    List<Constraint> fixQ4 =
        eq(
            e4result.get(1).getContext().reorderByName(List.of("cr", "br", "z₅")).getAnnotation(),
            Q4,
            "fix Q4");

    final var solution = prover.solve();
    assertTrue(solution.isPresent());
    // Constraint.plot(FQN, prover.getAccumulatedConstraints(), Paths.get("out"));
    final var minimizedSolution = prover.solve(sumConstraints, singletonList(target));

    System.out.println(
        "splay_eq above w/out minimization: "
            + Qvar.substitute(solution.get())
            + " -> "
            + Qpvar.substitute(solution.get())
            + " with Q3 = "
            + Q3var.substitute(solution.get()));
    System.out.println(
        "splay_eq above w/    minimization: "
            + (minimizedSolution
                .map(
                    sol ->
                        Qvar.substitute(sol)
                            + " -> "
                            + Qpvar.substitute(sol)
                            + " with Q3 = "
                            + Q3var.substitute(sol))
                .orElse("UNSAT")));

    assertAll(
        () -> assertContextEquals(q3v, Q3, rootObligation.getContext().substitute(solution.get())),
        () ->
            assertContextEquals(
                q3v,
                Q3,
                fromProver(prover, obligation -> E3.getFalsy().equals(obligation.getExpression()))
                    .substitute(solution.get())),
        () ->
            assertContextEquals(
                q3v,
                Q3,
                fromProver(
                        prover,
                        obligation -> INTERMEDIATE.getTruthy().equals(obligation.getExpression()))
                    .substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "bl", "br"),
                Q3,
                e4obligation.getContext().substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                List.of("cr", "br", "z"),
                Q4,
                e4result.get(1).getContext().substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                singletonList(""),
                Annotation.zero(1),
                e4result.get(2).getContext().substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                singletonList(""), P1110, e4result.get(3).getContext().substitute(solution.get())),
        () ->
            assertContextEqualsByPrefixes(
                singletonList(""),
                Annotation.zero(1),
                e4result.get(4).getContext().substitute(solution.get())));
  }

  @Test
  public void zigzigBelow() throws IOException {
    final var globals =
        new AnnotatingGlobals(
            // We can use empty maps here, since we will never apply (app).
            emptyMap(),
            PROGRAM.getFunctionDefinitions().get(SPLAY_FQN).getSizeAnalysis(),
            HEURISTIC);

    // final var Qvar = Annotation.zero(1, "Q");
    final var Qvar = HEURISTIC.generate("Q", 1);
    // final var Qvar = Q;

    // final var Qpvar = Annotation.zero(1, "Q'");
    final var Qpvar = HEURISTIC.generate("Q'", 1);
    // final var Qpvar = Qp;

    final Coefficient Qvarsum = new UnknownCoefficient("Qsum");
    final Coefficient Qpvarsum = new UnknownCoefficient("Q'sum");
    final var sumConstraints = new HashSet<Constraint>();
    sumConstraints.add(Qvar.sumAllCoefficients(Qvarsum));
    sumConstraints.add(Qpvar.sumAllCoefficients(Qpvarsum));
    final var target = new UnknownCoefficient("target");
    sumConstraints.add(new EqualsSumConstraint(target, List.of(Qvarsum, Qpvarsum), "sum target"));

    final Predicate<Obligation> isLeaf =
        x -> ((MatchExpression) SPLAY.getBody()).getLeaf().equals(x.getExpression());
    final Predicate<Obligation> isE1Truthy = x -> E1.getTruthy().equals(x.getExpression());
    final Predicate<Obligation> isE2Falsy = x -> E2.getFalsy().equals(x.getExpression());
    final Predicate<Obligation> isE2TruthyLeaf =
        x -> ((MatchExpression) E2.getTruthy()).getLeaf().equals(x.getExpression());
    final Predicate<Obligation> isE3 = x -> E3.equals(x.getExpression());
    final Predicate<Obligation> boundary =
        isLeaf.or(isE1Truthy).or(isE2Falsy).or(isE2TruthyLeaf).or(isE3);

    final Prover prover = new Prover("splay-below", globals);

    // Partial proof up to the boundary.
    final var remainingObligations =
        prover.proveUntil(
            new Obligation(SPLAY.treeLikeArguments(), Qvar, SPLAY.getBody(), Qpvar, 1), boundary);
    assertEquals(5, remainingObligations.size());
    assertEquals(E3, remainingObligations.get(3).getExpression());

    final var fixQ2 =
        Set.copyOf(
            eq(
                remainingObligations
                    .get(3)
                    .getContext()
                    .reorderByName("cr", "bl", "br")
                    .getAnnotation(),
                Q2,
                "fix Q2"));

    // Solve, and assert that there is a solution.
    prover.plot();
    final var solution = prover.solve(fixQ2);
    assertTrue(solution.isPresent());
    final var minimizedSolution = prover.solve(union(sumConstraints, fixQ2), singletonList(target));

    System.out.println(
        "splay_eq w/out minimization: "
            + Qvar.substitute(solution.get())
            + " -> "
            + Qpvar.substitute(solution.get()));
    System.out.println(
        "splay_eq w/    minimization: "
            + (minimizedSolution
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));

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
  }

  @Test
  public void costFree() {
    // final var Pvar = HEURISTIC.generate("P", 1);
    final var Pvar = P;
    // final var Ppvar = HEURISTIC.generate("P'", 1);
    final var Ppvar = Pp;

    final var fd = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);

    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Q, Qp),
                    Set.of(
                        new FunctionAnnotation(Pvar, Ppvar),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            fd.getSizeAnalysis(),
            HEURISTIC);

    final Prover prover = new Prover("splay-cf", globals);
    prover.setWeakenAggressively(true);
    prover.prove(new Obligation(SPLAY.treeLikeArguments(), Pvar, SPLAY.getBody(), Ppvar, 0));

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
                                List.of("cr", "bl", "br"),
                                P2,
                                o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(E5)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "z"),
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

  @Test
  public void zigzigCostFreeAbove() throws IOException {
    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Q, Qp),
                    Set.of(
                        new FunctionAnnotation(P, Pp),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            PROGRAM.getFunctionDefinitions().get(SPLAY_FQN).getSizeAnalysis(),
            HEURISTIC);

    final Prover prover = new Prover("splay-eq-cf-above", globals);

    final var q4v = AnnotatingContext.reorderByName(E3.freeVariables(), List.of("cr", "bl", "br"));

    Obligation rootObligation = new Obligation(q4v, P2, E3, Pp, 0);
    prover.prove(rootObligation);
    prover.plot();

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
                                List.of("cr", "bl", "br"),
                                P2,
                                o.getContext().substitute(solution.get()));
                  }
                  if (o.getExpression().equals(E5)) {
                    return (Executable)
                        () ->
                            assertContextEqualsByPrefixes(
                                List.of("cr", "br", "z"),
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

  @Test
  public void zigzig() throws IOException {
    final var Qvar = HEURISTIC.generate("Q", 1);
    // final var Qvar = Annotation.zero(1, "Qzero");
    // final var Qvar = Q;

    final var Qpvar = HEURISTIC.generate("Q'", 1);
    // final var Qpvar = Annotation.zero(1, "Q'zero");
    // final var Qpvar = Qp;

    // final var Pvar = HEURISTIC.generate("P", 1);
    final var Pvar = P;

    // final var Ppvar = HEURISTIC.generate("P'", 1);
    final var Ppvar = Pp;

    final var qRankCoefficientSum = new UnknownCoefficient("Qranksum");
    final var qpRankCoefficientSum = new UnknownCoefficient("Qpranksum");
    final var qCoefficientSum = new UnknownCoefficient("Qothersum");
    final var qpCoefficientSum = new UnknownCoefficient("Qpothersum");
    final var rankCoefficientSum = new UnknownCoefficient("rankCoefficientSum");
    final var coefficientSum = new UnknownCoefficient("coefficientSum");

    final var sumConstraints =
        Set.<Constraint>of(
            Qvar.sumRankCoefficients(qRankCoefficientSum),
            Qpvar.sumRankCoefficients(qpRankCoefficientSum),
            Qvar.sumCoefficients(qCoefficientSum),
            Qpvar.sumCoefficients(qpCoefficientSum),
            new EqualsSumConstraint(
                rankCoefficientSum,
                List.of(qRankCoefficientSum, qpRankCoefficientSum),
                "(opt) rank coefficient sum"),
            new EqualsSumConstraint(
                coefficientSum,
                List.of(qCoefficientSum, qpCoefficientSum),
                "(opt) coefficient sum"));

    final Set<Constraint> haveRank =
        singleton(
            new LessThanOrEqualConstraint(
                ONE, Qpvar.getRankCoefficient(), "have non-zero Q' rank"));

    final var fd = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);
    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Qvar, Qpvar),
                    Set.of(
                        new FunctionAnnotation(Pvar, Ppvar),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            fd.getSizeAnalysis(),
            HEURISTIC);

    final Predicate<Obligation> isLeaf =
        x -> ((MatchExpression) SPLAY.getBody()).getLeaf().equals(x.getExpression());
    final Predicate<Obligation> isE1Truthy = x -> E1.getTruthy().equals(x.getExpression());
    final Predicate<Obligation> isE2Falsy = x -> E2.getFalsy().equals(x.getExpression());
    final Predicate<Obligation> isE2TruthyLeaf =
        x -> ((MatchExpression) E2.getTruthy()).getLeaf().equals(x.getExpression());
    final Predicate<Obligation> isE3 = x -> E3.equals(x.getExpression());
    final Predicate<Obligation> boundary =
        isLeaf.or(isE1Truthy).or(isE2Falsy).or(isE2TruthyLeaf).or(isE3);

    final Prover prover = new Prover("splay", globals);

    // Partial proof up to the boundary.
    final var belowRemainingObligations =
        prover.proveUntil(
            new Obligation(SPLAY.treeLikeArguments(), Qvar, SPLAY.getBody(), Qpvar, 1), boundary);
    assertEquals(5, belowRemainingObligations.size());
    assertEquals(E3, belowRemainingObligations.get(3).getExpression());

    prover.setWeakenAggressively(true);
    prover.prove(belowRemainingObligations.get(0));
    prover.prove(belowRemainingObligations.get(1));
    prover.prove(belowRemainingObligations.get(2));
    prover.setWeakenAggressively(false);

    // TODO(lorenz.leutgeb): Find a more efficient way to prove this branch.
    /*
    prover.setAuto(true);
    prover.prove(belowRemainingObligations.get(4));
    prover.setAuto(false);
     */

    final var q3v = AnnotatingContext.reorderByName(E3.freeVariables(), List.of("cr", "bl", "br"));
    final var q2 = belowRemainingObligations.get(3).getContext().reorder(q3v);

    final Obligation aboveRootObligation = prover.weaken(belowRemainingObligations.get(3));

    final var q3 = aboveRootObligation.getContext().reorder(q3v);
    final Set<Constraint> fixQ3 = Set.copyOf(eqSoft(Q3, q3.getAnnotation(), "fix Q3"));

    final var q4bodyv =
        AnnotatingContext.reorderByName(E4.freeVariables(), List.of("cr", "bl", "br"));

    final Predicate<Obligation> isE4 = obligation -> E4.equals(obligation.getExpression());
    final Predicate<Obligation> isE4symm = obligation -> E4symm.equals(obligation.getExpression());
    final Predicate<Obligation> isE3Truthy =
        obligation -> E3.getTruthy().equals(obligation.getExpression());
    final Predicate<Obligation> isIntermediateFalsy =
        obligation -> INTERMEDIATE.getFalsy().equals(obligation.getExpression());

    final var remainingObligations =
        prover.proveUntil(
            aboveRootObligation, isE4.or(isE3Truthy).or(isE4symm).or(isIntermediateFalsy));
    assertEquals(3, remainingObligations.size());
    assertTrue(isE3Truthy.test(remainingObligations.get(0)));
    assertTrue(isE4.test(remainingObligations.get(1)));
    assertTrue(isIntermediateFalsy.test(remainingObligations.get(2)));

    prover.setWeakenAggressively(true);
    prover.prove(remainingObligations.get(0));
    prover.setWeakenAggressively(false);

    final var e4obligation = remainingObligations.get(1);
    final var e4result = prover.apply(e4obligation, LetTreeCfSimple.INSTANCE);
    assertEquals(5, e4result.size());

    final var q4 = e4result.get(1).getContext().reorderByName("cr", "z₅", "br");

    prover.prove(e4result.get(0));
    prover.prove(e4result.subList(2, e4result.size()));

    var intermediate1 =
        prover.proveUntil(e4result.get(1), o -> o.getExpression() instanceof LetExpression);
    assertEquals(1, intermediate1.size());
    var intermediate2 = prover.apply(intermediate1.get(0), LetTree.INSTANCE);

    assertEquals(2, intermediate2.size());
    prover.prove(intermediate2.get(1));
    prover.prove(prover.apply(intermediate2.get(0), LetTreeCfSimple.INSTANCE));

    // Now the symmetric case:
    final var symmObligation = prover.weaken(remainingObligations.get(2));
    final var symmBranches = prover.apply(symmObligation, Ite.INSTANCE);

    prover.setWeakenAggressively(true);
    prover.prove(symmBranches.get(0));
    prover.setWeakenAggressively(false);

    final var e4symmresult = prover.apply(symmBranches.get(1), LetTreeCfSimple.INSTANCE);
    assertEquals(5, e4symmresult.size());

    prover.prove(e4symmresult.get(0));
    prover.prove(e4symmresult.subList(2, e4symmresult.size()));

    var intermediate3 =
        prover.proveUntil(e4symmresult.get(1), o -> o.getExpression() instanceof LetExpression);

    assertEquals(1, intermediate3.size());
    var intermediate4 = prover.apply(intermediate3.get(0), LetTreeCfSimple.INSTANCE);

    assertEquals(5, intermediate4.size());
    prover.prove(prover.weaken(intermediate4.get(1)));

    prover.setWeakenAggressively(true);
    prover.prove(intermediate4.subList(2, 5));
    prover.prove(intermediate4.get(0));
    prover.setWeakenAggressively(false);

    // We're done!
    prover.plot();
    /*
    Set<Constraint> fixQ4 =
        Set.copyOf(
            eq(
                e4result.get(1).getContext().reorderByName("cr", "br", "z₅").getAnnotation(),
                Q4,
                "fix Q4"));

    Set<Constraint> fixP1 =
        Sets.union(
            Set.copyOf(eq(e4result.get(2).getContext().getAnnotation(), P1110, "fix P1")),
            Sets.union(
                Set.copyOf(eq(e4result.get(3).getContext().getAnnotation(), P1110, "fix P1")),
                Set.copyOf(eq(e4result.get(4).getContext().getAnnotation(), P1110, "fix P1"))));

     */
    Set<Constraint> fixQ2 = new HashSet<>();
    StreamSupport.stream(prover.getProof().spliterator(), false)
        .filter(o -> Objects.nonNull(o.getExpression()))
        .forEach(
            o -> {
              if (o.getExpression().equals(E3)
                  && !o.getContext().getAnnotation().getNameAndId().startsWith("weaken")) {
                fixQ2.addAll(
                    eqSoft(
                        Q2,
                        o.getContext().reorderByName("cr", "bl", "br").getAnnotation(),
                        "fix Q2"));
              }
            });

    // final var fixing = fixQ2;
    final var fixing = Collections.<Constraint>emptySet();
    // final var fixing = Sets.union(fixQ2, fixQ3);
    final var minimization = union(fixing, sumConstraints);
    final var minimizationTargets = List.of(rankCoefficientSum, coefficientSum);

    final var solution = prover.solve(fixing);
    assertTrue(solution.isPresent());
    final var minSolution = prover.solve(minimization, minimizationTargets);
    final var minSolutionWithRank =
        prover.solve(union(minimization, haveRank), minimizationTargets);

    Set<Constraint> softFixQ = Set.copyOf(eqSoft(Q, Qvar, "soft fix Q"));
    final var minSolutionWithRankAndFixedQ =
        prover.solve(union(minimization, union(haveRank, softFixQ)), minimizationTargets);

    System.out.println(
        "splay_eq w/out minimization: "
            + Qvar.substitute(solution.get())
            + " -> "
            + Qpvar.substitute(solution.get()));
    System.out.println(
        "splay_eq w/    minimization: "
            + (minSolution
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        "splay_eq rank  minimization: "
            + (minSolutionWithRank
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        "splay_eq w/ fixed Q        : "
            + (minSolutionWithRankAndFixedQ
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println("splay_eq expected          : " + Q + " -> " + Qp);

    System.out.println(
        printTable(
            List.of(
                q2.substitute(minSolutionWithRank.get()).rename("Q2"),
                q3.substitute(minSolutionWithRank.get()).rename("Q3"),
                q4.substitute(minSolutionWithRank.get()).rename("Q4"),
                e4result.get(2).substitute(minSolutionWithRank.get()).getContext(),
                e4result.get(3).substitute(minSolutionWithRank.get()).getContext(),
                e4result.get(4).substitute(minSolutionWithRank.get()).getContext()),
            List.of(
                Qvar.substitute(minSolutionWithRank.get()).rename("Q"),
                Qpvar.substitute(minSolutionWithRank.get()).rename("Q'"))));

    assertAll(
        union(
            Set.of(
                () ->
                    assertContextEquals(
                        q3v, Q3, aboveRootObligation.getContext().substitute(solution.get())),
                () ->
                    assertContextEquals(
                        q3v,
                        Q3,
                        fromProver(
                                prover,
                                obligation -> E3.getFalsy().equals(obligation.getExpression()))
                            .substitute(solution.get())),
                () ->
                    assertContextEquals(
                        q3v,
                        Q3,
                        fromProver(
                                prover,
                                obligation ->
                                    INTERMEDIATE.getTruthy().equals(obligation.getExpression()))
                            .substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        List.of("cr", "bl", "br"),
                        Q3,
                        e4obligation.getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        List.of("cr", "br", "z"),
                        Q4,
                        e4result.get(1).getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        singletonList(""),
                        P1110,
                        e4result.get(2).getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        singletonList(""),
                        P1110,
                        e4result.get(3).getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        singletonList(""),
                        P1110,
                        e4result.get(4).getContext().substitute(solution.get()))),
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
                      if (o.getExpression().equals(E3)
                          && !o.getContext().getAnnotation().getNameAndId().startsWith("weaken")) {
                        return (Executable)
                            () ->
                                assertContextEquals(
                                    AnnotatingContext.reorderByName(
                                        o.getExpression().freeVariables(),
                                        List.of("cr", "bl", "br")),
                                    Q2,
                                    o.getContext().substitute(solution.get()));
                      }
                      return null;
                    })
                .filter(Objects::nonNull)
                .collect(Collectors.toSet())));
  }

  @Test
  @Disabled
  public void zigzigWithCf() throws IOException {
    // final var Pvar = HEURISTIC.generate("P", 1);
    final var Pvar = P;
    // final var Ppvar = HEURISTIC.generate("P'", 1);
    final var Ppvar = Pp;

    final var Qvar = HEURISTIC.generate("Q", 1);
    // final var Qvar = Annotation.zero(1, "Qzero");
    // final var Qvar = Q;

    final var Qpvar = HEURISTIC.generate("Q'", 1);
    // final var Qpvar = Annotation.zero(1, "Qpzero");
    // final var Qpvar = Qp;

    final var fd = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);

    final var cfGlobals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Qvar, Qpvar),
                    Set.of(
                        new FunctionAnnotation(P, Pp),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            fd.getSizeAnalysis(),
            HEURISTIC);

    final Prover cfProver = new Prover("splay-cf", cfGlobals);
    cfProver.setWeakenAggressively(true);
    cfProver.prove(new Obligation(SPLAY.treeLikeArguments(), Pvar, SPLAY.getBody(), Ppvar, 0));

    final Coefficient qRankCoefficientSum = new UnknownCoefficient("Qranksum");
    final Coefficient qpRankCoefficientSum = new UnknownCoefficient("Qpranksum");

    final Coefficient qCoefficientSum = new UnknownCoefficient("Qothersum");
    final Coefficient qpCoefficientSum = new UnknownCoefficient("Qpothersum");

    final var sumConstraints = new HashSet<Constraint>();
    sumConstraints.add(Qvar.sumRankCoefficients(qRankCoefficientSum));
    sumConstraints.add(Qpvar.sumRankCoefficients(qpRankCoefficientSum));
    sumConstraints.add(Qvar.sumCoefficients(qCoefficientSum));
    sumConstraints.add(Qpvar.sumCoefficients(qpCoefficientSum));
    sumConstraints.add(
        new LessThanOrEqualConstraint(ONE, Qpvar.getRankCoefficient(), "have non-zero Q' rank"));

    final var rankCoefficientSum = new UnknownCoefficient("rankCoefficientSum");
    final var coefficientSum = new UnknownCoefficient("coefficientSum");
    sumConstraints.add(
        new EqualsSumConstraint(
            rankCoefficientSum,
            List.of(qRankCoefficientSum, qpRankCoefficientSum),
            "(opt) rank coefficient sum"));
    sumConstraints.add(
        new EqualsSumConstraint(
            coefficientSum, List.of(qCoefficientSum, qpCoefficientSum), "(opt) coefficient sum"));

    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Qvar, Qpvar),
                    Set.of(
                        new FunctionAnnotation(Pvar, Ppvar),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            fd.getSizeAnalysis(),
            HEURISTIC);

    final Predicate<Obligation> isLeaf =
        x -> ((MatchExpression) SPLAY.getBody()).getLeaf().equals(x.getExpression());
    final Predicate<Obligation> isE1Truthy = x -> E1.getTruthy().equals(x.getExpression());
    final Predicate<Obligation> isE2Falsy = x -> E2.getFalsy().equals(x.getExpression());
    final Predicate<Obligation> isE2TruthyLeaf =
        x -> ((MatchExpression) E2.getTruthy()).getLeaf().equals(x.getExpression());
    final Predicate<Obligation> isE3 = x -> E3.equals(x.getExpression());
    final Predicate<Obligation> boundary =
        isLeaf.or(isE1Truthy).or(isE2Falsy).or(isE2TruthyLeaf).or(isE3);

    final Prover prover = new Prover("splay", globals);

    // Partial proof up to the boundary.
    final var belowRemainingObligations =
        prover.proveUntil(
            new Obligation(SPLAY.treeLikeArguments(), Qvar, SPLAY.getBody(), Qpvar, 1), boundary);
    assertEquals(5, belowRemainingObligations.size());
    assertEquals(E3, belowRemainingObligations.get(3).getExpression());

    // -------------------------

    final var q3v = AnnotatingContext.reorderByName(E3.freeVariables(), List.of("cr", "bl", "br"));
    List<Obligation> above = prover.apply(belowRemainingObligations.get(3), W.INSTANCE);
    assertEquals(1, above.size());
    final var aboveRootObligation = above.get(0);
    final Set<Constraint> fixQ3 =
        //
        // Set.copyOf(W.compareCoefficientsLessOrEqual(above.get(0).getContext().reorder(q3v).getAnnotation(), Q3));
        Set.copyOf(eqSoft(Q3, above.get(0).getContext().reorder(q3v).getAnnotation(), "fix Q3"));
    // final var aboveRootObligation = belowRemainingObligations.get(3);

    // -----------------------

    final var q4bodyv =
        AnnotatingContext.reorderByName(E4.freeVariables(), List.of("cr", "bl", "br"));

    final Predicate<Obligation> isE4 = obligation -> E4.equals(obligation.getExpression());
    final Predicate<Obligation> isE4symm = obligation -> E4symm.equals(obligation.getExpression());
    final Predicate<Obligation> isE3Truthy =
        obligation -> E3.getTruthy().equals(obligation.getExpression());
    final Predicate<Obligation> isIntermediateFalsy =
        obligation -> INTERMEDIATE.getFalsy().equals(obligation.getExpression());

    final var remainingObligations =
        prover.proveUntil(
            aboveRootObligation, isE4.or(isE3Truthy).or(isE4symm).or(isIntermediateFalsy));
    assertEquals(3, remainingObligations.size());
    assertTrue(isE3Truthy.test(remainingObligations.get(0)));
    assertTrue(isE4.test(remainingObligations.get(1)));
    assertTrue(isIntermediateFalsy.test(remainingObligations.get(2)));
    // assertTrue(isE4symm.test(remainingObligations.get(3)));

    // prover.prove(remainingObligations.get(0));
    // prover.prove(remainingObligations.get(2));
    // TODO(lorenz.leutgeb): Also cover remainingObligations.get(2)
    // TODO(lorenz.leutgeb): Also cover remainingObligations.get(3) (E4symm).

    final var e4obligation = remainingObligations.get(1);
    final var e4result = prover.apply(e4obligation, LetTreeCfSimple.INSTANCE);
    assertEquals(5, e4result.size());
    prover.prove(e4result.get(0));
    prover.prove(e4result.subList(2, e4result.size()));

    var intermediate1 =
        prover.proveUntil(e4result.get(1), o -> o.getExpression() instanceof LetExpression);
    assertEquals(1, intermediate1.size());
    var intermediate1weakened = prover.apply(intermediate1.get(0), W.INSTANCE);
    assertEquals(1, intermediate1weakened.size());
    var intermediate2 = prover.apply(intermediate1weakened.get(0), LetTree.INSTANCE);

    /*
    assertEquals(1, intermediate1.size());
    var intermediate2 = prover.apply(intermediate1.get(0), LetTree.INSTANCE);
     */
    assertEquals(2, intermediate2.size());
    prover.prove(intermediate2.get(1));
    prover.prove(prover.apply(intermediate2.get(0), LetTreeCfSimple.INSTANCE));
    prover.plot();

    /*
    Set<Constraint> fixQ4 =
        Set.copyOf(
            eq(
                e4result.get(1).getContext().reorderByName("cr", "br", "z₅").getAnnotation(),
                Q4,
                "fix Q4"));

    Set<Constraint> fixP1 =
        Sets.union(
            Set.copyOf(eq(e4result.get(2).getContext().getAnnotation(), P1110, "fix P1")),
            Sets.union(
                Set.copyOf(eq(e4result.get(3).getContext().getAnnotation(), P1110, "fix P1")),
                Set.copyOf(eq(e4result.get(4).getContext().getAnnotation(), P1110, "fix P1"))));

    Set<Constraint> fixQ2 = new HashSet<>();
    StreamSupport.stream(prover.getProof().spliterator(), false)
        .filter(o -> Objects.nonNull(o.getExpression()))
        .forEach(
            o -> {
              if (o.getExpression().equals(E3)
                  && !o.getContext().getAnnotation().getName().startsWith("weaken")) {
                fixQ2.addAll(
                    eqSoft(
                        Q2,
                        o.getContext().reorderByName("cr", "bl", "br").getAnnotation(),
                        "fix Q2"));
              }
            });
     */

    final var combined =
        union(
            union(
                prover.getAccumulatedConstraints(),
                emptySet() /*cfProver.getAccumulatedConstraints()*/),
            sumConstraints);

    final var solution = ConstraintSystemSolver.solve(combined);
    // final var solution =
    //    prover.solve(); // (Sets.union(fixQ3, fixQ2)); // ;Sets.union(sumConstraints, fixQ2));
    // Constraint.plot(FQN, prover.getAccumulatedConstraints(), Paths.get("out"));
    assertTrue(solution.isPresent());
    final var minSolution =
        ConstraintSystemSolver.solve(
            sumConstraints, Paths.get("TODO"), List.of(rankCoefficientSum, coefficientSum));

    System.out.println(
        "splay_eq w/out minimization: "
            + Qvar.substitute(solution.get())
            + " -> "
            + Qpvar.substitute(solution.get()));
    System.out.println(
        "splay_eq w/    minimization: "
            + (minSolution
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println("splay_eq ideal             : " + Q + " -> " + Qp);

    assertAll(
        union(
            Set.of(
                () ->
                    assertContextEquals(
                        q3v, Q3, aboveRootObligation.getContext().substitute(solution.get())),
                () ->
                    assertContextEquals(
                        q3v,
                        Q3,
                        fromProver(
                                prover,
                                obligation -> E3.getFalsy().equals(obligation.getExpression()))
                            .substitute(solution.get())),
                () ->
                    assertContextEquals(
                        q3v,
                        Q3,
                        fromProver(
                                prover,
                                obligation ->
                                    INTERMEDIATE.getTruthy().equals(obligation.getExpression()))
                            .substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        List.of("cr", "bl", "br"),
                        Q3,
                        e4obligation.getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        List.of("cr", "br", "z"),
                        Q4,
                        e4result.get(1).getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        singletonList(""),
                        P1110,
                        e4result.get(2).getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        singletonList(""),
                        P1110,
                        e4result.get(3).getContext().substitute(solution.get())),
                () ->
                    assertContextEqualsByPrefixes(
                        singletonList(""),
                        P1110,
                        e4result.get(4).getContext().substitute(solution.get()))),
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
                      if (o.getExpression().equals(E3)
                          && !o.getContext().getAnnotation().getNameAndId().startsWith("weaken")) {
                        return (Executable)
                            () ->
                                assertContextEquals(
                                    AnnotatingContext.reorderByName(
                                        o.getExpression().freeVariables(),
                                        List.of("cr", "bl", "br")),
                                    Q2,
                                    o.getContext().substitute(solution.get()));
                      }
                      return null;
                    })
                .filter(Objects::nonNull)
                .collect(Collectors.toSet())));
  }

  @Test
  public void zigzigWithProof() throws IOException {
    final var Qvar = HEURISTIC.generate("Q", 1);
    // final var Qvar = Annotation.zero(1, "Qzero");
    // final var Qvar = Q;

    final var Qpvar = HEURISTIC.generate("Q'", 1);
    // final var Qpvar = Annotation.zero(1, "Q'zero");
    // final var Qpvar = Qp;

    // final var Pvar = HEURISTIC.generate("P", 1);
    final var Pvar = P;

    // final var Ppvar = HEURISTIC.generate("P'", 1);
    final var Ppvar = Pp;

    final var qRankCoefficientSum = new UnknownCoefficient("Qranksum");
    final var qpRankCoefficientSum = new UnknownCoefficient("Qpranksum");
    final var qCoefficientSum = new UnknownCoefficient("Qothersum");
    final var qpCoefficientSum = new UnknownCoefficient("Qpothersum");
    final var rankCoefficientSum = new UnknownCoefficient("rankCoefficientSum");
    final var coefficientSum = new UnknownCoefficient("coefficientSum");

    final var sumConstraints =
        Set.<Constraint>of(
            Qvar.sumRankCoefficients(qRankCoefficientSum),
            Qpvar.sumRankCoefficients(qpRankCoefficientSum),
            Qvar.sumCoefficients(qCoefficientSum),
            Qpvar.sumCoefficients(qpCoefficientSum),
            new EqualsSumConstraint(
                rankCoefficientSum,
                List.of(qRankCoefficientSum, qpRankCoefficientSum),
                "(opt) rank coefficient sum"),
            new EqualsSumConstraint(
                coefficientSum,
                List.of(qCoefficientSum, qpCoefficientSum),
                "(opt) coefficient sum"));

    final Set<Constraint> haveRank =
        singleton(
            new LessThanOrEqualConstraint(
                ONE, Qpvar.getRankCoefficient(), "have non-zero Q' rank"));

    final var fd = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);

    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                new CombinedFunctionAnnotation(
                    new FunctionAnnotation(Qvar, Qpvar),
                    Set.of(
                        new FunctionAnnotation(Pvar, Ppvar),
                        new FunctionAnnotation(
                            Annotation.zero(1, "cfargszero"),
                            Annotation.zero(1, "cfreturnzero"))))),
            fd.getSizeAnalysis(),
            HEURISTIC);

    final Prover prover = new Prover("splay", globals);

    final var root = new Obligation(SPLAY.treeLikeArguments(), Qvar, SPLAY.getBody(), Qpvar, 1);

    final var visitor = new TacticVisitorImpl(prover, root);

    visitor.visitTactic(
        new TacticParser(
                new CommonTokenStream(
                    new TacticLexer(
                        CharStreams.fromPath(
                            Paths.get(
                                ".", "src", "test", "resources", "tactics", "splay_eq.txt")))))
            .tactic());

    // final var fixing = fixQ2;
    final var fixing = Collections.<Constraint>emptySet();
    // final var fixing = Sets.union(fixQ2, fixQ3);
    final var minimization = union(fixing, sumConstraints);
    final var minimizationTargets = List.of(rankCoefficientSum, coefficientSum);

    final var solution = prover.solve(fixing);
    assertTrue(solution.isPresent());
    final var minSolution = prover.solve(minimization, minimizationTargets);
    final var minSolutionWithRank =
        prover.solve(union(minimization, haveRank), minimizationTargets);

    Set<Constraint> softFixQ = Set.copyOf(eqSoft(Q, Qvar, "soft fix Q"));
    final var minSolutionWithRankAndFixedQ =
        prover.solve(union(minimization, union(haveRank, softFixQ)), minimizationTargets);

    System.out.println(
        "splay_eq w/out minimization: "
            + Qvar.substitute(solution.get())
            + " -> "
            + Qpvar.substitute(solution.get()));
    System.out.println(
        "splay_eq w/    minimization: "
            + (minSolution
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        "splay_eq rank  minimization: "
            + (minSolutionWithRank
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        "splay_eq w/ fixed Q        : "
            + (minSolutionWithRankAndFixedQ
                .map(sol -> Qvar.substitute(sol) + " -> " + Qpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println("splay_eq expected          : " + Q + " -> " + Qp);
    System.out.println("splay_eq cf          : " + P + " -> " + Pp);
  }

  @Test
  public void insert() throws IOException {
    final var Rvar = HEURISTIC.generate("R", 1);
    // final var Rvar = Annotation.zero(1, "Rzero");
    // final var Rvar = R;

    final var Rpvar = HEURISTIC.generate("R'", 1);
    // final var Rpvar = Annotation.zero(1, "R'zero");
    // final var Rpvar = Rp;

    // final var Qvar = HEUQISTIC.generate("Q", 1);
    // final var Qvar = Annotation.zero(1, "Qzero");
    final var Qvar = Q;

    // final var Qpvar = HEURISTIC.generate("Q'", 1);
    // final var Qpvar = Annotation.zero(1, "Q'zero");
    final var Qpvar = Qp;

    final var Rcfvar = HEURISTIC.generate("R", 1);
    // final var Rcfvar = Annotation.zero(1, "Rzero");
    // final var Rcfvar = R;

    final var Rpcfvar = HEURISTIC.generate("R'", 1);
    // final var Rpcfvar = Annotation.zero(1, "R'zero");
    // final var Rpcfvar = Rp;

    // final var Pvar = HEURISTIC.generate("P", 1);
    final var Pvar = P;

    // final var Ppvar = HEURISTIC.generate("P'", 1);
    final var Ppvar = Pp;

    final var qRankCoefficientSum = new UnknownCoefficient("Qranksum");
    final var qpRankCoefficientSum = new UnknownCoefficient("Qpranksum");
    final var qCoefficientSum = new UnknownCoefficient("Qothersum");
    final var qpCoefficientSum = new UnknownCoefficient("Qpothersum");
    final var rankCoefficientSum = new UnknownCoefficient("rankCoefficientSum");
    final var coefficientSum = new UnknownCoefficient("coefficientSum");

    final var sumConstraints =
        Set.<Constraint>of(
            Qvar.sumRankCoefficients(qRankCoefficientSum),
            Qpvar.sumRankCoefficients(qpRankCoefficientSum),
            Qvar.sumCoefficients(qCoefficientSum),
            Qpvar.sumCoefficients(qpCoefficientSum),
            new EqualsSumConstraint(
                rankCoefficientSum,
                List.of(qRankCoefficientSum, qpRankCoefficientSum),
                "(opt) rank coefficient sum"),
            new EqualsSumConstraint(
                coefficientSum,
                List.of(qCoefficientSum, qpCoefficientSum),
                "(opt) coefficient sum"));

    final Set<Constraint> haveRank =
        singleton(
            new LessThanOrEqualConstraint(
                ONE, Qpvar.getRankCoefficient(), "have non-zero Q' rank"));

    final var insertFd = PROGRAM.getFunctionDefinitions().get(INSERT_FQN);
    final var splayFd = PROGRAM.getFunctionDefinitions().get(SPLAY_FQN);

    final var globals =
        new AnnotatingGlobals(
            Map.of(
                SPLAY_FQN,
                    new CombinedFunctionAnnotation(
                        new FunctionAnnotation(Qvar, Qpvar),
                        Set.of(
                            new FunctionAnnotation(Pvar, Ppvar),
                            new FunctionAnnotation(
                                Annotation.zero(1, "splaycfargszero"),
                                Annotation.zero(1, "splaycfreturnzero")))),
                INSERT_FQN,
                    new CombinedFunctionAnnotation(
                        new FunctionAnnotation(Rvar, Rpvar),
                        Set.of(
                            new FunctionAnnotation(Rcfvar, Rpcfvar),
                            new FunctionAnnotation(
                                Annotation.zero(1, "insertcfargszero"),
                                Annotation.zero(1, "insertcfreturnzero"))))),
            insertFd.getSizeAnalysis(),
            HEURISTIC);

    final Prover prover = new Prover("insert", globals);
    final var cfRoot =
        new Obligation(insertFd.treeLikeArguments(), Rcfvar, insertFd.getBody(), Rpcfvar, 0);
    prover.prove(cfRoot);

    final var root =
        new Obligation(insertFd.treeLikeArguments(), Rvar, insertFd.getBody(), Rpvar, 1);

    final var visitor = new TacticVisitorImpl(prover, root);

    visitor.visitTactic(
        new TacticParser(
                new CommonTokenStream(
                    new TacticLexer(
                        CharStreams.fromPath(
                            Paths.get(
                                ".", "src", "test", "resources", "tactics", "insert_eq.txt")))))
            .tactic());

    // final var fixing = fixQ2;
    final var fixing = Collections.<Constraint>emptySet();
    // final var fixing = Sets.union(fixQ2, fixQ3);
    final var minimization = union(fixing, sumConstraints);
    final var minimizationTargets = List.of(rankCoefficientSum, coefficientSum);

    final var solution = prover.solve(fixing);
    assertTrue(solution.isPresent());
    final var minSolution = prover.solve(minimization, minimizationTargets);

    System.out.println(
        "insert_eq w/out minimization: "
            + Rvar.substitute(solution.get())
            + " -> "
            + Rpvar.substitute(solution.get()));
    System.out.println(
        "insert_eq w/    minimization: "
            + (minSolution
                .map(sol -> Rvar.substitute(sol) + " -> " + Rpvar.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        "insert_eq cf w/ minimization: "
            + (minSolution
                .map(sol -> Rcfvar.substitute(sol) + " -> " + Rpcfvar.substitute(sol))
                .orElse("UNSAT")));
  }
}
