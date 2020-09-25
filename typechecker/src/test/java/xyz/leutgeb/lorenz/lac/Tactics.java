package xyz.leutgeb.lorenz.lac;

import static com.google.common.collect.Sets.union;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.fail;
import static xyz.leutgeb.lorenz.lac.TestUtil.printTable;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.unitIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.THREE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.fqnToFlatFilename;
import static xyz.leutgeb.lorenz.lac.util.Util.zeroCoefficients;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.FunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class Tactics {
  protected static final AnnotationHeuristic HEURISTIC = SmartRangeHeuristic.DEFAULT;

  protected static final Annotation Q =
      new Annotation(List.of(ONE), Map.of(List.of(1, 0), THREE, unitIndex(1), ONE), "Q");

  protected static final Annotation P =
      new Annotation(List.of(ZERO), Map.of(List.of(1, 0), ONE), "P");

  protected static final Annotation Pp =
      new Annotation(zeroCoefficients(1), Map.of(List.of(1, 0), ONE), "P'");

  protected static final Annotation Qp = new Annotation(List.of(ONE), emptyMap(), "Q'");
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

  @ParameterizedTest
  @CsvSource({
    "PairingHeap.link,PairingHeap/link",
    "SplayTree.splay_eq,SplayTree/splay_eq",
    // "SplayTree.splay_eq,SplayTree/splay_eq-auto",
    "SplayTree.splay_eq,SplayTree/splay_eq-zigzig",
    "SplayTree.insert_eq,SplayTree/insert_eq",
    "SplayTree.splay_max_eq,SplayTree/splay_max_eq",
    "SplayTree.contains_eq,SplayTree/contains_eq",
  })
  public void all(String fqn, String proofFileName) throws IOException {
    final var fd = PROGRAM.getFunctionDefinitions().get(fqn);

    final Map<String, FunctionAnnotation> annotations = new HashMap<>();
    final Map<String, Set<FunctionAnnotation>> cfAnnotations = new HashMap<>();
    fd.stubAnnotations(annotations, cfAnnotations, HEURISTIC);

    boolean splayVar = true;

    // final var Qvar I= splayVar ?

    if (!fqn.equals(SPLAY_FQN) && fd.getOcurringFunctions().contains(SPLAY_FQN)) {
      annotations.put(SPLAY_FQN, new FunctionAnnotation(Q, Qp));

      cfAnnotations.put(
          SPLAY_FQN,
          Set.of(
              new FunctionAnnotation(P, Pp),
              new FunctionAnnotation(
                  Annotation.zero(1, SPLAY_FQN + ":cfz"),
                  Annotation.zero(1, SPLAY_FQN + ":cfz'"))));
    }

    final var globals =
        new AnnotatingGlobals(annotations, cfAnnotations, fd.getSizeAnalysis(), HEURISTIC);

    final var from = fd.getAnnotation().from();
    final var to = fd.getAnnotation().to();

    final var qRankCoefficientSum = new UnknownCoefficient("Qranksum");
    final var qpRankCoefficientSum = new UnknownCoefficient("Qpranksum");
    final var qCoefficientSum = new UnknownCoefficient("Qothersum");
    final var qpCoefficientSum = new UnknownCoefficient("Qpothersum");
    final var rankCoefficientSum = new UnknownCoefficient("rankCoefficientSum");
    final var coefficientSum = new UnknownCoefficient("coefficientSum");
    final var setDiffSum = new UnknownCoefficient("setDiffSum");
    final var pairwiseDiffSum = new UnknownCoefficient("pairwiseDiffSum");

    final var sumConstraints =
        Set.<Constraint>of(
            from.sumRankCoefficients(qRankCoefficientSum),
            to.sumRankCoefficients(qpRankCoefficientSum),
            from.sumCoefficients(qCoefficientSum),
            to.sumCoefficients(qpCoefficientSum),
            new EqualsSumConstraint(
                rankCoefficientSum,
                List.of(qRankCoefficientSum, qpRankCoefficientSum),
                "(opt) rank coefficient sum"),
            new EqualsSumConstraint(
                coefficientSum,
                List.of(qCoefficientSum, qpCoefficientSum),
                "(opt) coefficient sum"));

    final var setCountingConstraints =
        from.size() == to.size()
            ? Set.copyOf(from.diff(to, setDiffSum))
            : Collections.<Constraint>emptySet();
    final var pairwiseDiffConstraints =
        from.size() == to.size()
            ? Set.copyOf(from.pairwiseDiff(to, pairwiseDiffSum))
            : Collections.<Constraint>emptySet();

    if (!PROGRAM.getFunctionDefinitions().containsKey(fqn)) {
      fail("Could not find function definition for '" + fqn + "'.");
    }

    final Prover prover = new Prover(fqnToFlatFilename(fqn), globals);

    for (var cfAnnotation : fd.getCfAnnotations()) {
      final var cfRoot =
          new Obligation(
              fd.treeLikeArguments(), cfAnnotation.from(), fd.getBody(), cfAnnotation.to(), 0);
      prover.prove(cfRoot);
    }

    final var root = new Obligation(fd.treeLikeArguments(), from, fd.getBody(), to, 1);
    prover.read(
        root, Paths.get(".", "src", "test", "resources", "tactics", proofFileName + ".txt"));

    /*
    final var solution = prover.solve();
    assertTrue(solution.isPresent());
     */

    final var minimizationTargets = List.<Coefficient>of(rankCoefficientSum, coefficientSum);
    final var minSolution = prover.solve(sumConstraints, minimizationTargets);

    final var minimizationWithSets =
        union(sumConstraints, union(setCountingConstraints, pairwiseDiffConstraints));
    final var minimizationTargetsWithSets =
        from.size() == to.size()
            ? List.<Coefficient>of(setDiffSum, pairwiseDiffSum, rankCoefficientSum, coefficientSum)
            : List.<Coefficient>of(rankCoefficientSum, coefficientSum);
    final var minSetSolution = prover.solve(minimizationWithSets, minimizationTargetsWithSets);

    final var minSetSolutionRat =
        prover.solve(
            minimizationWithSets,
            minimizationTargetsWithSets,
            ConstraintSystemSolver.Domain.RATIONAL);

    /*
    System.out.println(
        fqn
            + "         : "
            + from.substitute(solution.get())
            + " -> "
            + to.substitute(solution.get()));
     */
    System.out.println(
        fqn
            + "    min1 : "
            + (minSolution
                .map(sol -> from.substitute(sol) + " -> " + to.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        fqn
            + ":cf min1 : "
            + fd.getCfAnnotations().stream()
                .map(
                    annotation ->
                        (minSolution
                            .map(
                                sol ->
                                    annotation.from().substitute(sol)
                                        + " -> "
                                        + annotation.to().substitute(sol))
                            .orElse("UNSAT")))
                .collect(Collectors.joining(", ", "{", "}")));
    System.out.println(
        fqn
            + "    min2 : "
            + (minSetSolution
                .map(sol -> from.substitute(sol) + " -> " + to.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        fqn
            + ":cf min2 : "
            + fd.getCfAnnotations().stream()
                .map(
                    annotation ->
                        (minSetSolution
                            .map(
                                sol ->
                                    annotation.from().substitute(sol)
                                        + " -> "
                                        + annotation.to().substitute(sol))
                            .orElse("UNSAT")))
                .collect(Collectors.joining(", ", "{", "}")));

    final var acs = new ArrayList<AnnotatingContext>(prover.getNamed().size());
    final var as = new ArrayList<Annotation>(prover.getNamed().size() + 2);
    as.add(to);
    as.add(from);
    for (var entry : prover.getNamed().entrySet()) {
      acs.add(
          entry.getValue().getContext().substitute(minSetSolution.get()).rename(entry.getKey()));
      as.add(
          entry
              .getValue()
              .getAnnotation()
              .substitute(minSetSolution.get())
              .rename(entry.getKey() + "'"));
    }

    System.out.println(printTable(acs, as));
    System.out.println(
        fqn
            + "    min2r: "
            + (minSetSolutionRat
                .map(sol -> from.substitute(sol) + " -> " + to.substitute(sol))
                .orElse("UNSAT")));
    System.out.println(
        fqn
            + ":cf min2r: "
            + fd.getCfAnnotations().stream()
                .map(
                    annotation ->
                        (minSetSolutionRat
                            .map(
                                sol ->
                                    annotation.from().substitute(sol)
                                        + " -> "
                                        + annotation.to().substitute(sol))
                            .orElse("UNSAT")))
                .collect(Collectors.joining(", ", "{", "}")));
  }
}
