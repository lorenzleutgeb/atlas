package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static com.google.common.collect.Lists.cartesianProduct;
import static com.google.common.collect.Streams.concat;
import static com.microsoft.z3.Status.UNKNOWN;
import static com.microsoft.z3.Status.UNSATISFIABLE;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.atlas.typing.resources.Annotation.*;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient.unknownMaybeNegative;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.MINUS_ONE;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.MINUS_TWO;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.atlas.util.Util.append;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.flag;

import com.google.common.collect.Streams;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.IntExpr;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.shortestpath.AllDirectedPaths;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.atlas.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;

@Slf4j
public class W implements Rule {
  public static final W INSTANCE = new W();

  private static final IntIdGenerator ID = IntIdGenerator.fromZeroInclusive();

  private static final Map<List<List<Integer>>, List<LessThanOrEqual<List<Integer>>>> MONO_CACHE =
      new HashMap<>();

  private static final boolean DEBUG_SIZE = false;
  private static final boolean DEBUG_KNOWLEDGE = false;

  private static Annotation generateP(
      Annotation q, AnnotationHeuristic heuristic, Map<String, String> arguments) {
    // This method contains a leaky abstraction.
    // We should not compare the instance of the AnnotationHeuristic passed as argument.
    if (false && heuristic == SmartRangeHeuristic.DEFAULT && flag(W.class, arguments, "neg")) {
      return SmartRangeHeuristic.INCL_NEGATIVE_ONE.generate("w" + q.getName(), q);
    } else {
      return heuristic.generate("w" + q.getName(), q);
    }
  }

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var q = obligation.getContext().getAnnotation();
    final var qp = obligation.getAnnotation();

    // final var p = generateP(q, globals.getHeuristic(), arguments);
    final var p = globals.getHeuristic().generate("w" + q.getName(), q);
    final var pp = globals.getHeuristic().generate("w" + qp.getName(), qp);

    final List<IdentifierExpression> ids = obligation.getContext().getIds();

    return new Rule.ApplicationResult(
        singletonList(
            obligation.keepCost(new AnnotatingContext(ids, p), obligation.getExpression(), pp)),
        singletonList(
            append(
                compareCoefficientsLessOrEqualUsingFarkas(
                    ids, p, q, globals.getSizeAnalysis(), arguments),
                compareCoefficientsLessOrEqual(qp, pp, "Q'"))),
        emptyList());
  }

  public static List<Constraint> compareCoefficientsLessOrEqual(
      Annotation left, Annotation right, String reason) {
    return compareCoefficients(
        left,
        right,
        (x, y) -> new LessThanOrEqualConstraint(x, y, "(w) " + reason + " " + x + " ≤ " + y));
  }

  private static List<Constraint> compareCoefficients(
      Annotation left,
      Annotation right,
      BiFunction<Coefficient, Coefficient, Constraint> comparator) {
    if (left.size() != right.size()) {
      throw bug("cannot compare annotations of different size");
    }
    return concat(
            nonRankIndices(left, right)
                .map(
                    i ->
                        comparator.apply(
                            left.getCoefficientOrZero(i), right.getCoefficientOrZero(i))),
            IntStream.range(0, left.size())
                .mapToObj(
                    i ->
                        comparator.apply(
                            left.getRankCoefficientOrZero(i), right.getRankCoefficientOrZero(i))))
        .collect(toList());
  }

  public static List<Constraint> compareNonRankCoefficients(
      Annotation left,
      Annotation right,
      BiFunction<Coefficient, Coefficient, Constraint> comparator) {
    if (left.size() != right.size()) {
      throw bug("cannot compare annotations of different size");
    }
    return concat(
            nonRankIndices(left, right)
                .map(
                    i ->
                        comparator.apply(
                            left.getCoefficientOrZero(i), right.getCoefficientOrZero(i))))
        .collect(toList());
  }

  public static List<Constraint> compareCoefficientsLessOrEqualUsingFarkas(
      List<IdentifierExpression> identifiers,
      Annotation left,
      Annotation right,
      Graph<IdentifierExpression, SizeEdge> sizeAnalysis,
      Map<String, String> arguments) {
    // left is P in paper.
    // right is Q in paper.

    if (left.size() != right.size()) {
      throw new IllegalArgumentException();
    }
    final var size = left.size();

    final List<Constraint> constraints = new ArrayList<>();
    final var lemmaRk1 = flag(W.class, arguments, "lrk1");
    final var rankColumns = lemmaRk1;

    if (!rankColumns) {
      IntStream.range(0, size)
          .mapToObj(
              i ->
                  new LessThanOrEqualConstraint(
                      left.getRankCoefficientOrZero(i),
                      right.getRankCoefficientOrZero(i),
                      "(w) rk(" + identifiers.get(i) + ") (at index " + i + ")"))
          .forEach(constraints::add);
    }

    final var potentialFunctions = Annotation.nonRankIndices(left, right).collect(toList());
    final var columns = potentialFunctions.size() + (rankColumns ? size : 0);

    // Column numbering:
    //  - From 0 to potentialFunctions.size() are variables for non-rank.
    //  - From potentialFunctions.size() to potentialFunctions.size() + size are rank.

    final var all = flag(W.class, arguments, "all");
    final var lemma2xy = false; //flag(W.class, arguments, "l2xy") || all;
    final var lemmap1 = false; // flag(W.class, arguments, "lp1") || all;
    final var useSizeAnalysis = false; // flag(W.class, arguments, "size") || all;
    // TODO(lorenzleutgeb): Fix this lemma.
    final var lemmap1y = false; // flag(W.class, arguments, "lp1y") || all;
    final var mono = false; // flag(W.class, arguments, "mono") || useSizeAnalysis || all;

    final ReducedSizeAnalysis reducedSizeAnalysis;
    final Set<LessThan<Integer>> knowLt;
    final Set<Equal<Integer>> knowEq;
    final Set<Integer> knowOne;
    if (useSizeAnalysis) {
      reducedSizeAnalysis = reduceSizeAnalysis(identifiers, sizeAnalysis);
      // TODO: Clarify size analysis.
      knowLt = emptySet(); // reducedSizeAnalysis.knowLt;
      knowEq = emptySet(); // reducedSizeAnalysis.knowEq;
      knowOne = reducedSizeAnalysis.knowOne;

      if (DEBUG_SIZE && knowEq.isEmpty() && knowLt.isEmpty() && knowOne.isEmpty()) {
        // Breakpoint.
        log.info("Size analysis did not contribute any side conditions for monotony analysis.");
      }
    } else {
      knowLt = emptySet();
      knowEq = emptySet();
      knowOne = emptySet();
    }

    final List<LessThanOrEqual<List<Integer>>> monotonyInstances =
        mono ? monotony(potentialFunctions, knowLt, knowEq, knowOne) : emptyList();

    final List<List<List<Integer>>> lemma2XYInstances =
        lemma2xy ? lemma2XY(potentialFunctions) : emptyList();

    final List<Pair<List<Integer>, List<Integer>>> lemmaPlus1Instances =
        append(
            lemmap1 ? lemmaPlus1(potentialFunctions) : emptyList(),
            lemmap1y ? lemmaPlus1Known(potentialFunctions, knowOne) : emptyList());

    final var lemmap2 = flag(W.class, arguments, "lp2");
    final List<Pair<List<Integer>, List<Integer>>> lemmaPlus2Instances =
        lemmap2 ? lemmaPlus2(potentialFunctions) : emptyList();

    if (DEBUG_SIZE
        && useSizeAnalysis
        && (!knowLt.isEmpty() || !knowEq.isEmpty() || !knowOne.isEmpty())) {
      final List<LessThanOrEqual<List<Integer>>> monotonyInstancesWithoutSizeKnowledge;
      monotonyInstancesWithoutSizeKnowledge =
          monotony(potentialFunctions, emptySet(), emptySet(), emptySet());
      var improvement = new ArrayList<>(monotonyInstances);
      improvement.removeAll(monotonyInstancesWithoutSizeKnowledge);
      /*
         Sets.difference(
                 Set.copyOf(monotonyInstances), Set.copyOf(monotonyInstancesWithoutSizeKnowledge))
             .stream()
             .collect(Collectors.toCollection(ArrayList::new));
      */
      if (improvement.isEmpty()) {
        // Breakpoint.
        log.info("Size analysis did not contribute anything to monotony analysis.");
      } else {
        /*
        final var throwaway =
            new ArrayList<>(
                improvement.subList(improvement.size() / 2 - 5, improvement.size() - 1));
        monotonyInstances.removeAll(throwaway);
        improvement.removeAll(throwaway);
         */
        log.info("Monotony without size analysis:");
        monotonyInstancesWithoutSizeKnowledge.forEach(x -> log.info("{}", x));
        log.info("Improvement of size analysis:");
        improvement.forEach(x -> log.info("{}", x));
        /*
        log.info("Thrown away:");
        throwaway.forEach(x -> log.info("{}", x));
         */
      }
    }

    if (DEBUG_KNOWLEDGE) {
      log.info("(w) --- " + left.getId() + " <= " + right.getId() + " --- ");
      log.info("pot: " + potentialFunctions);
      log.info("ids: " + identifiers);
      log.info("one: " + knowOne.stream().map(identifiers::get).toList());
      log.info("lemma2XY:");
      lemma2XYInstances.forEach(
          instance ->
              log.info(
                  "2 * [...0, 2] + "
                      + instance.get(0)
                      + " + "
                      + instance.get(1)
                      + " <= 2 * "
                      + instance.get(2)));
      log.info(
          "lts: "
              + knowLt.stream()
                  .map(
                      x ->
                          new LessThanOrEqual(
                              identifiers.get(x.smaller), identifiers.get(x.greater)))
                  .toList());
      log.info(
          "eqs: "
              + knowEq.stream()
                  .map(x -> new Equal(identifiers.get(x.left), identifiers.get(x.right)))
                  .toList());

      // log(t+r-1)>=log(t)
      // [1, 0, 0] <= [1, 1, -1]
      // [0, 1, 0] <= [1, 1, -1]

      log.info("monotony:");
      monotonyInstances.forEach(x -> log.info("{}", x));

      log.info("lemma1Plus:");
      lemmaPlus1Instances.forEach(
          instance -> log.info(instance.getRight() + " <= 1 * [...0, 2] + " + instance.getLeft()));

      log.info("lemma2Plus:");
      lemmaPlus2Instances.forEach(
          instance -> log.info(instance.getRight() + " <= 2 * [...0, 2] + " + instance.getLeft()));

      log.info(" --- ");
    }

    // m is the number of rows of expert knowledge.
    final var m =
        monotonyInstances.size()
            + lemma2XYInstances.size()
            + lemmaPlus1Instances.size()
            + lemmaPlus2Instances.size()
            + (lemmaRk1 ? size : 0);

    if (m == 0) {
      // If we have no expert knowledge, fall back to comparing coefficients.
      return compareCoefficientsLessOrEqual(left, right, "fallback");
    }

    final var wid = "w" + ID.next();

    final var p = potentialFunctions.stream().map(left::getCoefficientOrZero).toList();
    final var q = potentialFunctions.stream().map(right::getCoefficientOrZero).toList();

    // NOTE: We do not add constraints saying f ≥ 0. This is generated for all unknown coefficients!
    final var f =
        IntStream.range(0, m).mapToObj(i -> Coefficient.unknown(wid + ".f[" + i + "]")).toList();

    final List<List<Coefficient>> sumByColumn = new ArrayList<>(potentialFunctions.size());

    for (int column = 0; column < potentialFunctions.size(); column++) {
      final var tmp = new ArrayList<Coefficient>(3);
      tmp.add(q.get(column));
      sumByColumn.add(tmp);
    }

    for (int rank = 0; rankColumns && rank < size; rank++) {
      final var tmp = new ArrayList<Coefficient>(3);
      tmp.add(right.getRankCoefficientOrZero(rank));
      sumByColumn.add(tmp);
    }

    if (sumByColumn.size() != columns) {
      throw new IllegalStateException();
    }

    if (!monotonyInstances.isEmpty()) {
      for (int column = 0; column < potentialFunctions.size(); column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < monotonyInstances.size(); row++) {
          final var knowledgeRow = monotonyInstances.get(row);
          if (potentialFunction.equals(knowledgeRow.smaller)) {
            sum.add(f.get(row));
          } else if (potentialFunction.equals(knowledgeRow.greater)) {
            sum.add(f.get(row).negate());
          }
        }
      }
    }

    if (!lemma2XYInstances.isEmpty()) {
      final var lemmaOffset = monotonyInstances.size();
      for (int column = 0; column < potentialFunctions.size(); column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < lemma2XYInstances.size(); row++) {
          final var knowledgeRow = lemma2XYInstances.get(row);
          UnknownCoefficient fi = f.get(row + lemmaOffset);
          if (potentialFunction.equals(knowledgeRow.get(0))) {
            // fi corresponds to log(x)
            sum.add(fi);
          } else if (potentialFunction.equals(knowledgeRow.get(1))) {
            // fi corresponds to log(y)
            sum.add(fi);
          } else if (potentialFunction.equals(knowledgeRow.get(2))) {
            // fi corresponds to log(x + y)
            UnknownCoefficient prod =
                unknownMaybeNegative(wid + ".lxy2.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_TWO, fi), "(w) lxy2" + prod + " = (-2) * " + fi));
            sum.add(prod);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            UnknownCoefficient prod =
                unknownMaybeNegative(wid + ".lxy2.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(TWO, fi), "(w) lxy2 const: " + prod + " = (2) * " + fi));
            sum.add(prod);
          }
        }
      }
    }

    if (!lemmaPlus1Instances.isEmpty()) {
      final var lemmaOffset = monotonyInstances.size() + lemma2XYInstances.size();
      for (int column = 0; column < potentialFunctions.size(); column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < lemmaPlus1Instances.size(); row++) {
          final var knowledgeRow = lemmaPlus1Instances.get(row);
          final var fi = f.get(row + lemmaOffset);
          if (potentialFunction.equals(knowledgeRow.getLeft())) {
            // fi corresponds to log(x)
            final var prod = unknownMaybeNegative(wid + ".lp1.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp1 log(x): " + prod + " = (-1) * " + fi));
            sum.add(prod);
          } else if (potentialFunction.equals(knowledgeRow.getRight())) {
            // fi corresponds to log(x + 1)
            sum.add(fi);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            final var prod = unknownMaybeNegative(wid + ".lp1.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp1 const: " + prod + " = (-1) * " + fi));
            sum.add(prod);
          }
        }
      }
    }

    if (!lemmaPlus2Instances.isEmpty()) {
      final var lemmaOffset =
          monotonyInstances.size() + lemma2XYInstances.size() + lemmaPlus1Instances.size();
      for (int column = 0; column < potentialFunctions.size(); column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < lemmaPlus2Instances.size(); row++) {
          final var knowledgeRow = lemmaPlus2Instances.get(row);
          final var fi = f.get(row + lemmaOffset);
          if (potentialFunction.equals(knowledgeRow.getLeft())) {
            // fi corresponds to log(x)
            final var prod = unknownMaybeNegative(wid + ".lp2.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp2 log(x): " + prod + " = (-1) * " + fi));
            sum.add(prod);
          } else if (potentialFunction.equals(knowledgeRow.getRight())) {
            // fi corresponds to log(x + 2)
            sum.add(fi);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            final var prod = unknownMaybeNegative(wid + ".lp2.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_TWO, fi), "(w) lp2 const: " + prod + " = (-2) * " + fi));
            sum.add(prod);
          }
        }
      }
    }

    if (lemmaRk1) {
      final var unitIndex = potentialFunctions.indexOf(unitIndex(size));
      if (unitIndex < 0) {
        throw new UnsupportedOperationException();
      }

      final var offset =
          monotonyInstances.size()
              + lemma2XYInstances.size()
              + lemmaPlus1Instances.size()
              + lemmaPlus2Instances.size();
      for (var column = 0; column < columns; column++) {
        final List<Coefficient> sum = sumByColumn.get(column);
        for (var rank = 0; rank < size; rank++) {
          final var fi = f.get(offset + rank);
          if (column == unitIndex) {
            sum.add(fi);
          } else if (column == potentialFunctions.size() + rank) {
            sum.add(fi.negate());
          }
        }
      }
    }

    // p ≤ fA + q (Note: fA is computed using matrix multiplication WITH f FROM THE LEFT.)
    for (int column = 0; column < columns; column++) {
      final var columnAsString =
          column < potentialFunctions.size()
              ? potentialFunctions.get(column).toString()
              : String.valueOf(column - potentialFunctions.size());

      final var fAplusQ = unknownMaybeNegative(wid + ".(fA + q)[" + columnAsString + "]");
      constraints.add(
          new EqualsSumConstraint(
              fAplusQ,
              sumByColumn.get(column),
              "(w "
                  + left.getId()
                  + " ≤ "
                  + right.getId()
                  + ") "
                  + fAplusQ
                  + " = Σ... + q["
                  + columnAsString
                  + "] (w)"));
      constraints.add(
          new LessThanOrEqualConstraint(
              (column < potentialFunctions.size()
                  ? p.get(column)
                  : left.getRankCoefficientOrZero(column - potentialFunctions.size())),
              fAplusQ,
              "(w "
                  + left.getId()
                  + " ≤ "
                  + right.getId()
                  + ") "
                  + wid
                  + ".p["
                  + columnAsString
                  + "] ≤ "
                  + fAplusQ
                  + " (w)"));
    }

    return constraints;
  }

  private record ReducedSizeAnalysis(
      Set<LessThan<Integer>> knowLt, Set<Equal<Integer>> knowEq, Set<Integer> knowOne) {}

  private static ReducedSizeAnalysis reduceSizeAnalysis(
      List<IdentifierExpression> identifiers, Graph<IdentifierExpression, SizeEdge> sizeAnalysis) {
    Set<LessThan<Integer>> knowLt = new HashSet<>();
    Set<Equal<Integer>> knowEq = new HashSet<>();

    final Predicate<GraphPath<IdentifierExpression, SizeEdge>> isGt =
        path ->
            path.getEdgeList().stream().anyMatch(edge -> SizeEdge.Kind.GT.equals(edge.getKind()));

    final Predicate<GraphPath<IdentifierExpression, SizeEdge>> isEq =
        path ->
            path.getEdgeList().stream().allMatch(edge -> SizeEdge.Kind.EQ.equals(edge.getKind()));

    final Set<Integer> knowOne =
        sizeAnalysis.containsVertex(IdentifierExpression.leaf())
            ? identifiers.stream()
                .filter(sizeAnalysis::containsVertex)
                .filter(
                    identifier -> {
                      final var leafPaths =
                          new AllDirectedPaths<>(sizeAnalysis)
                              .getAllPaths(identifier, IdentifierExpression.leaf(), true, 16);
                      return !leafPaths.isEmpty() && leafPaths.stream().anyMatch(isEq);
                    })
                .map(identifiers::indexOf)
                .collect(Collectors.toSet())
            : emptySet();

    cartesianProduct(identifiers, identifiers).stream()
        .filter(pair -> !pair.get(0).equals(pair.get(1)))
        .filter(pair -> sizeAnalysis.containsVertex(pair.get(0)))
        .filter(pair -> sizeAnalysis.containsVertex(pair.get(1)))
        .map(
            pair ->
                new AllDirectedPaths<>(sizeAnalysis)
                    .getAllPaths(pair.get(0), pair.get(1), true, Integer.MAX_VALUE))
        .forEach(
            paths -> {
              final var pathGt = paths.stream().filter(isGt).findFirst();
              if (pathGt.isPresent()) {
                knowLt.add(
                    new LessThan<>(
                        identifiers.indexOf(pathGt.get().getEndVertex()),
                        identifiers.indexOf(pathGt.get().getStartVertex())));
                return;
              }
              final var pathEq = paths.stream().filter(isEq).findFirst();
              if (pathEq.isPresent()) {
                int startIndex = identifiers.indexOf(pathEq.get().getStartVertex());
                int endIndex = identifiers.indexOf(pathEq.get().getEndVertex());
                if (startIndex < endIndex) {
                  knowEq.add(new Equal<>(startIndex, endIndex));
                }
              }
            });

    return new ReducedSizeAnalysis(knowLt, knowEq, knowOne);
  }

  /**
   * ∀ x ≥ 1, y ≥ 1 . 2 + log(x) + log(y) ≤ 2 * log(x + y)
   *
   * <p>Note that we exclude x = 2 and y = 2 and x + y = 2, because we want to express 2 as 2 * 1
   * and 1 as log(2).
   *
   * @return Instantiations as lists with three elements. The first element corresponds to x, the
   *     second element corresponds to y and the third element corresponds to x + y.
   */
  private static List<List<List<Integer>>> lemma2XY(List<List<Integer>> potentialFunctions) {
    final var set = Set.copyOf(potentialFunctions);
    return cartesianProduct(potentialFunctions, potentialFunctions).stream()
        .filter(pair -> !Annotation.isUnitIndex(pair.get(0))) // x is not unit
        .filter(pair -> !Annotation.isUnitIndex(pair.get(1))) // y is not unit
        .filter(pair -> INDEX_COMPARATOR.compare(pair.get(0), pair.get(1)) < 1)
        .map(pair -> List.of(pair.get(0), pair.get(1), sum(pair.get(0), pair.get(1))))
        .filter(pair -> !Annotation.isUnitIndex(pair.get(2))) // x + y is not unit
        .filter(triple -> set.contains(triple.get(2)))
        .collect(Collectors.toList());
  }

  /**
   * ∀ x ≥ 1. log(x + 1) ≤ 1 + log(x)
   *
   * <p>Note that we exclude x = 2 and x = 1 because we need log(2) to express 1.
   *
   * @return Instantations as pairs. The left element corresponds to x, the right element
   *     corresponds to x + 1.
   */
  private static List<Pair<List<Integer>, List<Integer>>> lemmaPlus1(
      List<List<Integer>> potentialFunctions) {
    final var set = Set.copyOf(potentialFunctions);
    return potentialFunctions.stream()
        .filter(Predicate.not(Annotation::isUnitIndex)) // x is not unit
        .filter(x -> !x.equals(constantIndex(x.size() - 1, 1)))
        .map(x -> Pair.of(x, sum(x, constantIndex(x.size() - 1, 1))))
        .filter(instance -> set.contains(instance.getRight()))
        .collect(Collectors.toList());
  }

  /**
   * ∀ x ≥ 1. |t| = 1 ==> log(x + |t|) ≤ 1 + log(x)
   *
   * <p>Note that we exclude x = 2 because we need log(2) to express 1. However we do not need to
   * take special care about y.
   *
   * @return Instantiations as pairs. The left element corresponds to x, the right element
   *     corresponds to x + y.
   */
  @Deprecated
  private static List<Pair<List<Integer>, List<Integer>>> lemmaPlus1Known(
      List<List<Integer>> potentialFunctions, Set<Integer> knowOne) {
    // TODO: Carefully check whether this is correct. Looks buggy.
    final var set = Set.copyOf(potentialFunctions);
    final var result =
        potentialFunctions.stream()
            .filter(Predicate.not(Annotation::isUnitIndex)) // x is not unit
            .flatMap(
                x ->
                    knowOne.stream()
                        .filter(one -> x.get(one) == 1)
                        .map(one -> Pair.of(sum(x, atIndex(x.size() - 1, one, -1)), x)))
            .filter(instance -> set.contains(instance.getLeft()))
            .collect(Collectors.toList());
    return result;
  }

  private static List<Pair<List<Integer>, List<Integer>>> lemmaPlus2(
      List<List<Integer>> potentialFunctions) {
    final var set = Set.copyOf(potentialFunctions);
    return potentialFunctions.stream()
        .filter(Predicate.not(Annotation::isUnitIndex)) // x is not unit
        .map(x -> Pair.of(x, sum(x, constantIndex(x.size() - 1, 2))))
        .filter(instance -> set.contains(instance.getRight()))
        .collect(Collectors.toList());
  }

  private static List<Integer> sum(List<Integer> a, List<Integer> b) {
    return Streams.zip(a.stream(), b.stream(), Integer::sum).collect(Collectors.toList());
  }

  private static List<Integer> atIndex(int size, int index, int value) {
    return IntStream.rangeClosed(0, size).map(x -> x == index ? value : 0).boxed().toList();
  }

  private record Equal<T>(T left, T right) {
    @Override
    public String toString() {
      return left + " = " + right;
    }
  }

  private record LessThanOrEqual<T>(T smaller, T greater) {
    @Override
    public String toString() {
      return smaller + " <= " + greater;
    }
  }

  private record LessThan<T>(T smaller, T greater) {
    @Override
    public String toString() {
      return smaller + " < " + greater;
    }
  }

  public static BoolExpr encode(
      List<Integer> potentialFunction, List<ArithExpr> variables, Context ctx) {
    if (potentialFunction.size() != variables.size()) {
      throw new IllegalArgumentException();
    }
    return ctx.mkAnd(
        Streams.zip(potentialFunction.stream().map(ctx::mkInt), variables.stream(), ctx::mkEq)
            .toArray(BoolExpr[]::new));
  }

  public static List<LessThanOrEqual<List<Integer>>> monotony(
      List<List<Integer>> potentialFunctions,
      Set<LessThan<Integer>> knowLt,
      Set<Equal<Integer>> knowEq,
      Set<Integer> knowOne) {
    if (potentialFunctions.isEmpty()) {
      return emptyList();
    }
    if (knowLt.isEmpty() && knowEq.isEmpty() && knowOne.isEmpty()) {
      synchronized (MONO_CACHE) {
        return MONO_CACHE.computeIfAbsent(
            potentialFunctions,
            (key) -> monotonyInternal(potentialFunctions, emptySet(), emptySet(), emptySet()));
      }
    }
    return monotonyInternal(potentialFunctions, knowLt, knowEq, knowOne);
  }

  public static List<LessThanOrEqual<List<Integer>>> monotonyInternal(
      List<List<Integer>> potentialFunctions,
      Set<LessThan<Integer>> knowLt,
      Set<Equal<Integer>> knowEq,
      Set<Integer> knowOne) {
    if (potentialFunctions.isEmpty()) {
      return emptyList();
    }

    final var treeSize = potentialFunctions.get(0).size() - 1;

    final var prod =
        cartesianProduct(potentialFunctions, potentialFunctions).stream()
            .filter(comparison -> !comparison.get(0).equals(comparison.get(1)))
            .toList();

    if (treeSize == 0) {
      return prod.stream()
          .filter(comparison -> comparison.get(0).get(0) <= comparison.get(1).get(0))
          .map(comparison -> new LessThanOrEqual<>(comparison.get(0), comparison.get(1)))
          .collect(toList());
    }

    try (final var ctx = new Context()) {
      final var solver = ctx.mkSolver();
      solver.push();
      final IntExpr one = ctx.mkInt(1);
      final List<IntExpr> vars =
          IntStream.range(0, treeSize).mapToObj(i -> ctx.mkIntConst("x" + i)).toList();

      for (var x : vars) {
        solver.add(ctx.mkLe(one, x));
      }
      for (var pair : knowLt) {
        solver.add(ctx.mkLt(vars.get(pair.smaller), vars.get(pair.greater)));
      }
      for (var pair : knowEq) {
        solver.add(ctx.mkEq(vars.get(pair.left), vars.get(pair.right)));
      }
      for (var index : knowOne) {
        solver.add(ctx.mkEq(vars.get(index), one));
      }

      final var result = new ArrayList<LessThanOrEqual<List<Integer>>>();

      for (var comparison : prod) {
        final var smaller = comparison.get(0);
        final var bigger = comparison.get(1);

        solver.push();

        solver.add(
            ctx.mkGt(
                ctx.mkAdd(
                    Streams.concat(
                            Streams.zip(
                                smaller.stream().map(ctx::mkInt),
                                vars.stream(),
                                (a, b) -> ctx.mkMul(a, b)),
                            Stream.of(ctx.mkInt(smaller.get(treeSize))))
                        .toArray(ArithExpr[]::new)),
                ctx.mkAdd(
                    Streams.concat(
                            Streams.zip(
                                bigger.stream().map(ctx::mkInt),
                                vars.stream(),
                                (a, b) -> ctx.mkMul(a, b)),
                            Stream.of(ctx.mkInt(bigger.get(treeSize))))
                        .toArray(ArithExpr[]::new))));

        final var status = solver.check();
        if (UNKNOWN.equals(status)) {
          throw bug("Z3 returned unknown status for monotonicity analysis");
        }

        if (status.equals(UNSATISFIABLE)) {
          result.add(new LessThanOrEqual<>(smaller, bigger));
        }

        solver.pop();
      }
      return result;
    }
  }
}
