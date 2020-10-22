package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import com.google.common.collect.Streams;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntNum;
import com.microsoft.z3.Status;
import lombok.AllArgsConstructor;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.Graph;
import org.jgrapht.GraphPath;
import org.jgrapht.alg.shortestpath.AllDirectedPaths;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.google.common.collect.Lists.cartesianProduct;
import static com.google.common.collect.Streams.concat;
import static com.microsoft.z3.Status.UNKNOWN;
import static com.microsoft.z3.Status.UNSATISFIABLE;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toUnmodifiableList;
import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.INDEX_COMPARATOR;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.constantIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.nonRankIndices;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.MINUS_ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.MINUS_TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient.maybeNegative;
import static xyz.leutgeb.lorenz.lac.util.Util.append;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

@Slf4j
public class W implements Rule {
  public static final W INSTANCE = new W();

  private static final boolean LEMMA_2XY_ENABLED = true;
  private static final boolean LEMMA_PLUS1_ENABLED = false;
  private static final boolean LEMMA_PLUS1Y_ENABLED = false;
  private static final boolean LEMMA_PLUS2_ENABLED = false;
  private static final boolean MONO_ONE_ENABLED = false;

  private static final Map<List<List<Integer>>, List<LessThanOrEqual<List<Integer>>>> MONO_CACHE =
      new HashMap<>();

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

  public static List<Constraint> compareCoefficientsLessOrEqualUsingFarkas(
      List<Identifier> identifiers,
      Annotation left,
      Annotation right,
      Graph<Identifier, SizeEdge> sizeAnalysis) {
    // left is P in paper.
    // right is Q in paper.

    final var reducedSizeAnalysis = reduceSizeAnalysis(identifiers, sizeAnalysis);
    Set<LessThan<Integer>> knowLt = reducedSizeAnalysis.knowLt;
    Set<Equal<Integer>> knowEq = reducedSizeAnalysis.knowEq;
    Set<Integer> knowOne = reducedSizeAnalysis.knowOne;

    final List<Constraint> constraints = new ArrayList<>();
    // TODO(lorenz.leutgeb): Exploit |t| > |t'| ==> rk(t) > rk(t')
    IntStream.range(0, left.size())
        .mapToObj(
            i ->
                new LessThanOrEqualConstraint(
                    left.getRankCoefficientOrZero(i),
                    right.getRankCoefficientOrZero(i),
                    "(w) rk(" + identifiers.get(i) + ") (at index " + i + ")"))
        .forEach(constraints::add);

    final var potentialFunctions = Annotation.nonRankIndices(left, right).collect(toList());

    final List<LessThanOrEqual<List<Integer>>> monotonyInstances =
        knowLt.isEmpty() && knowEq.isEmpty() && knowOne.isEmpty()
            ? MONO_CACHE.computeIfAbsent(
                potentialFunctions,
                (key) ->
                    cartesianProduct(potentialFunctions, potentialFunctions).stream()
                        .filter(
                            pair ->
                                lessThanOrEqual(
                                    pair.get(0),
                                    pair.get(1),
                                    knowLt,
                                    knowEq,
                                    MONO_ONE_ENABLED ? knowOne : emptySet()))
                        .map(pair -> new LessThanOrEqual<>(pair.get(0), pair.get(1)))
                        .collect(toList()))
            : cartesianProduct(potentialFunctions, potentialFunctions).stream()
                .filter(
                    pair ->
                        lessThanOrEqual(
                            pair.get(0),
                            pair.get(1),
                            knowLt,
                            knowEq,
                            MONO_ONE_ENABLED ? knowOne : emptySet()))
                .map(pair -> new LessThanOrEqual<>(pair.get(0), pair.get(1)))
                .collect(toList());

    final List<List<List<Integer>>> lemma2XYInstances =
        LEMMA_2XY_ENABLED ? lemma2XY(potentialFunctions) : emptyList();

    final List<Pair<List<Integer>, List<Integer>>> lemmaPlus1Instances =
        LEMMA_PLUS1_ENABLED
            ? append(
                lemmaPlus1(potentialFunctions),
                LEMMA_PLUS1Y_ENABLED ? lemmaPlus1Known(potentialFunctions, knowOne) : emptyList())
            : emptyList();

    final List<Pair<List<Integer>, List<Integer>>> lemmaPlus2Instances =
        LEMMA_PLUS2_ENABLED ? lemmaPlus2(potentialFunctions) : emptyList();

    // TODO(lorenz.leutgeb): Make this a log.trace(...);

    // List<String> identifierNames = identifiers.stream().map(Object::toString).collect(toList());
    /*
    System.out.println("(w) --- " + left.getId() + " <= " + right.getId() + " --- ");
    System.out.println("ids: " + identifiers);
    System.out.println(
        "lts: " + knowLt.stream().map(x -> x.map(identifiers::get)).collect(toUnmodifiableList()));
    System.out.println(
        "eqs: " + knowEq.stream().map(x -> x.map(identifiers::get)).collect(toUnmodifiableList()));
    System.out.println(
        "one: " + knowOne.stream().map(identifiers::get).collect(toUnmodifiableList()));

    System.out.println("monotony:");
    monotonyInstances.forEach(System.out::println);

    System.out.println("lemma2XY:");
    lemma2XYInstances.forEach(
        instance -> {
          System.out.println(
              "2 * [...0, 2] + "
                  + instance.get(0)
                  + " + "
                  + instance.get(1)
                  + " <= 2 * "
                  + instance.get(2));
        });

    System.out.println("lemma1Plus:");
    lemmaPlus1Instances.forEach(
        instance -> {
          System.out.println(instance.getRight() + " <= 1 * [...0, 2] + " + instance.getLeft());
        });

    System.out.println("lemma2Plus:");
    lemmaPlus2Instances.forEach(
        instance -> {
          System.out.println(instance.getRight() + " <= 2 * [...0, 2] + " + instance.getLeft());
        });

    System.out.println(" --- ");
     */

    // m is the number of rows of expert knowledge.
    final var m =
        monotonyInstances.size()
            + lemma2XYInstances.size()
            + lemmaPlus1Instances.size()
            + lemmaPlus2Instances.size();

    if (m == 0) {
      // If we have no expert knowledge, fall back to comparing coefficients.
      return compareCoefficientsLessOrEqual(left, right, "fallback");
    }

    final var wid = randomHex();

    final var p = potentialFunctions.stream().map(left::getCoefficientOrZero).collect(toList());
    final var q = potentialFunctions.stream().map(right::getCoefficientOrZero).collect(toList());

    // NOTE: We do not add constraints saying f ≥ 0. This is generated for all unknown coefficients!
    final var f =
        IntStream.range(0, m)
            .mapToObj(i -> new UnknownCoefficient(wid + ".f[" + i + "]"))
            .collect(toList());

    final List<List<Coefficient>> sumByColumn = new ArrayList<>(potentialFunctions.size());
    for (int column = 0; column < potentialFunctions.size(); column++) {
      final var tmp = new ArrayList<Coefficient>(3);
      tmp.add(q.get(column));
      sumByColumn.add(tmp);
    }

    if (!monotonyInstances.isEmpty()) {
      for (int column = 0; column < potentialFunctions.size(); column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < monotonyInstances.size(); row++) {
          final var knowledgeRow = monotonyInstances.get(row);
          if (potentialFunction.equals(knowledgeRow.smaller)) {
            sum.add(f.get(row));
          } else if (potentialFunction.equals(knowledgeRow.larger)) {
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
            UnknownCoefficient prod = maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_TWO, fi), "(w:l17) " + prod + " = (-2) * " + fi));
            sum.add(prod);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            UnknownCoefficient prod = maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(TWO, fi), "(w) lxy2  const: " + prod + " = (2) * " + fi));
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
            final var prod = maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp1 log(x): " + prod + " = (-1) * " + fi));
            sum.add(prod);
          } else if (potentialFunction.equals(knowledgeRow.getRight())) {
            // fi corresponds to log(x + 1)
            sum.add(fi);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            final var prod = maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
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
            final var prod = maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp2 log(x): " + prod + " = (-1) * " + fi));
            sum.add(prod);
          } else if (potentialFunction.equals(knowledgeRow.getRight())) {
            // fi corresponds to log(x + 2)
            sum.add(fi);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            final var prod = maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_TWO, fi), "(w) lp2 const: " + prod + " = (-2) * " + fi));
            sum.add(prod);
          }
        }
      }
    }

    // p ≤ fA + q (Note: fA is computed using matrix multiplication WITH f FROM THE LEFT.)
    for (int i = 0; i < potentialFunctions.size(); i++) {
      final var x = maybeNegative(wid + ".sum[" + i + "]");
      constraints.add(
          new EqualsSumConstraint(
              x,
              sumByColumn.get(i),
              "(w "
                  + left.getId()
                  + " ≤ "
                  + right.getId()
                  + ") "
                  + x
                  + " = Σ... + q["
                  + i
                  + "] (farkas)"));
      constraints.add(
          new LessThanOrEqualConstraint(
              p.get(i),
              x,
              "(w "
                  + left.getId()
                  + " ≤ "
                  + right.getId()
                  + ") "
                  + wid
                  + ".p["
                  + i
                  + "] ≤ "
                  + x
                  + " (farkas)"));
    }

    return constraints;
  }

  @Value
  private static class ReducedSizeAnalysis {
    public Set<LessThan<Integer>> knowLt;
    public Set<Equal<Integer>> knowEq;
    public Set<Integer> knowOne;
  }

  private static ReducedSizeAnalysis reduceSizeAnalysis(
      List<Identifier> identifiers, Graph<Identifier, SizeEdge> sizeAnalysis) {
    Set<LessThan<Integer>> knowLt = new HashSet<>();
    Set<Equal<Integer>> knowEq = new HashSet<>();

    final Predicate<GraphPath<Identifier, SizeEdge>> isGt =
        path ->
            path.getEdgeList().stream().anyMatch(edge -> SizeEdge.Kind.GT.equals(edge.getKind()));

    final Predicate<GraphPath<Identifier, SizeEdge>> isEq =
        path ->
            path.getEdgeList().stream().allMatch(edge -> SizeEdge.Kind.EQ.equals(edge.getKind()));

    final Set<Integer> knowOne =
        sizeAnalysis.containsVertex(LEAF)
            ? identifiers.stream()
                .filter(sizeAnalysis::containsVertex)
                .filter(
                    identifier -> {
                      final var leafPaths =
                          new AllDirectedPaths<>(sizeAnalysis)
                              .getAllPaths(identifier, LEAF, true, 16);
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

    if (!knowLt.isEmpty() || !knowEq.isEmpty()) {
      log.trace(
          "Size analysis useful ({}, {})!",
          knowLt.stream().map(x -> x.map(identifiers::get)).collect(toUnmodifiableList()),
          knowEq.stream().map(x -> x.map(identifiers::get)).collect(toUnmodifiableList()));
    }

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
  private static List<Pair<List<Integer>, List<Integer>>> lemmaPlus1Known(
      List<List<Integer>> potentialFunctions, Set<Integer> knowOne) {
    final var set = Set.copyOf(potentialFunctions);
    return potentialFunctions.stream()
        .filter(Predicate.not(Annotation::isUnitIndex)) // x is not unit
        .flatMap(
            x -> knowOne.stream().map(one -> Pair.of(x, sum(x, oneAtIndex(x.size() - 1, one)))))
        .filter(instance -> set.contains(instance.getRight()))
        .collect(Collectors.toList());
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

  private static List<Integer> oneAtIndex(int size, int index) {
    return IntStream.rangeClosed(0, size)
        .map(x -> x == index ? 1 : 0)
        .boxed()
        .collect(toUnmodifiableList());
  }

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var q = obligation.getContext().getAnnotation();
    final var qp = obligation.getAnnotation();

    final var p = globals.getHeuristic().generate("weaken p", q);
    final var pp = globals.getHeuristic().generate("weaken p'", qp);

    return new Rule.ApplicationResult(
        singletonList(
            obligation.keepCost(
                new AnnotatingContext(obligation.getContext().getIds(), p),
                obligation.getExpression(),
                pp)),
        singletonList(
            append(
                /*
                obligation.getContext().getIds().isEmpty()
                    ? compareCoefficientsLessOrEqual(p, q)
                    : */ compareCoefficientsLessOrEqualUsingFarkas(
                    obligation.getContext().getIds(), p, q, globals.getSizeAnalysis()),
                /*
                compareCoefficientsLessOrEqualUsingFarkas(
                    qp.size() > 0 ? singletonList(Identifier.DUMMY_TREE_ALPHA) : emptyList(),
                    qp,
                    pp,
                    globals.getSizeAnalysis()) */
                compareCoefficientsLessOrEqual(qp, pp, "Q'"))),
        emptyList());
  }

  @Value
  @AllArgsConstructor
  public static class Equal<T> {
    public T left;
    public T right;

    @Override
    public String toString() {
      return left + " = " + right;
    }

    public <U> Equal<U> map(Function<T, U> f) {
      return new Equal<>(f.apply(left), f.apply(right));
    }
  }

  @Value
  @AllArgsConstructor
  public static class LessThanOrEqual<T> {
    public T smaller;
    public T larger;

    @Override
    public String toString() {
      return smaller + " <= " + larger;
    }

    public <U> LessThanOrEqual<U> map(Function<T, U> f) {
      return new LessThanOrEqual<>(f.apply(smaller), f.apply(larger));
    }
  }

  @Value
  @AllArgsConstructor
  public static class LessThan<T> {
    public T smaller;
    public T larger;

    @Override
    public String toString() {
      return smaller + " < " + larger;
    }

    public <U> LessThan<U> map(Function<T, U> f) {
      return new LessThan<>(f.apply(smaller), f.apply(larger));
    }
  }

  @Deprecated
  public static List<LessThanOrEqual<List<Integer>>> lessThanOrEqual(
      List<List<Integer>> potentialFunctions,
      Set<LessThan<Integer>> expertLt,
      Set<Equal<Integer>> expertEq) {
    if (potentialFunctions.isEmpty()) {
      return emptyList();
    }

    // p1 and p2 represent linear combinations:
    //
    //     p1  =  a1.1 * x1 + a1.2 * x2 + a1.3 * x3 + ... + a1.n * xn + b1
    //     p2  =  a2.1 * x1 + a2.2 * x2 + a2.3 * x3 + ... + a2.n * xn + b2
    //
    // expertLt represents additional knowledge about relations between xs,
    // it might contain:
    //
    //     x1 < x2
    //     x4 < x3
    //
    // We now must decide whether p1 <= p2. This is done by setting up a
    // SMT instance that tries to find a solution such that p1 > p2 given
    // expertLt. If this is not possible, we return true.

    // NOTE: We decrement to get the "size" that we also use for annotations.
    final var size = potentialFunctions.get(0).size() - 1;

    if (size == 0) {
      return cartesianProduct(potentialFunctions, potentialFunctions).stream()
          .filter(
              pair -> {
                final var l = pair.get(0).get(0);
                final var r = pair.get(1).get(0);
                return l >= 1 && r >= 1 && l <= r;
              })
          .map(pair -> new LessThanOrEqual<>(pair.get(0), pair.get(1)))
          .collect(toUnmodifiableList());
    }

    try (final var ctx = new Context()) {
      final ArithExpr one = ctx.mkInt(1);
      final var solver = ctx.mkSolver();
      final ArithExpr depthBound = ctx.mkInt(16);

      final List<ArithExpr> ls =
          IntStream.rangeClosed(0, size)
              .mapToObj(i -> ctx.mkIntConst("l" + i))
              .collect(Collectors.toUnmodifiableList());

      final List<ArithExpr> rs =
          IntStream.rangeClosed(0, size)
              .mapToObj(i -> ctx.mkIntConst("r" + i))
              .collect(Collectors.toUnmodifiableList());

      solver.add(encodeAll(potentialFunctions, ls, ctx));
      solver.add(encodeAll(potentialFunctions, rs, ctx));

      solver.add(
          ctx.mkNot(
              ctx.mkAnd(
                  Streams.zip(ls.stream(), rs.stream(), ctx::mkEq).toArray(BoolExpr[]::new))));

      final List<ArithExpr> xs =
          IntStream.range(0, size)
              .mapToObj(i -> ctx.mkIntConst("x" + i))
              .collect(Collectors.toUnmodifiableList());

      final var precondition =
          ctx.mkAnd(
              Stream.concat(
                      xs.stream().map(x -> ctx.mkAnd(ctx.mkLe(one, x), ctx.mkLe(x, depthBound))),
                      Stream.concat(
                          expertLt.stream()
                              .map(pair -> ctx.mkLt(xs.get(pair.smaller), xs.get(pair.larger))),
                          expertEq.stream()
                              .map(pair -> ctx.mkEq(xs.get(pair.left), xs.get(pair.right)))))
                  .toArray(BoolExpr[]::new));

      final var main =
          ctx.mkGe(
              ctx.mkAdd(
                  Streams.concat(
                          Streams.zip(ls.stream(), xs.stream(), ctx::mkMul),
                          Stream.of(ls.get(ls.size() - 1)))
                      .toArray(ArithExpr[]::new)),
              ctx.mkAdd(
                  Streams.concat(
                          Streams.zip(rs.stream(), xs.stream(), ctx::mkMul),
                          Stream.of(rs.get(rs.size() - 1)))
                      .toArray(ArithExpr[]::new)));

      solver.add(
          ctx.mkNot(
              ctx.mkExists(
                  xs.stream().toArray(Expr[]::new),
                  ctx.mkAnd(precondition, main),
                  1,
                  null,
                  null,
                  null,
                  null)));

      solver.push();

      final List<LessThanOrEqual<List<Integer>>> result = new ArrayList<>();
      while (true) {
        final var checked = solver.check();
        if (checked.equals(UNSATISFIABLE)) {
          return result;
        }
        if (!checked.equals(Status.SATISFIABLE)) {
          throw bug("unexepected status: " + checked);
        }
        final var model = solver.getModel();
        final List<Integer> leftSolution =
            ls.stream()
                .map(model::getConstInterp)
                .map(x -> (IntNum) x)
                .map(IntNum::getInt)
                .collect(Collectors.toUnmodifiableList());

        final List<Integer> rightSolution =
            rs.stream()
                .map(model::getConstInterp)
                .map(x -> (IntNum) x)
                .map(IntNum::getInt)
                .collect(Collectors.toUnmodifiableList());

        result.add(new LessThanOrEqual<>(leftSolution, rightSolution));

        solver.add(
            ctx.mkNot(
                ctx.mkAnd(
                    Streams.concat(
                            Streams.zip(
                                ls.stream(), leftSolution.stream().map(ctx::mkInt), ctx::mkEq),
                            Streams.zip(
                                rs.stream(), rightSolution.stream().map(ctx::mkInt), ctx::mkEq))
                        .toArray(BoolExpr[]::new))));
      }
    }
  }

  public static BoolExpr encodeAll(
      List<List<Integer>> potentialFunctions, List<ArithExpr> variables, Context ctx) {
    return ctx.mkOr(
        potentialFunctions.stream()
            .map(potentialFunction -> encode(potentialFunction, variables, ctx))
            .toArray(BoolExpr[]::new));
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

  public static boolean lessThanOrEqual(
      List<Integer> smaller,
      List<Integer> bigger,
      Set<LessThan<Integer>> knowLt,
      Set<Equal<Integer>> knowEq,
      Set<Integer> knowOne) {
    // smaller and bigger represent linear combinations:
    //
    //     smaller  =  a1.1 * x1 + a1.2 * x2 + a1.3 * x3 + ... + a1.n * xn + b1
    //     bigger  =  a2.1 * x1 + a2.2 * x2 + a2.3 * x3 + ... + a2.n * xn + b2
    //
    // knowLt represents additional knowledge about relations between xs,
    // it might contain:
    //
    //     x1 < x2
    //     x4 < x3
    //
    // We now must decide whether smaller <= bigger. This is done by setting up a
    // SMT instance that tries to find a solution such that smaller > bigger given
    // knowLt. If this is not possible, we return true.

    // We do not explicate reflexivity.
    if (smaller.equals(bigger)) {
      return false;
    }

    if (smaller.size() != bigger.size()) {
      throw bug();
    }

    // NOTE: We decrement to get the "treeSize" that we also use for annotations.
    final var treeSize = smaller.size() - 1;

    if (treeSize == 0) {
      return smaller.get(0) <= bigger.get(0);
    }

    try (final var ctx = new Context()) {
      final var solver = ctx.mkSolver();
      final ArithExpr one = ctx.mkInt(1);
      final List<ArithExpr> vars =
          IntStream.range(0, treeSize)
              .mapToObj(i -> ctx.mkIntConst("x" + i))
              .collect(Collectors.toUnmodifiableList());

      for (var x : vars) {
        solver.add(ctx.mkLe(one, x));
      }
      for (var pair : knowLt) {
        solver.add(ctx.mkLt(vars.get(pair.smaller), vars.get(pair.larger)));
      }
      for (var pair : knowEq) {
        solver.add(ctx.mkEq(vars.get(pair.left), vars.get(pair.right)));
      }
      for (var index : knowOne) {
        solver.add(ctx.mkEq(vars.get(index), one));
      }

      solver.add(
          ctx.mkGt(
              ctx.mkAdd(
                  Streams.concat(
                          Streams.zip(smaller.stream().map(ctx::mkInt), vars.stream(), ctx::mkMul),
                          Stream.of(ctx.mkInt(smaller.get(treeSize))))
                      .toArray(ArithExpr[]::new)),
              ctx.mkAdd(
                  Streams.concat(
                          Streams.zip(bigger.stream().map(ctx::mkInt), vars.stream(), ctx::mkMul),
                          Stream.of(ctx.mkInt(bigger.get(treeSize))))
                      .toArray(ArithExpr[]::new))));

      final var status = solver.check();
      if (UNKNOWN.equals(status)) {
        throw bug("Z3 returned unknown status for monotonicity analysis");
      }
      return status.equals(UNSATISFIABLE);
    }
  }

  @Override
  public String getName() {
    return "w";
  }
}
