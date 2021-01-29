package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Lists.cartesianProduct;
import static com.google.common.collect.Streams.concat;
import static com.microsoft.z3.Status.UNKNOWN;
import static com.microsoft.z3.Status.UNSATISFIABLE;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toUnmodifiableList;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.INDEX_COMPARATOR;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.constantIndex;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.nonRankIndices;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.MINUS_ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.MINUS_TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient.maybeNegative;
import static xyz.leutgeb.lorenz.lac.util.Util.append;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.flag;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

import com.google.common.collect.Streams;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
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

@Slf4j
public class W implements Rule {
  public static final W INSTANCE = new W();

  private static final boolean FARKAS_ENABLED = false;

  private static final boolean LEMMA_2XY_ENABLED = false; // was true
  private static final boolean LEMMA_PLUS1_ENABLED = false;
  private static final boolean LEMMA_PLUS1Y_ENABLED = false;
  private static final boolean LEMMA_PLUS2_ENABLED = false;
  private static final boolean MONO_ONE_ENABLED = false; // was true

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
      Graph<Identifier, SizeEdge> sizeAnalysis,
      Map<String, String> arguments) {
    return compareCoefficientsLessOrEqualUsingFarkas(
        identifiers, left, right, sizeAnalysis, arguments, false);
  }

  public static List<Constraint> compareCoefficientsLessOrEqualUsingFarkas(
      List<Identifier> identifiers,
      Annotation left,
      Annotation right,
      Graph<Identifier, SizeEdge> sizeAnalysis,
      Map<String, String> arguments,
      boolean force) {
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

    final boolean mono = flag(arguments, "mono") || force;

    final List<LessThanOrEqual<List<Integer>>> monotonyInstances;

    if (knowLt.isEmpty() && knowEq.isEmpty() && knowOne.isEmpty()) {
      synchronized (MONO_CACHE) {
        monotonyInstances =
            MONO_CACHE.computeIfAbsent(
                potentialFunctions,
                (key) ->
                    lessThanOrEqualNew(
                        potentialFunctions, knowLt, knowEq, mono ? knowOne : emptySet()));
      }
    } else {
      monotonyInstances =
          lessThanOrEqualNew(potentialFunctions, knowLt, knowEq, mono ? knowOne : emptySet());
    }

    final var lemma2xy = flag(arguments, "l2xy") || force;

    final List<List<List<Integer>>> lemma2XYInstances =
        lemma2xy ? lemma2XY(potentialFunctions) : emptyList();

    final var lemmap1 = flag(arguments, "lp1");
    final var lemmap1y = flag(arguments, "lp1y");

    final List<Pair<List<Integer>, List<Integer>>> lemmaPlus1Instances =
        append(
            lemmap1 ? lemmaPlus1(potentialFunctions) : emptyList(),
            lemmap1y ? lemmaPlus1Known(potentialFunctions, knowOne) : emptyList());

    final var lemmap2 = flag(arguments, "lp2");
    final List<Pair<List<Integer>, List<Integer>>> lemmaPlus2Instances =
        lemmap2 ? lemmaPlus2(potentialFunctions) : emptyList();

    // List<String> identifierNames = identifiers.stream().map(Object::toString).collect(toList());
    log.trace("(w) --- " + left.getId() + " <= " + right.getId() + " --- ");
    log.trace("pot: " + potentialFunctions);
    log.trace("ids: " + identifiers);
    log.trace(
        "lts: " + knowLt.stream().map(x -> x.map(identifiers::get)).collect(toUnmodifiableList()));
    log.trace(
        "eqs: " + knowEq.stream().map(x -> x.map(identifiers::get)).collect(toUnmodifiableList()));
    log.trace("one: " + knowOne.stream().map(identifiers::get).collect(toUnmodifiableList()));

    log.trace("monotony:");
    monotonyInstances.forEach(x -> log.trace("{}", x));

    log.trace("lemma2XY:");
    lemma2XYInstances.forEach(
        instance -> {
          log.trace(
              "2 * [...0, 2] + "
                  + instance.get(0)
                  + " + "
                  + instance.get(1)
                  + " <= 2 * "
                  + instance.get(2));
        });

    log.trace("lemma1Plus:");
    lemmaPlus1Instances.forEach(
        instance -> {
          log.trace(instance.getRight() + " <= 1 * [...0, 2] + " + instance.getLeft());
        });

    log.trace("lemma2Plus:");
    lemmaPlus2Instances.forEach(
        instance -> {
          log.trace(instance.getRight() + " <= 2 * [...0, 2] + " + instance.getLeft());
        });

    log.trace(" --- ");

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
            UnknownCoefficient prod = maybeNegative(wid + ".lxy2.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_TWO, fi), "(w) lxy2" + prod + " = (-2) * " + fi));
            sum.add(prod);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            UnknownCoefficient prod = maybeNegative(wid + ".lxy2.prod[" + column + "," + row + "]");
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
            final var prod = maybeNegative(wid + ".lp1.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp1 log(x): " + prod + " = (-1) * " + fi));
            sum.add(prod);
          } else if (potentialFunction.equals(knowledgeRow.getRight())) {
            // fi corresponds to log(x + 1)
            sum.add(fi);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            final var prod = maybeNegative(wid + ".lp1.prod[" + column + "," + row + "]");
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
            final var prod = maybeNegative(wid + ".lp2.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_ONE, fi), "(w) lp2 log(x): " + prod + " = (-1) * " + fi));
            sum.add(prod);
          } else if (potentialFunction.equals(knowledgeRow.getRight())) {
            // fi corresponds to log(x + 2)
            sum.add(fi);
          } else if (Annotation.isUnitIndex(potentialFunction)) {
            // fi corresponds to log(2) = 1
            final var prod = maybeNegative(wid + ".lp2.prod[" + column + "," + row + "]");
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
        sizeAnalysis.containsVertex(Identifier.leaf())
            ? identifiers.stream()
                .filter(sizeAnalysis::containsVertex)
                .filter(
                    identifier -> {
                      final var leafPaths =
                          new AllDirectedPaths<>(sizeAnalysis)
                              .getAllPaths(identifier, Identifier.leaf(), true, 16);
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
            x ->
                knowOne.stream()
                    .filter(one -> x.get(one) == 1)
                    .map(one -> Pair.of(sum(x, atIndex(x.size() - 1, one, -1)), x)))
        .filter(instance -> set.contains(instance.getLeft()))
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
    return atIndex(size, index, 1);
  }

  private static List<Integer> atIndex(int size, int index, int value) {
    return IntStream.rangeClosed(0, size)
        .map(x -> x == index ? value : 0)
        .boxed()
        .collect(toUnmodifiableList());
  }

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var q = obligation.getContext().getAnnotation();
    final var qp = obligation.getAnnotation();

    final var p = globals.getHeuristic().generate("weaken p", q);
    final var pp = globals.getHeuristic().generate("weaken p'", qp);

    final var force = false;

    final var farkas = flag(arguments, "mono") || flag(arguments, "l2xy") || force;

    // arguments = Map.of("mono", "true", "l2xy", "true");

    return new Rule.ApplicationResult(
        singletonList(
            obligation.keepCost(
                new AnnotatingContext(obligation.getContext().getIds(), p),
                obligation.getExpression(),
                pp)),
        singletonList(
            append(
                farkas
                    ? compareCoefficientsLessOrEqualUsingFarkas(
                        obligation.getContext().getIds(),
                        p,
                        q,
                        globals.getSizeAnalysis(),
                        arguments,
                        force)
                    : compareCoefficientsLessOrEqual(p, q, "Q"),
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

  public static List<LessThanOrEqual<List<Integer>>> lessThanOrEqualNew(
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
            .collect(toUnmodifiableList());

    if (treeSize == 0) {
      return prod.stream()
          .filter(comparison -> comparison.get(0).get(0) <= comparison.get(1).get(0))
          .map(comparison -> new LessThanOrEqual<>(comparison.get(0), comparison.get(1)))
          .collect(toList());
    }

    try (final var ctx = new Context()) {
      final var solver = ctx.mkSolver();
      solver.push();
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
                                smaller.stream().map(ctx::mkInt), vars.stream(), ctx::mkMul),
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

        if (status.equals(UNSATISFIABLE)) {
          result.add(new LessThanOrEqual<>(smaller, bigger));
        }

        solver.pop();
      }
      return result;
    }
  }

  @Override
  public String getName() {
    return "w";
  }
}
