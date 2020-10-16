package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Lists.cartesianProduct;
import static com.google.common.collect.Streams.concat;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.INDEX_COMPARATOR;
import static xyz.leutgeb.lorenz.lac.typing.resources.Annotation.nonRankIndices;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.MINUS_TWO;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.TWO;
import static xyz.leutgeb.lorenz.lac.util.Util.append;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.randomHex;

import com.google.common.collect.Streams;
import com.microsoft.z3.ArithExpr;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Context;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntNum;
import com.microsoft.z3.Status;
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
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.sources.Predefined;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsProductConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.GreaterThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;
import xyz.leutgeb.lorenz.lac.util.Util;

@Slf4j
public class W implements Rule {
  public static final W INSTANCE = new W();

  private static final Map<List<List<Integer>>, List<Order<List<Integer>>>> MONO_CACHE =
      new HashMap<>();

  private static List<Constraint> compareCoefficientsGreaterOrEqual(
      Annotation left, Annotation right) {
    return compareCoefficients(
        left,
        right,
        (x, y) -> new GreaterThanOrEqualConstraint(x, y, "(w) geqSimple " + x + " >= " + y));
  }

  public static List<Constraint> compareCoefficientsLessOrEqual(Annotation left, Annotation right) {
    return compareCoefficients(
        left,
        right,
        (x, y) -> new LessThanOrEqualConstraint(x, y, "(w) leqSimple " + x + " <= " + y));
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
    // left is p in paper.
    // right is q in paper.

    final Predicate<GraphPath<Identifier, SizeEdge>> isGt =
        path ->
            path.getEdgeList().stream().anyMatch(edge -> SizeEdge.Kind.GT.equals(edge.getKind()));

    final Predicate<GraphPath<Identifier, SizeEdge>> isEq =
        path ->
            path.getEdgeList().stream().allMatch(edge -> SizeEdge.Kind.EQ.equals(edge.getKind()));

    Set<Order<Integer>> knowGt = new HashSet<>();
    Set<Order<Integer>> knowEq = new HashSet<>();
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
                knowGt.add(
                    new Order<>(
                        identifiers.indexOf(pathGt.get().getStartVertex()),
                        identifiers.indexOf(pathGt.get().getEndVertex())));
                return;
              }
              final var pathEq = paths.stream().filter(isEq).findFirst();
              if (pathEq.isPresent()) {
                knowEq.add(
                    new Order<>(
                        identifiers.indexOf(pathEq.get().getStartVertex()),
                        identifiers.indexOf(pathEq.get().getEndVertex())));
              }
            });

    if (!knowGt.isEmpty() || !knowEq.isEmpty()) {
      log.info("Size analysis useful ({})!", identifiers);
    }

    final List<Constraint> constraints = new ArrayList<>();
    // TODO(lorenz.leutgeb): What about rank coefficients?
    IntStream.range(0, left.size())
        .mapToObj(
            i ->
                new LessThanOrEqualConstraint(
                    left.getRankCoefficientOrZero(i),
                    right.getRankCoefficientOrZero(i),
                    "(w) rk(" + identifiers.get(i) + ") (at index " + i + ")"))
        .forEach(constraints::add);

    final var potentialFunctions = Annotation.nonRankIndices(left, right).collect(toList());

    final var n = potentialFunctions.size();

    // TODO(lorenz.leutgeb): This is a little inefficient: We compute the full cartesian product,
    // and
    // then filter out most pairs. It would be much better to only generate matching
    // pairs in the first place.
    // final var monoStamp = DateTime.now();
    // final List<Order<List<Integer>>> monoKnowledge = emptyList();
    final List<Order<List<Integer>>> monoKnowledge =
        knowGt.isEmpty()
            ? MONO_CACHE.computeIfAbsent(
                potentialFunctions,
                (key) ->
                    cartesianProduct(potentialFunctions, potentialFunctions).stream()
                        .filter(pair -> lessThanOrEqual(pair.get(0), pair.get(1), knowGt))
                        .filter(pair -> !pair.get(0).equals(pair.get(1)))
                        .map(pair -> new Order<>(pair.get(0), pair.get(1)))
                        .collect(toList()))
            : lessThanOrEqual(potentialFunctions, emptySet());

    final var lemmaKnowledge = lemma17(potentialFunctions);
    // final List<List<List<Integer>>> lemmaKnowledge = emptyList();

    // m is the number of rows of expert knowledge.
    final var m = monoKnowledge.size() + lemmaKnowledge.size();
    // log.info("m = {}", m);

    if (m == 0) {
      // throw bug("cannot apply (w) with empty expert knowledge!");
      return compareCoefficientsLessOrEqual(left, right);
    }

    final var wid = randomHex();

    final var p = potentialFunctions.stream().map(left::getCoefficientOrZero).collect(toList());
    final var q = potentialFunctions.stream().map(right::getCoefficientOrZero).collect(toList());

    // Note: We do not add constraints saying f ≥ 0. This is generated for all unknown coefficients!
    final var f =
        IntStream.range(0, m)
            .mapToObj(i -> new UnknownCoefficient(wid + ".f[" + i + "]"))
            .collect(toList());

    // p ≤ fA + q (Note: fA is computed using matrix multiplication WITH f FROM THE LEFT.)
    final List<List<Coefficient>> sumByColumn = new ArrayList<>(n);
    for (int column = 0; column < n; column++) {
      final var tmp = new ArrayList<Coefficient>(3);
      tmp.add(q.get(column));
      sumByColumn.add(tmp);
    }

    // First, we handle comparison with monotony.
    if (!monoKnowledge.isEmpty()) {
      for (int column = 0; column < n; column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < monoKnowledge.size(); row++) {
          final var knowledgeRow = monoKnowledge.get(row);
          if (potentialFunction.equals(knowledgeRow.smaller())) {
            sum.add(f.get(row));
          } else if (potentialFunction.equals(knowledgeRow.larger())) {
            sum.add(f.get(row).negate());
          }
        }
      }
    }

    // Then we handle lemma 17.
    if (!lemmaKnowledge.isEmpty()) {
      final var lemmaOffset = monoKnowledge.size();
      for (int column = 0; column < n; column++) {
        final List<Integer> potentialFunction = potentialFunctions.get(column);
        final List<Coefficient> sum = sumByColumn.get(column);
        for (int row = 0; row < lemmaKnowledge.size(); row++) {
          final var knowledgeRow = lemmaKnowledge.get(row);
          UnknownCoefficient fi = f.get(row + lemmaOffset);
          if (potentialFunction.equals(knowledgeRow.get(0))) {
            sum.add(fi);
          } else if (potentialFunction.equals(knowledgeRow.get(1))) {
            sum.add(fi);
          } else if (potentialFunction.equals(knowledgeRow.get(2))) {
            UnknownCoefficient prod =
                UnknownCoefficient.maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(MINUS_TWO, fi), "(w:l17) " + prod + " = (-2) * " + fi));
            sum.add(prod);
          } else if (Util.isConstant(potentialFunction)) {
            UnknownCoefficient prod =
                UnknownCoefficient.maybeNegative(wid + ".l17.prod[" + column + "," + row + "]");
            constraints.add(
                new EqualsProductConstraint(
                    prod, List.of(TWO, fi), "(w:l17) " + prod + " = (2) * " + fi));
            sum.add(prod);
          }
        }
      }

      // NOTE: b is offset by knowledge.size() since it is all zero for the first knowledge.size()
      // indices.
      // NOTE: Since b is all zero for the first knowledge.size() indices, we can just skip those.
      // NOTE: We do not explicitly construct b since all it's elements are -2 according to Lemma
      // 17.

      /*
      final List<Coefficient> fbsum = new ArrayList<>();
      for (int i = 0; i < lemmaKnowledge.size(); i++) {
        UnknownCoefficient x = UnknownCoefficient.maybeNegative(wid + ".(f * b)[" + i + "]");
        UnknownCoefficient fi = f.get(lemmaOffset + i);
        constraints.add(
            new EqualsProductConstraint(
                x, List.of(fi, MINUS_TWO), "(w:l17) " + x + " = " + fi + " * (-2)"));
        fbsum.add(x);
      }
      fbsum.add(cp);

      UnknownCoefficient fbcp = UnknownCoefficient.maybeNegative(wid + ".fbcp");
      constraints.add(new EqualsSumConstraint(fbcp, fbsum, "(w:l17) " + fbcp + " = Σ... + c_p"));
      constraints.add(
          new LessThanOrEqualConstraint(fbcp, cq, "(w:l17) " + fbcp + " ≤ " + wid + ".c_q"));
      */
    }

    // fb + c_p ≤ c_q (Note: fb is computed using dot product. Since b is all zeros, we simplify
    //                       to c_p ≤ c_q.)
    // constraints.add(new LessThanOrEqualConstraint(cp, cq, "(w) c_p <= c_q (farkas)"));

    for (int i = 0; i < n; i++) {
      UnknownCoefficient x = UnknownCoefficient.maybeNegative(wid + ".sum[" + i + "]");
      constraints.add(
          new EqualsSumConstraint(
              x, sumByColumn.get(i), "(w) " + x + " = Σ... + q[" + i + "] (farkas)"));
      constraints.add(
          new LessThanOrEqualConstraint(
              p.get(i), x, "(w) " + wid + ".p[" + i + "] ≤ " + x + " (farkas)"));
    }

    return constraints;
  }

  private static List<List<List<Integer>>> lemma17(List<List<Integer>> potentialFunctions) {
    final var set = Set.copyOf(potentialFunctions);
    return cartesianProduct(potentialFunctions, potentialFunctions).stream()
        .filter(pair -> !Util.isConstant(pair.get(0)))
        .filter(pair -> !Util.isConstant(pair.get(1)))
        .filter(pair -> INDEX_COMPARATOR.compare(pair.get(0), pair.get(1)) < 1)
        .map(pair -> List.of(pair.get(0), pair.get(1), sum(pair.get(0), pair.get(1))))
        .filter(triple -> set.contains(triple.get(2)))
        .collect(Collectors.toList());
  }

  private static List<Integer> sum(List<Integer> a, List<Integer> b) {
    return Streams.zip(a.stream(), b.stream(), Integer::sum).collect(Collectors.toList());
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
                compareCoefficientsLessOrEqualUsingFarkas(
                    qp.size() > 0
                        ? singletonList(
                            new Identifier(
                                Predefined.INSTANCE, "_", new TreeType(TypeVariable.ALPHA), null))
                        : emptyList(),
                    qp,
                    pp,
                    globals.getSizeAnalysis())
                // compareCoefficientsGreaterOrEqual(pp, qp))
                )),
        emptyList());
  }

  public record Order<T>(T smaller, T larger) {}

  @Deprecated
  public static List<Order<List<Integer>>> lessThanOrEqual(
      List<List<Integer>> potentialFunctions, Set<Order<Integer>> expertGt) {
    if (potentialFunctions.isEmpty()) {
      return emptyList();
    }

    final var ctx = new Context(Map.of("unsat_core", "true"));
    final ArithExpr zero = ctx.mkInt(0);
    final ArithExpr one = ctx.mkInt(1);
    final var solver = ctx.mkSolver();
    final var size = potentialFunctions.get(0).size();

    final List<ArithExpr> ls =
        IntStream.range(0, size)
            .mapToObj(i -> ctx.mkIntConst("l" + i))
            .collect(Collectors.toUnmodifiableList());

    final List<ArithExpr> rs =
        IntStream.range(0, size)
            .mapToObj(i -> ctx.mkIntConst("r" + i))
            .collect(Collectors.toUnmodifiableList());

    solver.add(encodeAll(potentialFunctions, ls, ctx));
    solver.add(encodeAll(potentialFunctions, rs, ctx));

    /*
    solver.add(
        ctx.mkNot(
            ctx.mkAnd(Streams.zip(ls.stream(), rs.stream(), ctx::mkEq).toArray(BoolExpr[]::new))));
     */

    final var xSymbols =
        IntStream.range(0, size - 1)
            .mapToObj(i -> ctx.mkSymbol("x" + i))
            .collect(Collectors.toUnmodifiableList());
    final List<ArithExpr> xs =
        xSymbols.stream().map(ctx::mkRealConst).collect(Collectors.toUnmodifiableList());

    final var precondition =
        ctx.mkAnd(
            Stream.concat(
                    xs.stream().map(x -> ctx.mkLe(one, x)),
                    expertGt.stream()
                        .map(pair -> ctx.mkGt(xs.get(pair.smaller()), xs.get(pair.larger()))))
                .toArray(BoolExpr[]::new));
    final var main =
        ctx.mkLe(
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
        ctx.mkForall(
            xs.stream().toArray(Expr[]::new),
            ctx.mkImplies(precondition, main),
            1,
            null,
            null,
            null,
            null));

    final List<Order<List<Integer>>> result = new ArrayList<>();
    while (solver.check().equals(Status.SATISFIABLE)) {
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

      result.add(new Order<>(leftSolution, rightSolution));

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
    return result;
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
      List<Integer> o1, List<Integer> o2, Set<Order<Integer>> expertGt) {
    // o1 and o2 represent linear combinations:
    //
    //     o1:  o1.1 * x1 + o1.2 * x2 + o1.3 * x3 + ... + o1.(n-1) * x(n-1) + o1.n
    //     o2:  o2.1 * x1 + o2.2 * x2 + o2.3 * x3 + ... + o2.(n-1) * x(n-1) + o2.n
    //
    // expertGt represents additional knowledge about relations between xs,
    // it might contain:
    //
    //     x1 > x2
    //     x4 > x3
    //
    // We now must decide whether o1 <= o2. This is done by setting up a
    // SMT instance that tries to find a solution such that o1 > o2 given
    // expertGt. If this is not possible, we return true.

    if (o1.equals(o2)) {
      return true;
    }

    final var ctx = new Context();
    final var solver = ctx.mkSolver();
    final ArithExpr one = ctx.mkInt(1);
    final ArithExpr zero = ctx.mkInt(0);
    final List<ArithExpr> vars =
        IntStream.range(0, o1.size() - 1)
            .mapToObj(i -> ctx.mkRealConst("x" + i))
            .collect(Collectors.toUnmodifiableList());
    for (var x : vars) {
      // TODO
      // State that all xs are greater than zero, because
      // they represent sizes of trees, which are at least 1.
      solver.add(ctx.mkLe(one, x));
      // solver.add(ctx.mkLe(zero, x));
    }
    for (var pair : expertGt) {
      solver.add(ctx.mkGt(vars.get(pair.smaller()), vars.get(pair.larger())));
    }
    solver.add(
        ctx.mkGt(
            ctx.mkAdd(
                IntStream.range(0, o1.size())
                    .mapToObj(
                        i ->
                            ctx.mkMul(
                                ctx.mkInt(o1.get(i)), getOrDefault(vars, i, ctx.mkInt(o1.get(i)))))
                    .toArray(ArithExpr[]::new)),
            ctx.mkAdd(
                IntStream.range(0, o2.size())
                    .mapToObj(
                        i ->
                            ctx.mkMul(
                                ctx.mkInt(o2.get(i)), getOrDefault(vars, i, ctx.mkInt(o1.get(i)))))
                    .toArray(ArithExpr[]::new))));

    final var status = solver.check();
    return status.equals(Status.UNSATISFIABLE);
  }

  private static <T> T getOrDefault(List<T> list, int index, T defaultValue) {
    if (index >= 0 && list.size() > index) {
      return list.get(index);
    }
    return defaultValue;
  }

  @Override
  public String getName() {
    return "w";
  }
}
