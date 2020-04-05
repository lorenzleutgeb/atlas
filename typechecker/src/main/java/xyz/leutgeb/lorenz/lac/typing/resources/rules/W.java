package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Comparators.lexicographical;
import static com.google.common.collect.Streams.concat;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.Util.append;
import static xyz.leutgeb.lorenz.lac.Util.bug;

import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.hipparchus.util.Pair;
import org.jgrapht.Graph;
import org.jgrapht.alg.shortestpath.AllDirectedPaths;
import xyz.leutgeb.lorenz.lac.SizeEdge;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.GreaterThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class W {
  private static List<Constraint> compareCoefficientsGreaterOrEqual(
      Annotation left, Annotation right) {
    return compareCoefficients(left, right, GreaterThanOrEqualConstraint::new);
  }

  private static List<Constraint> compareCoefficientsLessOrEqual(
      Annotation left, Annotation right) {
    return compareCoefficients(left, right, LessThanOrEqualConstraint::new);
  }

  private static List<Constraint> compareCoefficients(
      Annotation left,
      Annotation right,
      BiFunction<Coefficient, Coefficient, Constraint> comparator) {
    return concat(
            left.streamCoefficients()
                .map(e -> comparator.apply(e.getValue(), right.getCoefficientOrZero(e.getKey()))),
            IntStream.range(0, left.size())
                .mapToObj(
                    i -> comparator.apply(left.getRankCoefficient(i), right.getRankCoefficient(i))))
        .collect(toList());
  }

  private static List<Constraint> compareCoefficientsLessOrEqualUsingFarkas(
      List<String> identifiers,
      Annotation left,
      Annotation right,
      Graph<Identifier, SizeEdge> sizeAnalysis) {
    // left is p in paper.
    // right is q in paper.

    Map<String, Identifier> vertices = new HashMap<>();
    for (String stringIdentifier : identifiers) {
      var matches =
          sizeAnalysis.vertexSet().stream()
              .filter(identifier -> identifier.getName().equals(stringIdentifier))
              .collect(Collectors.toList());
      if (matches.size() != 1) {
        vertices = null;
        break;
      }
      vertices.put(stringIdentifier, matches.get(0));
    }

    if (vertices != null) {
      var ids = List.copyOf(vertices.keySet());
      Map<String, Identifier> finalVertices = vertices;
      Lists.cartesianProduct(ids, ids).stream()
          .filter(pair -> !pair.get(0).equals(pair.get(1)))
          .map(
              pair -> {
                final var paths =
                    (new AllDirectedPaths<>(sizeAnalysis))
                        .getAllPaths(
                            finalVertices.get(pair.get(0)),
                            finalVertices.get(pair.get(1)),
                            true,
                            Integer.MAX_VALUE);
                return paths;
              })
          .collect(Collectors.toList());
    }

    final List<Constraint> constraints = new ArrayList<>();
    // TODO: What about rank coefficients?
    IntStream.range(0, left.size())
        .mapToObj(
            i ->
                new LessThanOrEqualConstraint(
                    left.getRankCoefficient(i), right.getRankCoefficient(i)))
        .forEach(constraints::add);

    final var variablePotentialFunctionIndices =
        Stream.concat(left.streamCoefficients(), right.streamCoefficients())
            .map(Map.Entry::getKey)
            .filter(not(Util::isConstant))
            .distinct()
            .sorted(lexicographical(Integer::compareTo))
            .collect(toList());

    final var n = variablePotentialFunctionIndices.size();

    final var knowledge =
        Lists.cartesianProduct(variablePotentialFunctionIndices, variablePotentialFunctionIndices)
            .stream()
            .filter(pair -> ordered(pair.get(0), pair.get(1)))
            .collect(toList());

    // m is the number of rows of expert knowledge.
    final var m = knowledge.size();

    // Note: We do not add constraints saying f ≥ 0. This is generated for all unknown coefficients!
    // TODO: Does this mean that all elements of f must be positive, or just a single one?
    // Note: If b never changes to be nonzero, this could be refactored, generating f
    // element by element in the multiplication loop below. No explicit allocation of f
    // in a list would be required.
    final var f = Stream.generate(UnknownCoefficient::unknown).limit(m).collect(toList());

    final var cp = left.getUnitCoefficientOrZero();
    final var cq = right.getUnitCoefficientOrZero();

    // fb + c_p ≤ c_q (Note: fb is computed using dot product. Since b is all zeros, we simplify
    //                       to c_p ≤ c_q.)
    // If we were to generate b as all zeros, it'd look as follows:
    // final var b = Stream.generate(() -> ZERO).limit(m).collect(toList());
    constraints.add(new LessThanOrEqualConstraint(cp, cq));

    final var p =
        variablePotentialFunctionIndices.stream().map(left::getCoefficientOrZero).collect(toList());
    final var q =
        variablePotentialFunctionIndices.stream()
            .map(right::getCoefficientOrZero)
            .collect(toList());

    if (n != p.size() || n != q.size()) {
      throw bug("q, p  need to be column vectors for A");
    }

    // p ≤ fA + q (Note: fA is computed using matrix multiplication WITH f FROM THE LEFT.)
    for (int j = 0; j < n; j++) {
      final List<Integer> potentialFunction = variablePotentialFunctionIndices.get(j);
      final List<Coefficient> sum = new ArrayList<>();
      for (int k = 0; k < m; k++) {
        List<List<Integer>> knowledgeRow = knowledge.get(k);
        if (potentialFunction.equals(knowledgeRow.get(0))) {
          sum.add(f.get(j));
        } else if (potentialFunction.equals(knowledgeRow.get(1))) {
          sum.add(f.get(j).negate());
        }
      }
      sum.add(q.get(j));
      UnknownCoefficient x = UnknownCoefficient.unknown();
      constraints.add(new EqualsSumConstraint(x, sum));
      constraints.add(new LessThanOrEqualConstraint(p.get(j), x));
    }

    return constraints;
  }

  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var q = obligation.getContext().getAnnotation();
    final var qp = obligation.getAnnotation();

    final var p = globals.getHeuristic().generate("w", q);
    final var pp = globals.getHeuristic().generate("wp", qp);

    return new Rule.ApplicationResult(
        singletonList(
            Pair.create(
                obligation.keepCost(
                    new AnnotatingContext(obligation.getContext().getIds(), p),
                    obligation.getExpression(),
                    pp),
                append(
                    compareCoefficientsLessOrEqualUsingFarkas(
                        obligation.getContext().getIds(), p, q, globals.getSizeAnalysis()),
                    // compareCoefficientsLessOrEqualUsingFarkas(qp, pp)
                    // compareCoefficientsLessOrEqual(p, q),
                    compareCoefficientsGreaterOrEqual(pp, qp)))),
        emptyList());
  }

  public static boolean ordered(List<Integer> o1, List<Integer> o2) {
    // As this is implemented now, we only account for equality between the
    // sizes of trees in the left and right linear combination.
    // In the future, we may also have additional knowledge, that the size
    // of a tree might be bigger than the size of some other tree. Consider:
    //
    //   f x t1 t2 = let t3 = (t1, x, t2) in (t3, x, t1)
    //
    // which gets unshared to
    //
    //   f x t1 t2 = share t1 as (t1', t1'') in let t3 = (t1', x, t2) in (t3, x, t1'')
    //
    // Now, we would reach
    //
    //   t3, t1'' | Q |- (t3, x, t1'') | Q'
    //
    // and apply (w), followed by (node).
    //
    // In this case, we can be sure that |t3| > |t1''|. We must infer
    //   - |t1| = |t1'| = |t1''| from the share expression
    //   - |t3| > |t1'| and |t3| > |t2| from the let expression
    //
    // With this information we can now derive additional expert knowledge
    // asserting that:
    //
    //   (1, 0) < (0, 1)
    //
    // While without such analysis we can only do:
    //
    //   (0, 0) < (0, 1)
    //   (0, 0) < (1, 0)
    //   (0, 0) < (1, 1)
    //   (0, 1) < (1, 1)
    //   (1, 0) < (1, 1)

    boolean bigger = false;
    for (int i = 0; i < o1.size(); i++) {
      final int diff = o1.get(i) - o2.get(i);
      if (diff > 0) {
        return false;
      } else if (diff < 0) {
        bigger = true;
      }
    }
    return bigger;
  }
}
