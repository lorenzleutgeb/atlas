package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Sets.intersection;
import static java.util.Collections.singleton;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;

import com.google.common.collect.Sets;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.tuple.Pair;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.*;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;

public class LetTreeCf {
  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (LetExpression) obligation.getExpression();
    final var declared = expression.getDeclared();
    final var value = expression.getValue();
    final var gammaDeltaQ = obligation.getContext();
    final var body = expression.getBody();
    final var x = declared.getName();
    final var qp = obligation.getAnnotation();
    final List<Constraint> crossConstraints = new ArrayList<>();

    // Γ is used as context for e1, so from the combined context,
    // take Γ to be exactly the variables that occur in e1.
    final var varsForGamma = Util.setOfNames(value.freeVariables());

    final var bodyFreeVarsAsStrings = Util.setOfNames(body.freeVariables());

    // Δ on the other hand is "everything that's not in Γ".
    final var varsForDelta =
        gammaDeltaQ.getIds().stream()
            .filter(not(varsForGamma::contains))
            .collect(Collectors.toSet());

    // Now, one sanity check: There must not be free variables in the body which
    // are not in Δ.
    if (!Sets.difference(Sets.difference(bodyFreeVarsAsStrings, singleton(x)), varsForDelta)
        .isEmpty()) {
      throw bug("there are free variables in the body of a let binding which do not occur in Δ");
    }

    if (!intersection(varsForGamma, varsForDelta).isEmpty()) {
      throw bug("shared variables in let expression when attempting to generate constraints");
    }

    final var gamma = new ArrayList<>(varsForGamma);
    final var delta = new ArrayList<>(varsForDelta);

    final var isTree = value.getType() instanceof TreeType;

    if (!isTree) {
      throw bug("cannot apply (let:tree:cf) to a variable that is not a tree");
    }

    final var deltax = new ArrayList<>(delta);
    deltax.add(x);

    final var gammaP = globals.getHeuristic().generateContext("letcf" + x + "ΓP", gamma);
    final var deltaxr = globals.getHeuristic().generateContext("letcf" + x + "ΔR", deltax);

    // This is the "standard" obligation that we have to fulfill. It talks about e2
    // which is the body of the let-expression.
    // Δ, x : Tree | R ⊢ e_2 : β | Q'
    final Pair<Obligation, List<Constraint>> r =
        Pair.of(obligation.keepCost(deltaxr, body, qp), new ArrayList<>());

    // Γ | P ⊢ e1 : T | P'
    final var e1pp = globals.getHeuristic().generate(value);
    final Pair<Obligation, List<Constraint>> p =
        Pair.of(obligation.keepCost(gammaP, value, e1pp), new ArrayList<>());

    // First, all constraints that are generated for all types of e1. Ordered from l-to-r and
    // t-to-b.

    // For the next two constraints, note that x is neither included in Γ nor Δ.

    // Ensures that the potential through rank coefficients we get to evaluate this.value is the
    // same as we have available for this. Note that Γ ∩ Δ = ∅.
    p.getRight()
        .addAll(EqualityConstraint.eqRanks(gamma, gammaP, gammaDeltaQ, "(let:tree:cf) p_i = q_i"));

    // Ensures that the potential through rank coefficients we get to evaluate this.body is the same
    // as we have available for this. Note that Γ ∩ Δ = ∅.
    r.getRight()
        .addAll(
            EqualityConstraint.eqRanks(
                delta, deltaxr, gammaDeltaQ, "(let:tree:cf) r_j = q_{m + j}"));

    // Ensures that we transfer potential for Γ under Q to P (which
    // covers Γ exclusively).
    p.getRight()
        .addAll(
            gammaP.stream()
                .map(
                    pEntry ->
                        new EqualityConstraint(
                            pEntry.getValue(),
                            gammaDeltaQ.getCoefficientOrZero(pEntry.padWithZero()),
                            "(let:tree:cf) p_{(\\vec{a}, c)} = q_{(\\vec{a}, \\vec{0}, c)} with [\\vec{a}, c] = "
                                + pEntry.toString()))
                .collect(Collectors.toSet()));

    // Then, all constraints that are generated for e1 : T.
    // Γ | P ⊢ e1 : T | P'
    // Preserving the potential of evaluation of this.value of only makes sense if it is of type
    // tree.
    crossConstraints.add(
        // Since the result of evaluating this.value is effectively the same as the newly
        // introduced
        // variable, equate those coefficients.
        new EqualityConstraint(
            deltaxr.getRankCoefficient(x),
            e1pp.getRankCoefficient(),
            "(let:tree:cf) r_{k + 1} = p'_{*}"));

    crossConstraints.addAll(
        // Again, restore the potential we have got after evaluating this.value to align
        // with the new
        // variable
        // in the context for this.body.
        e1pp.streamCoefficients()
            .map(
                e -> {
                  final var index = e.getKey();
                  return new EqualityConstraint(
                      deltaxr.getCoefficient(id -> id.equals(x) ? index.get(0) : 0, index.get(1)),
                      e.getValue(),
                      "(let:tree:cf) r_{(\\vec{0}, a, c)} = p'_{(a, c)}");
                })
            .collect(toList()));

    final Map<Map<String, Integer>, Obligation> bs = new HashMap<>();

    final Function<? super Map<String, Integer>, Obligation> pbProducer =
        (key) ->
            new Obligation(
                globals.getHeuristic().generateContext("cf" + key, gamma),
                value,
                globals.getHeuristic().generate(value),
                0);

    gammaDeltaQ.stream()
        .filter(index -> delta.stream().anyMatch(id -> index.getAssociatedIndex(id) != 0))
        .filter(index -> gamma.stream().anyMatch(id -> index.getAssociatedIndex(id) != 0))
        .forEach(
            index -> {
              // Check if the index for all variables from delta are zero.
              final var b = new HashMap<String, Integer>();
              delta.forEach(id -> b.put(id, index.getAssociatedIndex(id)));

              final var bObligation = bs.computeIfAbsent(b, pbProducer);

              final var conclusion = new ArrayList<Constraint>();
              conclusion.add(
                  new EqualityConstraint(
                      bObligation.getContext().getCoefficient(index),
                      index.getValue(),
                      "(let:tree:cf) p^{(\\vec{b})}_{(\\vec{a},c)} = q_{(\\vec{a}, \\vec{b}, c)}"));

              // Ensure that exactly one of p'^{(\vec{b})}_{(a, c)} with a != 0 is one.
              conclusion.addAll(
                  exactlyOneIsOne(
                      bObligation
                          .getAnnotation()
                          .streamCoefficients()
                          .filter(entry -> entry.getKey().get(0) != 0)
                          .map(Map.Entry::getValue)
                          .collect(toList())));

              // Ensure that all p'^{(\vec{b})}_{(a, c)} with a == 0 are zero.
              conclusion.addAll(
                  bObligation
                      .getAnnotation()
                      .streamCoefficients()
                      .filter(entry -> entry.getKey().get(0) == 0)
                      .map(Map.Entry::getValue)
                      .map(
                          coefficient ->
                              new EqualityConstraint(
                                  coefficient, ZERO, "(let:tree:cf) p'^{(\\vec{b})}_{(0,c)} = 0"))
                      .collect(toList()));

              crossConstraints.add(
                  new DisjunctiveConstraint(
                      List.of(
                          new EqualityConstraint(
                              index.getValue(), ZERO, "(let:tree:cf) antecedent q? = 0"),
                          new ConjunctiveConstraint(conclusion, "?")),
                      "(let:tree:cf) q_{(\\vec{a},\\vec{b},c)} != 0 -> (p^{(\\vec{b})}_{(\\vec{a},c)} = q_({\\vec{a},\\vec{b},c) /\\ ...)"));
            });

    deltaxr.stream()
        .filter(index -> delta.stream().anyMatch(id -> index.getAssociatedIndex(id) != 0))
        .forEach(
            index -> {
              // Check if the index for all variables from delta are zero.
              final var b = new HashMap<String, Integer>();
              delta.forEach(id -> b.put(id, index.getAssociatedIndex(id)));

              final var a = index.getAssociatedIndex(x);

              final var bObligation = bs.computeIfAbsent(b, pbProducer);

              if (bObligation == null) {
                throw bug("oops");
              }

              crossConstraints.add(
                  new EqualityConstraint(
                      index.getValue(),
                      a == 0
                          ? gammaDeltaQ.getCoefficientOrZero(index.padWithZero())
                          : bObligation
                              .getAnnotation()
                              .getCoefficientOrZero(a, index.getOffsetIndex()),
                      "(let:tree:cf) r_{(\\vec{b},a,c)} = "
                          + (a == 0 ? "q_{(\\vec{0},b,c)}" : "p'^{(\\vec{b})}_{(a, c)}")));
            });

    var oldschool =
        Util.append(
            List.of(p, r),
            bs.values().stream()
                .map(o -> Pair.of(o, Collections.<Constraint>emptyList()))
                .collect(toList()));

    return new Rule.ApplicationResult(
        oldschool.stream().map(Pair::getLeft).collect(Collectors.toUnmodifiableList()),
        oldschool.stream().map(Pair::getRight).collect(Collectors.toUnmodifiableList()),
        crossConstraints);
  }

  public static List<Constraint> exactlyOneIsOne(Collection<Coefficient> coefficients) {
    return Stream.concat(
            Stream.of(new EqualsSumConstraint(ONE, coefficients, "∃!")),
            coefficients.stream()
                .map(
                    coefficient ->
                        new DisjunctiveConstraint(
                            List.of(
                                new EqualityConstraint(coefficient, ONE, "∃!"),
                                new EqualityConstraint(coefficient, ZERO, "∃!")),
                            "∃!")))
        .collect(toList());
  }
}
