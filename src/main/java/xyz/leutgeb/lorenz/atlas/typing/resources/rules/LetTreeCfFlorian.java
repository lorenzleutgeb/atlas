package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static com.google.common.collect.Sets.intersection;
import static java.util.Collections.singleton;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.ast.LetExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.DisjunctiveConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.LessThanOrEqualConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.Util;

/*
1 forall b⃗ != 0: p^{b⃗}_{(a⃗⃗,c)} <= q_{(a⃗⃗,b⃗,c)}
2 forall b != 0. exactly one of the coefficients p'^{b⃗}_{(a,c)} = 1 and all other p'^{b⃗}_{(e,d)} = 0 (i.e., a != e or c != d) or all p'^{b⃗_{(a,c)} = 0
3 forall b != 0. r_{(b⃗,a,c)} = p'^{b⃗_{(a,c)}
 */
@Deprecated
public class LetTreeCfFlorian implements Rule {
  public static final LetTreeCfFlorian INSTANCE = new LetTreeCfFlorian();

  public ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (LetExpression) obligation.getExpression();
    final var x = expression.getDeclared();
    final var value = expression.getValue();
    final var gammaDeltaQ = obligation.getContext();
    final var body = expression.getBody();
    final List<Constraint> crossConstraints = new ArrayList<>();

    if (!(value.getType() instanceof TreeType)) {
      throw bug("cannot apply (let:tree:cf:florian) to a variable that is not a tree");
    }

    // Γ is used as context for e1, so from the combined context,
    // take Γ to be exactly the variables that occur in e1.
    final var varsForGammaAsSet = value.freeVariables();

    // Δ on the other hand is "everything that's not in Γ".
    final var varsForDeltaAsSet =
        gammaDeltaQ.getIds().stream()
            .filter(not(varsForGammaAsSet::contains))
            .collect(Collectors.toSet());

    // Now, one sanity check: There must not be free variables in the body which
    // are not in Δ.
    if (!Sets.difference(Sets.difference(body.freeVariables(), singleton(x)), varsForDeltaAsSet)
        .isEmpty()) {
      throw bug("there are free variables in the body of a let binding which do not occur in Δ");
    }

    if (!intersection(varsForGammaAsSet, varsForDeltaAsSet).isEmpty()) {
      throw bug("shared variables in let expression when attempting to generate constraints");
    }

    final var varsForGammaAsList =
        obligation.getContext().getIds().stream().filter(varsForGammaAsSet::contains).toList();

    final var varsForDeltaAsList =
        obligation.getContext().getIds().stream().filter(varsForDeltaAsSet::contains).toList();

    final var deltax = new ArrayList<>(varsForDeltaAsList);
    deltax.add(x);

    // final var gammaP =
    //    globals.getHeuristic().generateContext("letcf " + x + " ΓP", varsForGammaAsList);
    final var gammaP = new AnnotatingContext(varsForGammaAsList, "P(" + x + ")");

    // final var deltaxr = globals.getHeuristic().generateContext("letcf " + x + " ΔR", deltax);
    final var deltaxr = new AnnotatingContext(deltax, "R(" + x + ")");

    // This is the "standard" obligation that we have to fulfill. It talks about e2
    // which is the body of the let-expression.
    // Δ, x : Tree | R ⊢ e2 : β | Q'
    final Pair<Obligation, List<Constraint>> r =
        Pair.of(obligation.keepAnnotationAndCost(deltaxr, body), new ArrayList<>());

    // Γ | P ⊢ e1 : T | P'
    final var pp = globals.getHeuristic().generate("P'(" + x + ")", value);
    // TODO: Heuristic generation would check whether x is a tree. Do we need to do that here, too?
    // final var pp = new Annotation(1, "letcf " + x + "Pprime");
    final Pair<Obligation, List<Constraint>> p =
        Pair.of(obligation.keepCost(gammaP, value, pp), new ArrayList<>());

    // First, all constraints that are generated for all types of e1. Ordered from l-to-r and
    // t-to-b.

    // For the next two constraints, note that x is neither included in Γ nor Δ.

    // Ensures that the potential through rank coefficients we get to evaluate this.value is the
    // same as we have available for this. Note that Γ ∩ Δ = ∅.
    varsForGammaAsList.stream()
        .map(
            id ->
                new EqualityConstraint(
                    gammaDeltaQ.getRankCoefficient(id),
                    gammaP.getRankCoefficientOrDefine(id),
                    "(let:tree:cf:florian) q_i = p_i"))
        .forEach(constraint -> p.getRight().add(constraint));

    // Ensures that the potential through rank coefficients we get to evaluate this.body is the same
    // as we have available for this. Note that Γ ∩ Δ = ∅.
    varsForDeltaAsList.stream()
        .map(
            id ->
                new EqualityConstraint(
                    gammaDeltaQ.getRankCoefficient(id),
                    deltaxr.getRankCoefficientOrDefine(id),
                    "(let:tree:cf:florian on " + x + ") q_{m+j} = r_j"))
        .forEach(constraint -> r.getRight().add(constraint));

    // Ensures that we transfer potential for Γ under Q to P (which
    // covers Γ exclusively).
    p.getRight()
        .addAll(
            gammaDeltaQ
                .streamNonRank()
                .filter(
                    entry ->
                        varsForDeltaAsSet.isEmpty() || entry.zeroAndNonEmptyOn(varsForDeltaAsSet))
                .map(
                    entry ->
                        new EqualityConstraint(
                            entry.getValue(),
                            gammaP.getCoefficientOrDefine(entry),
                            "(let:tree:cf:florian"
                                + x
                                + ") p_{(a⃗⃗,c)} = q_{(a⃗⃗,0⃗,c)} with [a⃗⃗, c] = "
                                + entry.toString()))
                .collect(Collectors.toSet()));

    // Then, all constraints that are generated for e1 : T.
    // Γ | P ⊢ e1 : T | P'
    crossConstraints.add(
        // Since the result of evaluating this.value is effectively the same as the newly
        // introduced variable, equate those coefficients.
        new EqualityConstraint(
            deltaxr.getRankCoefficientOrDefine(x),
            pp.getRankCoefficientOrDefine(),
            "(let:tree:cf:florian on " + x + ") r_{k + 1} = p'_{*}"));

    crossConstraints.addAll(
        // Again, restore the potential we have got after evaluating this.value to align
        // with the variable in the context for this.body.
        pp.streamNonRankCoefficients()
            .map(
                e -> {
                  final var index = e.getKey();
                  final var rCoefficient =
                      deltaxr.getCoefficientOrDefine(
                          id -> id.equals(x) ? index.get(0) : 0, index.get(1));
                  return new EqualityConstraint(
                      rCoefficient,
                      e.getValue(),
                      "(let:tree:cf:florian on " + x + ") r_{(0⃗,a,c)} = p'_{(a, c)}");
                })
            .collect(toList()));

    final Map<Map<Identifier, Integer>, Obligation> bs = new HashMap<>();

    final Function<? super Map<Identifier, Integer>, Obligation> pbProducer =
        (key) ->
            new Obligation(
                /*
                globals
                    .getHeuristic()
                    .generateContext("P(" + x + ")(" + key + ")", varsForGammaAsList),

                     */
                new AnnotatingContext(varsForGammaAsList, "P(" + x + ")(" + key + ")"),
                value,
                // globals.getHeuristic().generate("P'(" + x + ")(" + key + ")", value),
                new Annotation(1, "P'(" + x + ")(" + key + ")"),
                false,
                Optional.of(obligation));

    gammaDeltaQ
        .streamNonRank()
        .filter(index -> index.nonZeroOrEmptyOn(varsForDeltaAsSet))
        .forEach(
            qEntry -> {
              // Check if the index for all variables from delta are zero.
              final var b = new HashMap<Identifier, Integer>();
              varsForDeltaAsList.forEach(id -> b.put(id, qEntry.getAssociatedIndex(id)));

              final var bObligation = bs.computeIfAbsent(b, pbProducer);

              for (final var id : bObligation.getContext().getIds()) {
                crossConstraints.add(
                    new EqualityConstraint(
                        bObligation.getContext().getRankCoefficientOrDefine(id),
                        ZERO,
                        "(let:tree:cf:florian on " + x + ") no rank"));
              }

              // 1: forall vec(b) != 0: p^vec(b)_vec(a),c <= q_vec(a),vec(b),c
              if ((!(qEntry.allAssociatedIndicesMatch(varsForGammaAsList, a -> a == 0))
                          || qEntry.getOffsetIndex() > 0)
                      && !(qEntry.getOffsetIndex() == 0)
                  || !varsForGammaAsList.isEmpty()
                      && !qEntry.allAssociatedIndicesMatch(varsForGammaAsList, a -> a == 0)) {

                Coefficient coefficientOrZero =
                    bObligation.getContext().getCoefficientOrDefine(qEntry);
                crossConstraints.add(
                    new LessThanOrEqualConstraint(
                        coefficientOrZero,
                        qEntry.getValue(),
                        "(let:tree:cf:florian on "
                            + x
                            + ") p^{(b⃗)}_{(a⃗⃗,c)} ≤ q_{(a⃗⃗, b⃗, c)="
                            + qEntry.toIndexString()
                            + "}"));
              }

              // 2
              // forall b != 0. exactly one of the coefficients p'^vec(b)_a,c = 1 and all other
              // p'^vec(b)_e,d = 0 (i.e., a != e or c != d) or all p'^vec(b)_a,c = 0
              final var pbcoeffs =
                  bObligation
                      .getAnnotation()
                      .streamNonRankCoefficients()
                      // .filter(entry -> entry.getKey().get(0) != 0)
                      .map(Map.Entry::getValue)
                      .collect(toList());

              crossConstraints.add(
                  new DisjunctiveConstraint(
                      List.of(exactlyOneIsOne(pbcoeffs), allZero(pbcoeffs)),
                      "(let:tree:cf:florian) none or one"));
            });

    // 3 forall b != 0. r_{(b⃗,a,c)} = p'^{b⃗_{(a,c)}
    deltaxr
        .streamNonRank()
        .filter(index -> index.nonZeroOrEmptyOn(varsForDeltaAsSet))
        .forEach(
            index -> {
              final var b = new HashMap<Identifier, Integer>();
              varsForDeltaAsList.forEach(id -> b.put(id, index.getAssociatedIndex(id)));
              final var bObligation = bs.computeIfAbsent(b, pbProducer);
              crossConstraints.add(
                  new EqualityConstraint(
                      index.getValue(),
                      bObligation
                          .getAnnotation()
                          .getCoefficientOrDefine(
                              index.getAssociatedIndex(x), index.getOffsetIndex()),
                      "(let:tree:cf:florian) r_{(b⃗,a,c)} = p'^{(b⃗)}_{(a, c)}"));
            });

    var old =
        Util.append(
            List.of(p, r),
            bs.values().stream()
                .map(o -> Pair.of(o, Collections.<Constraint>emptyList()))
                .collect(toList()));

    return new ApplicationResult(
        old.stream().map(Pair::getLeft).toList(),
        old.stream().map(Pair::getRight).toList(),
        crossConstraints);
  }

  public static Constraint exactlyOneIsOne(Collection<Coefficient> coefficients) {
    return new ConjunctiveConstraint(
        Stream.concat(
                Stream.of(new EqualsSumConstraint(ONE, coefficients, "∃!")),
                coefficients.stream()
                    .map(
                        coefficient ->
                            new DisjunctiveConstraint(
                                List.of(
                                    new EqualityConstraint(coefficient, ONE, "∃!"),
                                    new EqualityConstraint(coefficient, ZERO, "∃!")),
                                "∃!")))
            .collect(toList()),
        "∃!");
  }

  public static Constraint allZero(Collection<Coefficient> coefficients) {
    return new EqualsSumConstraint(ZERO, coefficients, "allzero");
  }
}
