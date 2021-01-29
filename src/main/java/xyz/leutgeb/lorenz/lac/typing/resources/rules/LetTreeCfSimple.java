package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Sets.intersection;
import static java.util.Collections.singleton;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ONE;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.ConjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.DisjunctiveConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.Util;

@Deprecated
public class LetTreeCfSimple implements Rule {
  public static final LetTreeCfSimple INSTANCE = new LetTreeCfSimple();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var expression = (LetExpression) obligation.getExpression();
    final var x = expression.getDeclared();
    final var value = expression.getValue();
    final var gammaDeltaQ = obligation.getContext();
    final var body = expression.getBody();
    final List<Constraint> crossConstraints = new ArrayList<>();

    if (!(value.getType() instanceof TreeType)) {
      throw bug("cannot apply (let:tree:cf) to a variable that is not a tree");
    }

    final Set<Coefficient> occurred = new HashSet<>();

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
        obligation.getContext().getIds().stream()
            .filter(varsForGammaAsSet::contains)
            .collect(Collectors.toUnmodifiableList());

    final var varsForDeltaAsList =
        obligation.getContext().getIds().stream()
            .filter(varsForDeltaAsSet::contains)
            .collect(Collectors.toUnmodifiableList());

    final var deltax = new ArrayList<>(varsForDeltaAsList);
    deltax.add(x);

    final var gammaP =
        globals.getHeuristic().generateContext("letcf " + x + " ΓP", varsForGammaAsList);
    final var deltaxr = globals.getHeuristic().generateContext("letcf " + x + " ΔR", deltax);

    // This is the "standard" obligation that we have to fulfill. It talks about e2
    // which is the body of the let-expression.
    // Δ, x : Tree | R ⊢ e2 : β | Q'
    final Pair<Obligation, List<Constraint>> r =
        Pair.of(obligation.keepAnnotationAndCost(deltaxr, body), new ArrayList<>());

    // Γ | P ⊢ e1 : T | P'
    final var pp = globals.getHeuristic().generate("letcf " + x + "P'", value);
    final Pair<Obligation, List<Constraint>> p =
        Pair.of(obligation.keepCost(gammaP, value, pp), new ArrayList<>());

    // First, all constraints that are generated for all types of e1. Ordered from l-to-r and
    // t-to-b.

    // For the next two constraints, note that x is neither included in Γ nor Δ.

    // Ensures that the potential through rank coefficients we get to evaluate this.value is the
    // same as we have available for this. Note that Γ ∩ Δ = ∅.
    p.getRight()
        .addAll(
            EqualityConstraint.eqRanks(
                varsForGammaAsList, gammaP, gammaDeltaQ, "(let:tree:cf) p_i = q_i"));

    // Ensures that the potential through rank coefficients we get to evaluate this.body is the same
    // as we have available for this. Note that Γ ∩ Δ = ∅.
    r.getRight()
        .addAll(
            EqualityConstraint.eqRanks(
                varsForDeltaAsList, deltaxr, gammaDeltaQ, "(let:tree:cf) r_j = q_{m + j}"));

    // Ensures that we transfer potential for Γ under Q to P (which
    // covers Γ exclusively).
    p.getRight()
        .addAll(
            gammaP
                .streamNonRank()
                .map(
                    pEntry ->
                        new EqualityConstraint(
                            pEntry.getValue(),
                            gammaDeltaQ.getCoefficientOrZero(pEntry.padWithZero()),
                            "(let:tree:cf) p_{(a⃗⃗, c)} = q_{(a⃗⃗, 0⃗, c)} with [a⃗⃗, c] = "
                                + pEntry.toString()))
                .collect(Collectors.toSet()));

    // Then, all constraints that are generated for e1 : T.
    // Γ | P ⊢ e1 : T | P'
    crossConstraints.add(
        // Since the result of evaluating this.value is effectively the same as the newly
        // introduced variable, equate those coefficients.
        new EqualityConstraint(
            deltaxr.getRankCoefficient(x),
            pp.getRankCoefficient(),
            "(let:tree:cf) r_{k + 1} = p'_{*}"));

    crossConstraints.addAll(
        // Again, restore the potential we have got after evaluating this.value to align
        // with the variable in the context for this.body.
        pp.streamNonRankCoefficients()
            .map(
                e -> {
                  final var index = e.getKey();
                  final var rCoefficient =
                      deltaxr.getCoefficient(id -> id.equals(x) ? index.get(0) : 0, index.get(1));
                  occurred.add(rCoefficient);
                  return new EqualityConstraint(
                      rCoefficient, e.getValue(), "(let:tree:cf) r_{(0⃗, a, c)} = p'_{(a, c)}");
                })
            .collect(toList()));

    final Map<Map<Identifier, Integer>, Obligation> bs = new HashMap<>();

    final Function<? super Map<Identifier, Integer>, Obligation> pbProducer =
        (key) ->
            new Obligation(
                globals
                    .getHeuristic()
                    .generateContext("letcf " + x + " b is " + key, varsForGammaAsList),
                value,
                globals.getHeuristic().generate("letcf' " + x + " b is " + key, value),
                0,
                Optional.of(obligation));

    gammaDeltaQ
        .streamNonRank()
        .filter(
            index -> varsForDeltaAsList.stream().anyMatch(id -> index.getAssociatedIndex(id) != 0))
        .filter(
            index -> varsForGammaAsList.stream().anyMatch(id -> index.getAssociatedIndex(id) != 0))
        .forEach(
            index -> {
              // Check if the index for all variables from delta are zero.
              final var b = new HashMap<Identifier, Integer>();
              varsForDeltaAsList.forEach(id -> b.put(id, index.getAssociatedIndex(id)));

              final var bObligation = bs.computeIfAbsent(b, pbProducer);

              final var conclusion = new ArrayList<Constraint>();
              conclusion.add(
                  new EqualityConstraint(
                      bObligation.getContext().getCoefficient(index),
                      index.getValue(),
                      "(let:tree:cf) p^{(b⃗)="
                          + b
                          + "}_{(a⃗⃗,c)="
                          + index
                          + "} = q_{(a⃗⃗, b⃗, c)="
                          + index
                          + "}"));

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
                      "(let:tree:cf) none or one"));

              // Ensure that exactly one of p'^{(\vec{b})}_{(a, c)} with a != 0 is one.
              conclusion.add(
                  exactlyOneIsOne(
                      bObligation
                          .getAnnotation()
                          .streamNonRankCoefficients()
                          .filter(entry -> entry.getKey().get(0) != 0)
                          .map(Map.Entry::getValue)
                          .collect(toList())));

              // Ensure that all p'^{(\vec{b})}_{(a, c)} with a == 0 are zero.
              conclusion.addAll(
                  bObligation
                      .getAnnotation()
                      .streamNonRankCoefficients()
                      .filter(entry -> entry.getKey().get(0) == 0)
                      .map(Map.Entry::getValue)
                      .map(
                          coefficient ->
                              new EqualityConstraint(
                                  coefficient,
                                  ZERO,
                                  "(let:tree:cf) p'^{(b⃗)=" + b + "}_{(0,c)} = 0"))
                      .collect(toList()));

              crossConstraints.add(
                  new DisjunctiveConstraint(
                      List.of(
                          new EqualityConstraint(
                              index.getValue(), ZERO, "(let:tree:cf) antecedent q? = 0"),
                          new ConjunctiveConstraint(conclusion, "conjunction in let:tree:cf")),
                      "(let:tree:cf) q_{(a⃗⃗,b⃗,c)} != 0 -> (p^{(b⃗)="
                          + b
                          + "}_{(a⃗⃗,c)} = q_({a⃗⃗,b⃗,c) /\\ ...)"));
            });

    deltaxr
        .streamNonRank()
        .filter(
            index -> varsForDeltaAsList.stream().anyMatch(id -> index.getAssociatedIndex(id) != 0))
        .forEach(
            index -> {
              // Check if the index for all variables from delta are zero.
              final var b = new HashMap<Identifier, Integer>();
              varsForDeltaAsList.forEach(id -> b.put(id, index.getAssociatedIndex(id)));

              final var a = index.getAssociatedIndex(x);

              final var bObligation = bs.computeIfAbsent(b, pbProducer);

              if (bObligation == null) {
                throw bug("oops");
              }

              occurred.add(index.getValue());
              crossConstraints.add(
                  new EqualityConstraint(
                      index.getValue(),
                      a == 0
                          ? gammaDeltaQ.getCoefficientOrZero(index.padWithZero())
                          : bObligation
                              .getAnnotation()
                              .getCoefficientOrZero(a, index.getOffsetIndex()),
                      "(let:tree:cf) r_{(b⃗,a,c)} = "
                          + (a == 0 ? "q_{(0⃗,b,c)}" : "p'^{(b⃗)}_{(a, c)}")));
            });

    var old =
        Util.append(
            List.of(p, r),
            bs.values().stream()
                .map(o -> Pair.of(o, Collections.<Constraint>emptyList()))
                .collect(toList()));

    /*
    deltaxr.stream()
        .map(AnnotatingContext.Entry::getValue)
        .filter(not(occurred::contains))
        .map(
            coefficient ->
                new EqualityConstraint(ZERO, coefficient, "(let:tree:cf) setToZero " + coefficient))
        .forEach(crossConstraints::add);
     */

    return new Rule.ApplicationResult(
        old.stream().map(Pair::getLeft).collect(Collectors.toUnmodifiableList()),
        old.stream().map(Pair::getRight).collect(Collectors.toUnmodifiableList()),
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

  @Override
  public String getName() {
    return "let:tree:cf:simple";
  }
}
