package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Sets.intersection;
import static java.util.Collections.singleton;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toList;
import static xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient.ZERO;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.util.Pair;

public class LetTree implements Rule {
  public static final LetTree INSTANCE = new LetTree();

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (LetExpression) obligation.getExpression();
    final var declared = expression.getDeclared();
    final var value = expression.getValue();
    final var gammaDeltaQ = obligation.getContext();
    final var body = expression.getBody();
    final var x = declared;
    final var qp = obligation.getAnnotation();
    final List<Constraint> crossConstraints = new ArrayList<>();

    final var occurred = new HashSet<Coefficient>();

    // Γ is used as context for e1, so from the combined context,
    // take Γ to be exactly the variables that occur in e1.
    final var varsForGamma = value.freeVariables();

    final var bodyFreeVarsAsStrings = body.freeVariables();

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
      throw bug("cannot apply (let:tree) if variable is not a tree");
    }

    final var deltax = new ArrayList<>(delta);
    deltax.add(x);

    final var gammaP = globals.getHeuristic().generateContext("let" + x + "ΓP", gamma);
    final var deltaxr = globals.getHeuristic().generateContext("let" + x + "ΔR", deltax);

    // This is the "standard" obligation that we have to fulfill. It talks about e2
    // which is the body of the let-expression.
    // Δ, x : Tree | R ⊢ e_2 : β | Q'
    final Pair<Obligation, List<Constraint>> r =
        Pair.of(obligation.keepCost(deltaxr, body, qp), new ArrayList<>());

    // Γ | P ⊢ e1 : T | P'
    final var e1pp = globals.getHeuristic().generate("lettree " + x + " P'", value);
    final Pair<Obligation, List<Constraint>> p =
        Pair.of(obligation.keepCost(gammaP, value, e1pp), new ArrayList<>());

    // For the next two constraints, note that x is neither included in Γ nor Δ.

    // Ensures that the potential through rank coefficients we get to evaluate this.value is the
    // same as we have available for this. Note that Γ ∩ Δ = ∅.
    p.getRight()
        .addAll(EqualityConstraint.eqRanks(gamma, gammaP, gammaDeltaQ, "(let:tree) p_i = q_i"));

    // Ensures that the potential through rank coefficients we get to evaluate this.body is the same
    // as we have available for this. Note that Γ ∩ Δ = ∅.
    r.getRight()
        .addAll(
            EqualityConstraint.eqRanks(delta, deltaxr, gammaDeltaQ, "(let:tree) r_j = q_{m + j}"));

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
                            "(let:tree) p_{(\\vec{a},c)} = q_{(\\vec{a},\\vec{0},c)}"))
                .collect(Collectors.toSet()));

    // Preserving the potential of evaluation of this.value of only makes sense if it is of type
    // tree.
    crossConstraints.add(
        // Since the result of evaluating this.value is effectively the same as the newly
        // introduced variable, equate those coefficients.
        new EqualityConstraint(
            deltaxr.getRankCoefficient(x),
            e1pp.getRankCoefficient(),
            "(let:tree) r_{k + 1} = p'_{*} (for `" + x + "` on the left)"));

    occurred.add(deltaxr.getRankCoefficient(x));

    crossConstraints.addAll(
        // Again, restore the potential we have got after evaluating this.value to align
        // with the new variable in the context for this.body.
        e1pp.streamCoefficients()
            .map(
                e -> {
                  final var index = e.getKey();
                  Coefficient rCoefficient =
                      deltaxr.getCoefficient(id -> id.equals(x) ? index.get(0) : 0, index.get(1));
                  occurred.add(rCoefficient);
                  return new EqualityConstraint(
                      rCoefficient,
                      e.getValue(),
                      "(let:tree) r_{(\\vec{0}, a, c)} = p'_{(a, c)} with (a, c) = " + index);
                })
            .collect(toList()));

    deltaxr.stream()
        .filter(
            rEntry ->
                delta.stream().anyMatch(id -> rEntry.getAssociatedIndices().get(id) != 0)
                    && rEntry.getAssociatedIndex(x) == 0)
        .forEach(
            rEntry -> {
              occurred.add(rEntry.getValue());
              r.getRight()
                  .add(
                      new EqualityConstraint(
                          rEntry.getValue(),
                          gammaDeltaQ.getCoefficientOrZero(
                              rEntry.mask(
                                  id ->
                                      varsForGamma.contains(id)
                                          ? 0
                                          : rEntry.getAssociatedIndices().get(id))),
                          "(let:tree) ∀ \\vec{b} ≠ \\vec{0} . r_{(\\vec{b}, 0, c)} = q_{(\\vec{0}, \\vec{b}, c)} with (\\vec{b}, 0, c) = "
                              + rEntry));
            });

    final List<Constraint> setToZeroR =
        deltaxr.stream()
            .map(AnnotatingContext.Entry::getValue)
            .filter(not(occurred::contains))
            .map(
                coefficient ->
                    new EqualityConstraint(
                        coefficient, ZERO, "(let:tree) setToZero r " + coefficient))
            .collect(Collectors.toUnmodifiableList());

    crossConstraints.addAll(setToZeroR);
    return new Rule.ApplicationResult(
        List.of(p.getLeft(), r.getLeft()), List.of(p.getRight(), r.getRight()), crossConstraints);
  }

  @Override
  public String getName() {
    return "let:tree";
  }
}
