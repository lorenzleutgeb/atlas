package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static com.google.common.collect.Sets.intersection;
import static java.util.Collections.singleton;
import static java.util.function.Predicate.not;
import static xyz.leutgeb.lorenz.lac.Util.bug;

import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;

public class Let {
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

    final var deltax = new ArrayList<>(delta);
    // For the case that the scrutinee (variable "x") is not a tree, we do not actually
    // add it to the context that R annotates, and it is the same as Δ.
    if (isTree) {
      deltax.add(x);
    }

    final var gammaP = globals.getHeuristic().generateContext("let" + x + "ΓP", gamma);
    final var deltaxr = globals.getHeuristic().generateContext("let" + x + "ΔR", deltax);

    // This is the "standard" obligation that we have to fulfill. It talks about e2
    // which is the body of the let-expression.
    // In the paper it takes two forms:
    //
    //  (let : T)      Δ, x : Tree | R  \turnstile  e_2 : β | Q'
    //  (let : gen)    Δ, x : α    | R  \turnstile  e_2 : β | Q'
    //
    // Both forms are spelled the same way in this implementation. For
    // (let : gen) we will not have x in the context, but this is handled
    // above, where deltaxr is initialized.
    final Pair<Obligation, List<Constraint>> r =
        Pair.create(obligation.keepCost(deltaxr, body, qp), new ArrayList<>());

    final var e1pp = isTree ? globals.getHeuristic().generate("lel", 1) : Annotation.empty();
    final Pair<Obligation, List<Constraint>> p =
        Pair.create(obligation.keepCost(gammaP, value, e1pp), new ArrayList<>());

    // First, all constraints that are generated for all types of e1. Ordered from l-to-r and
    // t-to-b.

    // For the next two constraints, note that x is neither included in Γ nor Δ.

    // p_i = q_i
    // Ensures that the potential through rank coefficients we get to evaluate this.value is the
    // same as we have available for this. Note that Γ ∩ Δ = ∅.
    for (var id : gamma) {
      p.getSecond()
          .add(
              new EqualityConstraint(
                  gammaP.getRankCoefficient(id), gammaDeltaQ.getRankCoefficient(id)));
    }

    // r_j = q_{m + j}
    // Ensures that the potential through rank coefficients we get to evaluate this.body is the same
    // as we have available for this. Note that Γ ∩ Δ = ∅.
    for (var id : delta) {
      r.getSecond()
          .add(
              new EqualityConstraint(
                  deltaxr.getRankCoefficient(id), gammaDeltaQ.getRankCoefficient(id)));
    }

    // p_{(\vec{a}, c)} = q_{(\vec{a}, \vec{0}, c)}
    // Ensures that we transfer potential for Γ under Q to P (which
    // covers Γ exclusively).

    p.getSecond()
        .addAll(
            gammaP.stream()
                .map(
                    pEntry ->
                        new EqualityConstraint(
                            pEntry.getValue(),
                            gammaDeltaQ.getCoefficientOrZero(pEntry.padWithZero())))
                .collect(Collectors.toSet()));

    // Then, all constraints that are generated for e1 : T.
    if (isTree) {
      // Γ | P \turnstile e1 : T | P'
      // Preserving the potential of evaluation of this.value of only makes sense if it is of type
      // tree.

      crossConstraints.add(
          // r_{k + 1} = p'_{*}
          // Since the result of evaluating this.value is effectively the same as the newly
          // introduced
          // variable, equate those coefficients.
          new EqualityConstraint(deltaxr.getRankCoefficient(x), e1pp.getRankCoefficient()));

      crossConstraints.addAll(
          // r_{(\vec{0}, a, c)} = p'_{(a, c)}
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
                        e.getValue());
                  })
              .collect(Collectors.toList()));

      /*
       // Now for the cost-free part:

       // Keep track of all P_{\vec{b}}
       final Map<Map<String, Integer>, AnnotatingContext> pbs = new HashMap<>();

       // Produces a new annotating context to be used for cost-free typing.
       final Function<? super Map<String, Integer>, AnnotatingContext> pbProducer =
           (key) -> globals.getHeuristic().generateContext("cf", gamma);

       // p^{\vec{b}}_{(\vec{a}, c)} = q_{(\vec{a}, \vec{b}, c)}
       gammaDeltaQ
           .streamIndices()
           .filter(index -> delta.stream().anyMatch(id -> index.getFirst().get(id) != 0))
           .forEach(
               index -> {
                 // Check if the index for all variables from delta are zero.
                 final var b = new HashMap<String, Integer>();
                 delta.forEach(id -> b.put(id, index.getFirst().get(id)));
                 generatedConstraints.addAll(
                     EqualityConstraint.eq(
                         gammaDeltaQ.getCoefficient(index),
                         pbs.computeIfAbsent(b, pbProducer).getCoefficient(index)));
               });

       for (var e : pbs.entrySet()) {
         final var b = e.getKey();
         // We call inferAnnotationsInternal here instead of inferAnnotations to avoid getting back
         // the cached annotation for this.value.

         // Old way:
         // final var pbp = value.inferAnnotationsInternal(e.getValue(), globals.costFree());

         final var pbp = globals.getHeuristic().generate("lol", 1);

         generatedObligations.add(obligation.keepCost(e.getValue(), value, pbp));

         if (pbp.size() != 1) {
           throw bug("annotation must be of expected size 1");
         }

         // p'^{\vec{b}}_{(a, c)} = r_{(\vec{b}, a, c)}
         for (var d : pbp.getCoefficients()) {
           final var index = d.getKey();
           generatedConstraints.addAll(
               EqualityConstraint.eq(
                   deltaxr.getCoefficient(
                       id -> id.equals(x) ? index.get(0) : b.get(id), index.get(1)),
                   d.getValue()));
         }
       }
      */

      // forall \vec{b} \not = \vec{0}
      // r_{(\vec{b}, 0, c)} = q_{(\vec{0}, \vec{b}, c)}
      deltaxr.stream()
          .filter(
              rEntry -> delta.stream().anyMatch(id -> rEntry.getAssociatedIndices().get(id) != 0))
          .forEach(
              rEntry -> {
                r.getSecond()
                    .add(
                        new EqualityConstraint(
                            rEntry.getValue(),
                            gammaDeltaQ.getCoefficientOrZero(
                                rEntry.mask(
                                    id ->
                                        (varsForGamma.contains(id) || id.equals(x))
                                            ? 0
                                            : rEntry.getAssociatedIndices().get(id)))));
              });
    } else {
      // Then, all constraints that are generated for e1 : A with A != T.
      // r_{(\vec{b}, c)} = q_{(\vec{0}, \vec{b}, c)}
      // Note that here we do not want \vec{b} \neq \vec{0}.
      deltaxr.stream()
          .forEach(
              rEntry -> {
                r.getSecond()
                    .add(
                        new EqualityConstraint(
                            rEntry.getValue(),
                            gammaDeltaQ.getCoefficientOrZero(
                                id ->
                                    varsForGamma.contains(id)
                                        ? 0
                                        : rEntry.getAssociatedIndices().get(id),
                                rEntry.getOffsetIndex())));
              });
    }

    return new Rule.ApplicationResult(List.of(p, r), crossConstraints);
  }
}
