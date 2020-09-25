package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Leaf implements Rule {
  public static final Leaf INSTANCE = new Leaf();

  public Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    if (!LEAF.equals(obligation.getExpression())) {
      throw bug("cannot apply (leaf) to identifier that is not 'leaf'");
    }

    final var context = obligation.getContext();
    if (!context.isEmpty()) {
      throw bug("have something in context for leaf");
    }

    var qplus1 = context.getAnnotation();
    var qp = obligation.getAnnotation();

    var q = globals.getHeuristic().generate("qplus1minus1", qplus1);

    // q_{(c)} = Σ_{a + b = c} q'_{(a, b)}
    return Rule.ApplicationResult.onlyConstraints(
        Stream.concat(
                qplus1.increment(q, obligation.getCost(), "(leaf) Q + 1").stream(),
                q.streamCoefficients()
                    .flatMap(
                        qEntry -> {
                          List<Coefficient> sum =
                              qp.streamCoefficients()
                                  .filter(
                                      qpEntry -> {
                                        final var index = qpEntry.getKey();
                                        final var a = index.get(0);
                                        final var b = index.get(1);
                                        final var c = qEntry.getKey().get(0);
                                        return a + b == c;
                                      })
                                  .map(Map.Entry::getValue)
                                  .collect(Collectors.toList());
                          if (!sum.isEmpty()) {
                            return Stream.of(
                                new EqualsSumConstraint(
                                    qEntry.getValue(),
                                    sum,
                                    "(leaf) q_{(c)} = Σ_{a + b = c} q'_{(a, b)}"));
                          } else {
                            return Stream.empty();
                          }
                        }))
            .collect(Collectors.toList()));
  }

  @Override
  public String getName() {
    return "leaf";
  }
}
