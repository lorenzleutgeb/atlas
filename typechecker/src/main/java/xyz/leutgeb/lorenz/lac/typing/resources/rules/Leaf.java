package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;

import java.util.Map;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public class Leaf {
  public static Rule.ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    if (!LEAF.equals(obligation.getExpression())) {
      throw bug("cannot apply (leaf) to identifier that is not 'leaf'");
    }

    final var context = obligation.getContext();
    if (!context.isEmpty()) {
      throw bug("have something in context for leaf");
    }

    var q = context.getAnnotation();
    var qp = obligation.getAnnotation();

    // q_{(c)} = Σ_{a + b = c} q'_{(a, b)}
    return Rule.ApplicationResult.onlyConstraints(
        q.streamCoefficients()
            .map(
                qEntry ->
                    new EqualsSumConstraint(
                        qEntry.getValue(),
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
                            .collect(Collectors.toList())))
            .collect(Collectors.toList()));
  }
}
