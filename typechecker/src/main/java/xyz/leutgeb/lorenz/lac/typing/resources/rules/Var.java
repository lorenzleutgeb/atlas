package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;

import java.util.Map;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualsSumConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;

public class Var implements Rule {
  @Override
  public RuleApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    final var expression = (Identifier) obligation.getExpression();
    final var name = expression.getName();
    final var context = obligation.getContext();

    // Three cases
    //  - leaf
    //    see rule (leaf)
    //  - tree variables
    //    return some annotation
    //  - non-tree variables or constants (like true/false)
    //    return a zero-valued annotation
    if (LEAF.getName().equals(name)) {
      if (!context.isEmpty()) {
        throw bug("have something in context for leaf");
      }
      var q = context.getAnnotation();
      var qp = obligation.getAnnotation();

      // q_{(c)} = Σ_{a + b = c} q'_{(a, b)}
      return RuleApplicationResult.onlyConstraints(
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
    } else if (expression.getType() instanceof TreeType) {
      if (context.size() != 1) {
        throw bug("variable rule expects context with exactly one element for tree identifier");
      }
      return RuleApplicationResult.onlyConstraints(
          EqualityConstraint.eq(obligation.getAnnotation(), context.getAnnotation()));
    } else {
      if (!context.isEmpty()) {
        throw bug("got nonempty context for nontree variable");
      }
      return RuleApplicationResult.empty();
    }
  }
}
