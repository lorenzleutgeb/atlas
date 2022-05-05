package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.pick;

import java.util.Map;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.expressions.Expression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.TupleExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;

@Slf4j
public class Var implements Rule {
  public static final Var INSTANCE = new Var();

  public Rule.ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments) {
    final var context = obligation.getContext();
    final Expression expression = obligation.getExpression();

    Optional<IdentifierExpression> target;
    if (expression instanceof IdentifierExpression id) {
      target = id.getType() instanceof TreeType ? Optional.of(id) : Optional.empty();
    } else if (expression instanceof TupleExpression tuple) {
      target = tuple.getTree();
    } else {
      target = Optional.empty();
    }

    if (target.isPresent()) {
      if (context.size() != 1) {
        throw bug("cannot apply (var) when context does not have exactly one element");
      }
      if (!pick(context.getIds()).equals(target.get())) {
        throw bug("wrong variable in context");
      }
    } else {
      if (!context.isEmpty()) {
        throw bug(
            "cannot apply (var) to identifier that is not of type tree with nonempty context");
      }
    }

    return Rule.ApplicationResult.onlyConstraints(
        EqualityConstraint.eq(context.getAnnotation(), obligation.getAnnotation(), "(var)"));
  }
}
