package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
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
    final var expression = obligation.getExpression();

    if (!(expression instanceof Identifier)) {
      throw bug("cannot apply (var) to expression that is not an identifier");
    }

    /* DANGER ZONE
    if (!(expression.getType() instanceof TreeType)) {
      if (!context.isEmpty()) {
        throw bug(
                "cannot apply (var) to identifier that is not of type tree with nonempty context");
      }
      log.warn("NOT CONSTRAINING IN VAR");
      return Rule.ApplicationResult.empty();
    }
     */

    final var id = (Identifier) expression;

    if (id.getType() instanceof TreeType) {
      if (context.size() != 1) {
        throw bug("cannot apply (var) when context does not have exactly one element");
      }
      if (!context.getIds().get(0).equals(id)) {
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

  @Override
  public String getName() {
    return "var";
  }
}
