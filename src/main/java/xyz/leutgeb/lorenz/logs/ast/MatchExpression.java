package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.stream.Collectors;
import lombok.Data;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
@Log4j2
public class MatchExpression extends Expression {
  private final Expression test;
  private final List<Case> cases;

  public MatchExpression(Source source, Expression test, List<Case> cases) {
    super(source);
    this.test = test;
    this.cases = cases;
    Optional<Case> nilCase =
        cases.stream().filter(x -> x.getMatcher().equals(Identifier.NIL)).findAny();
    if (nilCase.isEmpty()) {
      log.info("Adding case `nil -> nil` to match " + source);
      this.cases.add(new Case(Derived.desugar(source), Identifier.NIL, Identifier.NIL));
    }
  }

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    final var result = context.getProblem().fresh();
    final var testType = context.getProblem().fresh();
    context.getProblem().add(testType, test.infer(context));

    for (Case it : cases) {
      var matcher = it.getMatcher();
      var body = it.getBody();

      // The matcher is an identifier. We therefore create a new context, add a type variable for it
      // and decompose with the type of test.
      if (matcher instanceof Identifier) {
        var id = (Identifier) matcher;
        Context sub = new Context(context);
        var fresh = sub.getProblem().fresh();
        sub.put(id.getName(), fresh);
        sub.getProblem().add(testType, matcher.infer(sub));
        sub.getProblem().add(result, body.infer(sub));
      } else if (matcher instanceof Tuple) {
        var tuple = (Tuple) matcher;

        if (tuple.getElements().size() != 3) {
          throw new UnsupportedOperationException();
        }

        Context sub = new Context(context);
        for (int i = 0; i < 3; i++) {
          if (!(tuple.getElements().get(i) instanceof Identifier)) {
            throw new UnsupportedOperationException();
          }
          String name = ((Identifier) tuple.getElements().get(i)).getName();
          var fresh = sub.getProblem().fresh();
          sub.put(name, fresh);
        }

        sub.getProblem().add(testType, it.getMatcher().infer(sub));
        sub.getProblem().add(result, it.getBody().infer(sub));
      } else {
        throw new UnsupportedOperationException();
      }
    }
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (test.isImmediate()) {
      return this;
    }

    Identifier id = Identifier.getSugar();
    context.push(new Pair<>(id, test.normalize(context)));

    return new MatchExpression(
        source, id, cases.stream().map(Case::normalize).collect(Collectors.toList()));
  }
}
