package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;

import java.io.PrintStream;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
@EqualsAndHashCode(callSuper = true)
@Log4j2
public class MatchExpression extends Expression {
  private final Expression test;

  // Invariant: size() == 2, asserted in constructor
  private final List<Case> cases;

  public MatchExpression(Source source, Expression test, List<Case> cases) {
    super(source);
    this.test = test;
    this.cases = cases;
    Optional<Case> nilCase =
        cases.stream().filter(x -> x.getMatcher().equals(Identifier.NIL)).findAny();
    if (nilCase.isEmpty() && this.cases.size() == 1) {
      log.info("Adding case `nil -> nil` to match " + source);
      this.cases.add(new Case(Derived.desugar(source), Identifier.NIL, Identifier.NIL));
    }
    if (this.cases.size() != 2) {
      throw new IllegalArgumentException(
          "exactly 2 cases are required for a match, however "
              + this.cases.size()
              + " were encountered");
    }
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    var a = cases.stream().map(Case::getBody);
    var b = cases.stream().map(Case::getMatcher);
    return Stream.concat(Stream.of(test), Stream.concat(a, b));
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
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
        var sub = context.child();
        var fresh = sub.getProblem().fresh();
        sub.put(id.getName(), fresh);
        sub.getProblem().add(testType, matcher.infer(sub));
        sub.getProblem().add(result, body.infer(sub));
      } else if (matcher instanceof Tuple) {
        var tuple = (Tuple) matcher;

        if (tuple.getElements().size() != 3) {
          throw new UnsupportedOperationException();
        }

        var sub = context.child();
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
      return new MatchExpression(
              source, test, cases.stream().map(Case::normalize).collect(Collectors.toList()));
    }

    Identifier id = Identifier.getSugar();
    context.push(new Pair<>(id, test.normalize(context)));

    return new MatchExpression(
        source, id, cases.stream().map(Case::normalize).collect(Collectors.toList()));
  }

  @Override
  public AnnotatedType inferAnnotations(Context context, Annotation typingContext)
      throws UnificationError, TypeError {
    if (cases.size() != 2) {
      throw new IllegalStateException("must have exactly two cases");
    }
    Optional<Case> nilCase =
        cases.stream().filter(x -> x.getMatcher().equals(Identifier.NIL)).findAny();
    Optional<Case> nodeCase =
        cases.stream().filter(x -> !x.getMatcher().equals(Identifier.NIL)).findAny();

    if (nilCase.isEmpty() || nodeCase.isEmpty()) {
      // TODO(lorenz.leutgeb): This is a too restrictive and throws too much. The fault probably is
      // equality of nil identifiers. Investigate.
      // throw new IllegalStateException("case missing");
    }

    // We have exactly two cases inside this match, being the two constructors of trees.

    var qp = nilCase.get().getBody().inferAnnotations(context, typingContext);
    if (qp.getAnnotation().getSize() != 1) {
      // TODO(lorenz.leutgeb): Waiting for Georg's response on how this is justified.
      throw new RuntimeException("annotation of nil case must have size exactly one");
    }

    // TODO(lorenz.leutgeb): Waiting for Georg's response on whether the size should be m + 2 or m +
    // 3.
    var sub = context.getConstraints().heuristic(typingContext.getSize() + 2);

    return super.inferAnnotations(context, typingContext);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("match ");
    test.printTo(out, indentation);
    out.println(" with");

    for (int i = 0; i < cases.size(); i++) {
      // TODO(lorenz.leutgeb): Skip "nil -> nil".
      var item = cases.get(i);
      indent(out, indentation);
      out.print("| ");
      item.getMatcher().printTo(out, indentation + 1);
      out.print(" -> ");
      item.getBody().printTo(out, indentation + 1);
      if (i < cases.size() - 1) {
        out.println();
      }
    }
  }
}
