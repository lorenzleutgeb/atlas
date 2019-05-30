package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;
import static xyz.leutgeb.lorenz.logs.ast.Identifier.NIL;

import java.io.PrintStream;
import java.util.Arrays;
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
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
@EqualsAndHashCode(callSuper = true)
@Log4j2
public class MatchExpression extends Expression {
  private final Expression test;

  // Invariant: size() == 2, asserted in constructor
  private final List<Pair<Expression, Expression>> cases;

  public MatchExpression(Source source, Expression test, List<Pair<Expression, Expression>> cases) {
    super(source);
    this.test = test;
    this.cases = cases;
    Optional<Pair<Expression, Expression>> nilCase =
        cases.stream().filter(x -> x.getKey().equals(Identifier.nil())).findAny();
    if (nilCase.isEmpty() && this.cases.size() == 1) {
      // log.info("Adding case `nil -> nil` to match " + source);
      this.cases.add(new Pair<>(Identifier.nil(), Identifier.nil()));
    }
    if (this.cases.size() != 2) {
      throw new IllegalArgumentException(
          "exactly 2 cases are required for a match, however "
              + this.cases.size()
              + " were encountered");
    }
  }

  private static Pair<Expression, Expression> normalize(Pair<Expression, Expression> pair) {
    if (pair.getKey() instanceof Tuple
        && !((Tuple) pair.getKey()).getElements().stream().allMatch(Expression::isImmediate)) {
      throw new UnsupportedOperationException("non-immediate patterns are not supported");
    }
    return new Pair<>(pair.getKey(), pair.getValue().normalizeAndBind());
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    var a = cases.stream().map(Pair::getValue);
    var b = cases.stream().map(Pair::getKey);
    return Stream.concat(Stream.of(test), Stream.concat(a, b));
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    final var result = context.getProblem().fresh();
    final var testType = context.getProblem().fresh();
    context.getProblem().add(this, testType, test.infer(context));

    for (Pair<Expression, Expression> it : cases) {
      var matcher = it.getKey();
      var body = it.getValue();

      // The matcher is an identifier. We therefore create a new context, add a signature variable
      // for it and decompose with the signature of test.
      if (matcher instanceof Identifier) {
        if (!NIL.equals(matcher)) {
          throw new RuntimeException("the only identifier allowed as destruction pattern is nil");
        }
        var id = (Identifier) matcher;
        var sub = context.child();
        var fresh = sub.getProblem().fresh();
        sub.putType(id.getName(), fresh);
        sub.getProblem().add(this, testType, matcher.infer(sub));
        sub.getProblem().add(this, result, body.infer(sub));
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
          sub.putType(name, fresh);
        }

        sub.getProblem().add(this, testType, it.getKey().infer(sub));
        sub.getProblem().add(this, result, it.getValue().infer(sub));
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
          source,
          test,
          cases.stream().map(MatchExpression::normalize).collect(Collectors.toList()));
    }

    return new MatchExpression(
        Derived.anf(source),
        test.forceImmediate(context),
        cases.stream().map(MatchExpression::normalize).collect(Collectors.toList()));
  }

  @Override
  public Annotation inferAnnotations(Context context) throws UnificationError, TypeError {
    if (cases.size() != 2) {
      throw new IllegalStateException("must have exactly two cases");
    }

    if (!(getType() instanceof TreeType)) {
      // This seems to be wrong
      return Annotation.EMPTY;
    }

    final var nilFirst = cases.get(0).getKey().equals(NIL);
    final var nilCase = cases.get(nilFirst ? 0 : 1);
    final var nodeCase = cases.get(nilFirst ? 1 : 0);

    if (nilCase == null || nodeCase == null) {
      throw new IllegalStateException("case missing");
    }

    final var scrutinee = (Identifier) test;
    final var pattern = (Tuple) nodeCase.getKey();
    final var x1 = (Identifier) pattern.getLeft();
    final var x3 = (Identifier) pattern.getRight();

    // nil
    var nilqp = nilCase.getValue().inferAnnotations(context);
    if (nilqp.size() != 1) {
      throw new RuntimeException("annotation of nil case must have size exactly one");
    }

    // node
    var sub = context.child();
    for (var e : pattern.getElements()) {
      sub.putAnnotation(((Identifier) e).getName(), sub.getConstraints().heuristic(1));
    }
    var nodeqp = nodeCase.getValue().inferAnnotations(sub);
    if (nodeqp.size() != 1) {
      throw new RuntimeException("annotation of node case must have size exactly one");
    }

    context.getConstraints().eq(nilqp, nodeqp);

    // TODO: r_{\vec{a}, a, a, b} = q_{\vec{a}, a, b}
    // TODO: p_{\vec{a},c} = \Sigma_{a+b=c} q_{\vec{a}, a, c}

    // r_{m+1} = r_{m+2} = q_{m+1}
    sub.getConstraints()
        .eq(
            sub.lookupAnnotation(x1.getName()).getRankCoefficient(),
            sub.lookupAnnotation(x3.getName()).getRankCoefficient(),
            sub.lookupAnnotation(scrutinee.getName()).getRankCoefficient());

    // r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}
    sub.getConstraints()
        .eq(
            sub.lookupAnnotation(x1.getName()).getCoefficients().get(Arrays.asList(1, 0)),
            sub.lookupAnnotation(x3.getName()).getCoefficients().get(Arrays.asList(1, 0)),
            sub.lookupAnnotation(scrutinee.getName()).getRankCoefficient());

    return nilqp;
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("match ");
    test.printTo(out, indentation);
    out.println(" with");

    for (int i = 0; i < cases.size(); i++) {
      var item = cases.get(i);
      if (item.getKey().equals(NIL) && item.getValue().equals(NIL)) {
        continue;
      }
      indent(out, indentation);
      out.print("| ");
      item.getKey().printTo(out, indentation + 1);
      out.print(" -> ");
      item.getValue().printTo(out, indentation + 1);
      if (i < cases.size() - 1) {
        out.println();
      }
    }
  }

  @Override
  public String toString() {
    return "match " + test.toString();
  }
}
