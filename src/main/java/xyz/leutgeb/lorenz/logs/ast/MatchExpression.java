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
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
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
    Optional<Pair<Expression, Expression>> nilCase =
        cases.stream().filter(x -> x.getKey().equals(NIL)).findAny();
    Optional<Pair<Expression, Expression>> nodeCase =
        cases.stream().filter(x -> !x.getKey().equals(NIL)).findAny();

    if (nilCase.isEmpty() || nodeCase.isEmpty()) {
      // TODO(lorenz.leutgeb): This is a too restrictive and throws too much. The fault probably is
      // equality of nil identifiers. Investigate.
      throw new IllegalStateException("case missing");
    }

    if (!(nilCase.get().getValue().getType() instanceof TreeType)
        || !(nodeCase.get().getValue().getType() instanceof TreeType)) {
      throw new UnsupportedOperationException("(match) only implemented for e1 : T and e2 : T");
    }

    // We have exactly two cases inside this match, being the two constructors of trees.

    // Note that in the nil-case, there are no new variables being introduced, so we just pass on
    // context.
    var nilqp = nilCase.get().getValue().inferAnnotations(context);
    if (nilqp.size() != 1) {
      // TODO(lorenz.leutgeb): Waiting for Georg's response on how this is justified.
      throw new RuntimeException("annotation of nil case must have size exactly one");
    }

    // TODO(lorenz.leutgeb): Waiting for Georg's response on whether the size should be m + 2 or m +
    // 3.
    var sub = context.child();
    for (var e : ((Tuple) nodeCase.get().getKey()).getElements()) {
      sub.putAnnotation(((Identifier) e).getName(), sub.getConstraints().heuristic(1));
    }
    var nodeqp = nodeCase.get().getValue().inferAnnotations(sub);
    if (nodeqp.size() != 1) {
      throw new RuntimeException("annotation of node case must have size exactly one");
    }

    context.getConstraints().eq(nilqp, nodeqp);

    // r_{m+1} = r_{m+2} = q_{m+1}
    sub.getConstraints()
        .eq(
            sub.lookupAnnotation(
                    ((Identifier) ((Tuple) nodeCase.get().getKey()).getLeft()).getName())
                .getRankCoefficient(),
            sub.lookupAnnotation(
                    ((Identifier) ((Tuple) nodeCase.get().getKey()).getRight()).getName())
                .getRankCoefficient(),
            sub.lookupAnnotation(((Identifier) test).getName()).getRankCoefficient());

    // r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}
    sub.getConstraints()
        .eq(
            sub.lookupAnnotation(
                    ((Identifier) ((Tuple) nodeCase.get().getKey()).getLeft()).getName())
                .getCoefficients()
                .get(Arrays.asList(1, 0)),
            sub.lookupAnnotation(
                    ((Identifier) ((Tuple) nodeCase.get().getKey()).getRight()).getName())
                .getCoefficients()
                .get(Arrays.asList(1, 0)),
            sub.lookupAnnotation(((Identifier) test).getName()).getRankCoefficient());

    // TODO: r_{\vec{a}, a, a, b} = q_{\vec{a}, a, b}
    // TODO: p_{\vec{a},c} = \Sigma_{a+b=c} q_{\vec{a}, a, c}

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
