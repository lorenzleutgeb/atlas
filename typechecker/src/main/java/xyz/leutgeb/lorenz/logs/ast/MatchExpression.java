package xyz.leutgeb.lorenz.logs.ast;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.logs.Util.indent;
import static xyz.leutgeb.lorenz.logs.ast.Identifier.NIL;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.log4j.Log4j2;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Predefined;
import xyz.leutgeb.lorenz.logs.ast.sources.Renamed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
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

  public MatchExpression(
      Source source, Expression test, List<Pair<Expression, Expression>> cases, Type type) {
    super(source, type);
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
      throw new IllegalArgumentException("non-immediate patterns are not supported");
    }
    return new Pair<>(pair.getKey(), pair.getValue().normalizeAndBind());
  }

  private static Pair<Expression, Expression> rename(
      Pair<Expression, Expression> cc, Map<String, String> renaming) {
    return new Pair<>(cc.getFirst(), cc.getSecond().rename(renaming));
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.concat(
        Stream.of(test), Stream.concat(cases.stream().map(Pair::getKey), follow()));
  }

  public Stream<? extends Expression> follow() {
    return cases.stream().map(Pair::getValue);
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    final var result = context.getProblem().fresh();
    final var testType = context.getProblem().fresh();
    context.getProblem().addIfNotEqual(this, testType, test.infer(context).wiggle(context));

    for (Pair<Expression, Expression> it : cases) {
      var matcher = it.getKey();
      var body = it.getValue();

      // The matcher is an identifier. We therefore create a new context, add a signature variable
      // for it and deconstruct with the signature of test.
      if (matcher instanceof Identifier) {
        if (!NIL.equals(matcher)) {
          throw new RuntimeException("the only identifier allowed as destruction pattern is nil");
        }
        var id = (Identifier) matcher;
        var sub = context.hide(((Identifier) test).getName());
        var fresh = sub.getProblem().fresh();
        sub.putType(id.getName(), fresh);
        sub.getProblem().addIfNotEqual(this, testType, matcher.infer(sub).wiggle(sub));
        sub.getProblem().addIfNotEqual(this, result, body.infer(sub).wiggle(sub));
      } else if (matcher instanceof Tuple) {
        var tuple = (Tuple) matcher;

        if (tuple.getElements().size() != 3) {
          throw new UnsupportedOperationException();
        }

        var sub = context.hide(((Identifier) test).getName());
        for (int i = 0; i < 3; i++) {
          if (!(tuple.getElements().get(i) instanceof Identifier)) {
            throw new UnsupportedOperationException();
          }
          String name = ((Identifier) tuple.getElements().get(i)).getName();
          var fresh = sub.getProblem().fresh();
          sub.putType(name, fresh);
        }

        sub.getProblem().addIfNotEqual(this, testType, it.getKey().infer(sub).wiggle(sub));
        sub.getProblem().addIfNotEqual(this, result, it.getValue().infer(sub).wiggle(sub));
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
  public MatchExpression rename(Map<String, String> renaming) {
    return new MatchExpression(
        new Renamed(source),
        test.rename(renaming),
        cases.stream().map(e -> rename(e, renaming)).collect(Collectors.toList()),
        type);
  }

  @Override
  public Annotation inferAnnotationsInternal(AnnotatingContext gammaxq, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    if (cases.size() != 2) {
      throw new IllegalStateException("must have exactly two cases");
    }

    if (!(getType() instanceof TreeType)) {
      return Annotation.empty();
    }

    final var constraints = globals.getConstraints();

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

    final var gammap = gammaxq.pop(constraints, scrutinee);

    // nil
    var nilqp = nilCase.getValue().inferAnnotations(gammap, globals);
    if (nilqp.size() != 1) {
      throw new RuntimeException("annotation of nil case must have size exactly one");
    }

    // \Gamma, x_1 : T, x_2 : B, x_3 : T  (NOTE: x_2 is skipped since it is not a tree).
    final var gammaxsr = gammaxq.pop(constraints, scrutinee).extend(constraints, x1, x3);

    // node (also called "e_2" in the paper)
    var nodeqp = nodeCase.getValue().inferAnnotations(gammaxsr, globals);
    if (nodeqp.size() != 1) {
      throw new RuntimeException("annotation of node case must have size exactly one");
    }

    // Both e_1 and e_2 are annotated with Q' in the paper, which we cannot
    // directly express but equate the results.
    constraints.eq(this, nilqp, nodeqp);

    // r_{m+1} = r_{m+2} = q_{m+1}
    // m refers to the size of \Gamma
    // r_{m+1} refers to x_1
    // r_{m+2} refers to x_3
    constraints.eq(
        this,
        gammaxsr.getRankCoefficient(x1.getName()),
        gammaxsr.getRankCoefficient(x3.getName()),
        gammaxq.getRankCoefficient(scrutinee.getName()));

    // r_{(\vec{0}, 1, 0, 0)} = r_{(\vec{0}, 0, 1, 0)} = q_{m+1}
    // To implement this constraint we use two indices defined as function that set the
    // index for all variables except x_1/x_3 to zero. That way we get the above selection.
    // Again m refers to the size of \Gamma, thus q_{m+1} refers to the coefficient of the
    // scrutinee.
    final Function<String, Integer> index1 = (String name) -> name.equals(x1.getName()) ? 1 : 0;
    final Function<String, Integer> index2 = (String name) -> name.equals(x3.getName()) ? 1 : 0;

    constraints.eq(
        this,
        gammaxsr.getCoefficient(index1, 0),
        gammaxsr.getCoefficient(index2, 0),
        gammaxq.getRankCoefficient(scrutinee.getName()));

    // r_{\vec{a}, a, a, b} = q_{\vec{a}, a, b}
    // TODO: Clarify whether \vec{a} should really look like a, a, a, ...
    gammaxsr
        .streamIndices()
        .filter(
            index -> {
              final var a1 = index.getFirst().get(x1.getName());
              final var a2 = index.getFirst().get(x3.getName());
              return a1 != null && a1.equals(a2);

              // This would filter for all a to be the same.
              // return new HashSet<>(index.getFirst().values()).size() == 1;
            })
        .forEach(
            index -> {
              // Here we use the coefficient of x1, but could just as well use x3. The
              // previous filter ensures that they are the same.
              index.getFirst().put(scrutinee.getName(), index.getFirst().get(x1.getName()));
              constraints.eq(this, gammaxsr.getCoefficient(index), gammaxq.getCoefficient(index));
            });

    // p_{\vec{a}, c} = \Sigma_{a+b=c} q_{\vec{a}, a, b}
    gammap
        .streamIndices()
        .forEach(
            indexLeft -> {
              final List<Coefficient> sum =
                  gammaxq
                      .streamIndices()
                      .filter(
                          indexRight ->
                              indexLeft
                                  .getSecond()
                                  .equals(
                                      indexRight.getSecond()
                                          + indexRight.getFirst().get(scrutinee.getName())))
                      .map(gammaxq::getCoefficient)
                      .collect(Collectors.toList());

              if (!sum.isEmpty()) {
                constraints.eqSum(this, gammap.getCoefficient(indexLeft), sum);
              }
            });

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
  public void printHaskellTo(PrintStream out, int indentation) {
    out.print("case ");
    test.printTo(out, indentation);
    out.println(" of");

    for (int i = 0; i < cases.size(); i++) {
      var item = cases.get(i);
      indent(out, indentation);
      item.getKey().printHaskellTo(out, indentation + 1);
      out.print(" -> ");
      item.getValue().printHaskellTo(out, indentation + 1);
      if (i < cases.size() - 1) {
        out.println();
      }
    }
  }

  @Override
  public String toString() {
    return "match " + test.toString();
  }

  @Override
  public Set<String> freeVariables() {
    final var result = super.freeVariables();
    result.removeAll(cases.get(0).getKey().freeVariables());
    result.removeAll(cases.get(1).getKey().freeVariables());
    return result;
  }

  @Override
  public Expression unshare(Map<String, Integer> unshared) {
    final var c0 = cases.get(0).getSecond();
    final var c1 = cases.get(1).getSecond();

    if (!(test instanceof Identifier)) {
      throw new IllegalStateException("anf required");
    }

    final var testName = ((Identifier) test).getName();

    Set<String> free0 = c0.freeVariables();
    Set<String> free1 = c1.freeVariables();

    if (free0.contains(testName) || free1.contains(testName)) {
      throw new IllegalStateException(
          "test variable is destructed, so it cannot occur freely in any case expression");
    }

    Sets.SetView<String> intersection = intersection(free1, free0);

    if (intersection.isEmpty()) {
      return new MatchExpression(
          source,
          test,
          List.of(
              Pair.create(cases.get(0).getFirst(), c0.unshare(unshared)),
              Pair.create(cases.get(1).getFirst(), c1.unshare(unshared))),
          type);
    }

    Identifier up = new Identifier(Predefined.INSTANCE, intersection.iterator().next());
    var down = ShareExpression.clone(up, unshared);
    var result = ShareExpression.rename(up, down, Pair.create(c0, c1));

    var replacement =
        new ShareExpression(
            source,
            up,
            down,
            new MatchExpression(
                source,
                test,
                List.of(
                    Pair.create(cases.get(0).getFirst(), result.getFirst().unshare(unshared)),
                    Pair.create(cases.get(1).getFirst(), result.getSecond().unshare(unshared))),
                type));

    return intersection.size() > 1 ? replacement.unshare(unshared) : replacement;
  }
}
