package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
public class CallExpression extends Expression {
  @NonNull Identifier name;
  @NonNull List<Expression> parameters;

  public CallExpression(
      Source source, @NonNull Identifier name, @NonNull List<Expression> parameters) {
    super(source);
    this.name = name;
    this.parameters = parameters;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.concat(Stream.of(name), parameters.stream());
  }

  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var fTy = (FunctionType) name.infer(context).wiggle(new HashMap<>(), context.getProblem());
    if (fTy.getFrom().getElements().size() != parameters.size()) {
      throw new TypeError();
    }
    List<Type> xTy = new ArrayList<>(parameters.size());
    for (int i = 0; i < parameters.size(); i++) {
      Expression parameter = parameters.get(i);
      xTy.add(parameter.infer(context));
      context.getProblem().add(this, fTy.getFrom().getElements().get(i), parameter.infer(context));
    }
    var result = context.getProblem().fresh();
    context.getProblem().add(this, fTy, new FunctionType(xTy, result));
    return result;
  }

  @Override
  public Annotation inferAnnotations(Context context) throws UnificationError, TypeError {
    var look = context.lookupFunctionAnnotation(name.getName());
    var q = look.getFirst();
    var qprime = look.getSecond();
    var qplusone = context.getConstraints().heuristic(q.size());
    context.getConstraints().increment(qplusone, q);
    return qprime;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (parameters.stream().allMatch(Expression::isImmediate)) {
      return this;
    }
    return new CallExpression(
        source,
        name,
        parameters.stream().map(x -> x.forceImmediate(context)).collect(Collectors.toList()));
  }

  @Override
  public String toString() {
    return name.getName()
        + " "
        + parameters.stream().map(Object::toString).collect(Collectors.joining(" "));
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    name.printTo(out, indentation);
    out.print(" ");

    for (int i = 0; i < parameters.size(); i++) {
      parameters.get(i).printTo(out, indentation);
      if (i < parameters.size() - 1) {
        out.print(" ");
      }
    }
  }
}
