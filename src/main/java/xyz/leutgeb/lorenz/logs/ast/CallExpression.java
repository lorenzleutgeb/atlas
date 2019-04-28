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
import xyz.leutgeb.lorenz.logs.type.FunctionType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
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
    List<Type> xTy = new ArrayList<>(parameters.size());
    for (Expression parameter : parameters) {
      xTy.add(parameter.infer(context));
    }
    Type fTy = this.name.infer(context).wiggle(new HashMap<>(), context.getProblem());
    var result = context.getProblem().fresh();
    context.getProblem().add(fTy, new FunctionType(xTy, result));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (parameters.stream().allMatch(Expression::isImmediate)) {
      return this;
    }

    final List<Expression> normalized = new ArrayList<>(parameters.size());
    for (Expression parameter : parameters) {
      if (parameter.isImmediate()) {
        normalized.add(parameter);
      }
      var id = Identifier.getSugar();
      normalized.add(id);
      context.push(new Pair<>(id, parameter.normalize(context)));
    }

    return new CallExpression(source, name, normalized);
  }

  @Override
  public String toString() {
    return "(call "
        + name.getName()
        + " ["
        + parameters.stream().map(Object::toString).collect(Collectors.joining(", "))
        + "])";
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
