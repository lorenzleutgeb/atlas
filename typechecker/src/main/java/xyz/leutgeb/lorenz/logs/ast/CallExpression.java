package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.bug;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Renamed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
public class CallExpression extends Expression {
  @NonNull Identifier name;

  // after normalization, this is effectively a List<Identifier>
  @NonNull List<Expression> parameters;

  public CallExpression(
      Source source, @NonNull Identifier name, @NonNull List<Expression> parameters) {
    super(source);
    this.name = name;
    this.parameters = parameters;
  }

  public CallExpression(
      Source source, @NonNull Identifier name, @NonNull List<Expression> parameters, Type type) {
    super(source, type);
    this.name = name;
    this.parameters = parameters;
  }

  @Override
  public Set<String> freeVariables() {
    final var result = super.freeVariables();
    result.remove(name.getName());
    return result;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.concat(Stream.of(name), follow());
  }

  @Override
  public Stream<? extends Expression> follow() {
    return parameters.stream();
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
  public Annotation inferAnnotationsInternal(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    final var treeParameters =
        parameters
            .stream()
            .map(param -> (Identifier) param)
            .filter(param -> param.getType() instanceof TreeType)
            .collect(Collectors.toList());

    final var annotation = globals.getFunctionAnnotations().get(name.getName());
    final var q = annotation.getFirst();
    final var qprime = annotation.getSecond();

    // Apply (w : var) to truncate context.
    final var weakenedContext =
        context.weakenIdentifiersExcept(this, globals.getConstraints(), treeParameters);

    // Since we're in a leaf here, also apply (w).
    final var weakenedPotential =
        weakenedContext.getAnnotation().less(this, globals.getConstraints());

    globals.getConstraints().increment(this, q, weakenedPotential, globals.getCost());

    if (qprime.size() > 1) {
      throw bug("size should be at most one since we can return at most one tree!");
    }

    return qprime.greater(this, globals.getConstraints());
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
  public Expression rename(Map<String, String> renaming) {
    return new CallExpression(
        new Renamed(source),
        name,
        parameters.stream().map(x -> x.rename(renaming)).collect(Collectors.toList()),
        type);
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
