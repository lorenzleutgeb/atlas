package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.bug;
import static xyz.leutgeb.lorenz.logs.Util.notImplemented;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
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
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.FunctionType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
public class CallExpression extends Expression {
  @NonNull String moduleName;

  @NonNull Identifier name;

  // after normalization, this is effectively a List<Identifier>
  @NonNull List<Expression> parameters;

  public CallExpression(
      Source source,
      String moduleName,
      @NonNull Identifier name,
      @NonNull List<Expression> parameters) {
    super(source);
    this.moduleName = moduleName;
    this.name = name;
    this.parameters = parameters;
  }

  public CallExpression(
      Source source,
      @NonNull Identifier name,
      @NonNull List<Expression> parameters,
      Type type,
      String moduleName) {
    super(source, type);
    this.name = name;
    this.parameters = parameters;
    this.moduleName = moduleName;
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
    if (!context.hasSignature(name.getName())) {
      throw new TypeError();
    }

    List<Type> xTy = new ArrayList<>(parameters.size());
    for (int i = 0; i < parameters.size(); i++) {
      Expression parameter = parameters.get(i);
      xTy.add(parameter.infer(context));
    }

    FunctionSignature signature =
        context
            .getSignatures()
            .get(name.getName())
            .wiggle(new Substitution(), context.getProblem());

    final var fTy = signature.getType();
    name.infer(context);
    for (var typeConstraint : signature.getConstraints()) {
      context.getProblem().addConstraint(typeConstraint);
    }

    if (fTy.getFrom().getElements().size() != parameters.size()) {
      throw new TypeError();
    }

    for (int i = 0; i < parameters.size(); i++) {
      Expression parameter = parameters.get(i);
      context
          .getProblem()
          .addIfNotEqual(
              this, fTy.getFrom().getElements().get(i), parameter.infer(context).wiggle(context));
    }
    var result = context.getProblem().fresh();
    context.getProblem().addIfNotEqual(this, fTy, new FunctionType(xTy, result).wiggle(context));
    return result;
  }

  @Override
  public Annotation inferAnnotationsInternal(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    final var treeParameters =
        parameters.stream()
            .map(param -> (Identifier) param)
            .filter(param -> param.getType() instanceof TreeType)
            .map(Identifier::toString)
            .collect(Collectors.toSet());

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
        moduleName,
        name,
        parameters.stream().map(x -> x.forceImmediate(context)).collect(Collectors.toList()));
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return new CallExpression(
        new Renamed(source),
        name,
        parameters.stream().map(x -> x.rename(renaming)).collect(Collectors.toList()),
        type,
        moduleName);
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

  @Override
  public void printHaskellTo(PrintStream out, int indentation) {
    name.printHaskellTo(out, indentation);
    out.print(" ");

    for (int i = 0; i < parameters.size(); i++) {
      parameters.get(i).printHaskellTo(out, indentation);
      if (i < parameters.size() - 1) {
        out.print(" ");
      }
    }
  }

  @Override
  public Set<String> getOcurringFunctions() {
    var fqn = name.getName();
    if (!fqn.contains(".")) {
      fqn = moduleName + "." + fqn;
    }

    // NOTE: In case this expression is not in LNF yet,
    // there might be more call expressions "hiding"
    // inside the parameters!
    return Sets.union(Collections.singleton(fqn), super.getOcurringFunctions());
  }

  @Override
  public Expression unshare(Map<String, Integer> unshared) {
    boolean eqs = false;
    int a = -1, b = -1;
    for (int i = 0; i < parameters.size() - 1; i++) {
      if (!(parameters.get(i) instanceof Identifier)) {
        throw new IllegalStateException("must be in anf");
      }
      for (int j = i + 1; j < parameters.size(); j++) {
        if (!parameters.get(j).equals(parameters.get(i))) {
          continue;
        }
        a = i;
        b = j;
        if (!eqs) {
          eqs = true;
        } else {
          throw notImplemented("unsharing of call with more than one pair of equal parameters");
        }
      }
    }

    if (a == -1 || b == -1) {
      return this;
    }

    var down = ShareExpression.clone((Identifier) parameters.get(a), unshared);
    var newParameters = new ArrayList<>(parameters);
    newParameters.set(a, down.getFirst());
    newParameters.set(b, down.getSecond());

    return new ShareExpression(
        (Identifier) parameters.get(a),
        down,
        new CallExpression(source, name, newParameters, type, moduleName));
  }
}
