package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.union;
import static java.util.Collections.singleton;
import static xyz.leutgeb.lorenz.lac.Util.notImplemented;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

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

  private CallExpression(
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
  public Set<Identifier> freeVariables() {
    final var result = super.freeVariables();
    result.remove(name);
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

  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    FunctionSignature signature =
        context.getSignature(name.getName()).wiggle(new Substitution(), context);

    final var fTy = signature.getType();

    if (fTy.getFrom().getElements().size() != parameters.size()) {
      throw new TypeError();
    }

    List<Type> xTy = new ArrayList<>(parameters.size());
    for (Expression parameter : parameters) {
      xTy.add(parameter.infer(context));
    }

    name.infer(context);

    if (!name.getName().equals(context.getFunctionInScope())
        && !context.getSignatures().isEmpty()) {
      FunctionSignature functionSignature =
          context.getSignatures().get(context.getFunctionInScope());
      context
          .getSignatures()
          .put(
              context.getFunctionInScope(),
              new FunctionSignature(
                  Sets.union(functionSignature.getConstraints(), signature.getConstraints()),
                  functionSignature.getType()));
    }

    var result = context.fresh();
    context.addIfNotEqual(fTy, new FunctionType(xTy, result).wiggle(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (parameters.stream().allMatch(Expression::isImmediate)) {
      return this;
    }
    return new CallExpression(
        source,
        moduleName,
        name,
        parameters.stream()
            .map(x -> x.forceImmediate(context, idGenerator))
            .collect(Collectors.toList()));
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    // TODO: Only create new expression if really necessary!
    return new CallExpression(
        Derived.rename(this),
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
  public Set<String> getOccurringFunctions() {
    var fqn = name.getName();
    if (!fqn.contains(".")) {
      fqn = moduleName + "." + fqn;
    }

    // NOTE: In case this expression is not in LNF yet,
    // there might be more call expressions "hiding"
    // inside the parameters!
    return union(singleton(fqn), super.getOccurringFunctions());
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
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

    var down = ShareExpression.clone((Identifier) parameters.get(a), idGenerator);
    var newParameters = new ArrayList<>(parameters);
    newParameters.set(a, down.getLeft());
    newParameters.set(b, down.getRight());

    return new ShareExpression(
        this,
        (Identifier) parameters.get(a),
        down,
        new CallExpression(source, name, newParameters, type, moduleName));
  }

  @Override
  public boolean isTerminal() {
    return true;
  }
}
