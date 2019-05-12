package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;

import java.io.PrintStream;
import java.util.Stack;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public abstract class Expression extends Syntax {
  protected Type type;
  protected boolean typeResolved;

  public Expression(Source source) {
    super(source);
  }

  public abstract Stream<? extends Expression> getChildren();

  protected abstract Type inferInternal(Context context) throws UnificationError, TypeError;

  public Object evaluate(Context context) {
    throw new UnsupportedOperationException();
  }

  public Type infer(Context context) throws UnificationError, TypeError {
    if (this.type == null) {
      this.type = inferInternal(context);
    }
    return this.type;
  }

  public void resolveType(Substitution substitution) {
    typeResolved = true;
    this.type = substitution.apply(this.type);
    getChildren().forEach(x -> x.resolveType(substitution));
  }

  public @Nonnull Type getType() {
    if (type == null) {
      throw new IllegalStateException("type has not been inferred yet");
    }
    return type;
  }

  public boolean isTypeResolved() {
    return typeResolved;
  }

  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (isImmediate()) {
      return this;
    }
    throw new UnsupportedOperationException("not implemented");
  }

  public boolean isImmediate() {
    return false;
  }

  public Annotation inferAnnotations(Context context) throws UnificationError, TypeError {
    throw new UnsupportedOperationException("not implemented");
  }

  public Expression bindAll(Stack<Pair<Identifier, Expression>> context) {
    var binder = this;
    while (!context.isEmpty()) {
      final var binding = context.pop();
      binder =
          new LetExpression(Derived.anf(source), binding.getFirst(), binding.getSecond(), binder);
    }
    return binder;
  }

  public Expression forceImmediate(Stack<Pair<Identifier, Expression>> context) {
    if (isImmediate()) {
      return this;
    }
    var id = Identifier.getSugar();
    context.push(new Pair<>(id, normalize(context)));
    return id;
  }

  protected Expression normalizeAndBind() {
    var context = new Stack<Pair<Identifier, Expression>>();
    return normalize(context).bindAll(context);
  }

  public void printTo(PrintStream out, int indentation) {
    indent(out, indentation);
    out.println(toString());
  }
}
