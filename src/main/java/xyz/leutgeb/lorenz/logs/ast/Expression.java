package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import java.util.stream.Stream;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
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

  public boolean isTypeResolved() {
    return typeResolved;
  }

  public abstract Expression normalize(Stack<Pair<Identifier, Expression>> context);

  public boolean isImmediate() {
    return false;
  }

  public AnnotatedType inferAnnotations(Context context, Annotation typingContext)
      throws UnificationError, TypeError {
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

  public Expression normalizeAndBind() {
    var context = new Stack<Pair<Identifier, Expression>>();
    return normalize(context).bindAll(context);
  }
}
