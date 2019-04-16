package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public abstract class Expression extends Syntax {

  public Expression(Source source) {
    super(source);
  }

  public abstract Type infer(Context context) throws UnificationError, TypeError;

  public abstract Expression normalize(Stack<Pair<Identifier, Expression>> context);

  public boolean isImmediate() {
    return false;
  }

  public AnnotatedType inferAnnotations(Context context) throws UnificationError, TypeError {
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
