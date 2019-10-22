package xyz.leutgeb.lorenz.logs.ast;

import java.util.Map;
import java.util.stream.Stream;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Predefined;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
public class ShareExpression extends Expression {
  Identifier up;
  Pair<Identifier, Identifier> down;
  Expression e;

  public ShareExpression(Identifier up, Pair<Identifier, Identifier> down, Expression e) {
    super(Predefined.INSTANCE);
    this.up = up;
    this.down = down;
    this.e = e;
    this.type = e.getType();
    this.typeResolved = e.typeResolved;
  }

  public ShareExpression(
      Source source, Identifier up, Pair<Identifier, Identifier> down, Expression e) {
    super(source);
    this.up = up;
    this.down = down;
    this.e = e;
    this.type = e.getType();
    this.typeResolved = e.typeResolved;
  }

  public ShareExpression(
      Source source, Type type, Identifier up, Pair<Identifier, Identifier> down, Expression e) {
    super(source, type);
    this.up = up;
    this.down = down;
    this.e = e;
    this.type = e.getType();
    this.typeResolved = e.typeResolved;
  }

  public static Pair<Identifier, Identifier> clone(Identifier identifier) {
    return new Pair<>(
        new Identifier(identifier.getSource(), identifier.getName() + "'"),
        new Identifier(identifier.getSource(), identifier.getName() + "''"));
  }

  public static Pair<Expression, Expression> rename(
      Identifier up, Pair<Identifier, Identifier> down, Pair<Expression, Expression> expressions) {
    return Pair.create(
        expressions.getFirst().rename(Map.of(up.getName(), down.getFirst().getName())),
        expressions.getSecond().rename(Map.of(up.getName(), down.getSecond().getName())));
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(e);
  }

  @Override
  protected Type inferInternal(Context context) throws UnificationError, TypeError {
    var sub = context.child();
    sub.putType(down.getFirst().getName(), up.infer(sub));
    sub.putType(down.getSecond().getName(), up.infer(sub));
    return e.infer(sub);
  }

  @Override
  protected Annotation inferAnnotationsInternal(
      AnnotatingContext context, AnnotatingGlobals globals) throws UnificationError, TypeError {
    return e.inferAnnotationsInternal(context, globals);
  }
}
