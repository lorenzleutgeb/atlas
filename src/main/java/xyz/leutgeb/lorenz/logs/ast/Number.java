package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.NumType;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = false)
public class Number extends Expression {
  int value;

  public Number(Source source, int value) {
    super(source);
    this.value = value;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.empty();
  }

  @Override
  protected xyz.leutgeb.lorenz.logs.typing.types.Type inferInternal(Context context)
      throws UnificationError, TypeError {
    return NumType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return this;
  }

  @Override
  public Annotation inferAnnotations(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    return Annotation.empty();
  }

  @Override
  public Object evaluate(Context context) {
    return value;
  }
}
