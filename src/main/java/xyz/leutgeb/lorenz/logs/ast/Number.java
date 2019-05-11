package xyz.leutgeb.lorenz.logs.ast;

import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.NumType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
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
  protected Type inferInternal(Context context) throws UnificationError, TypeError {
    return NumType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return this;
  }

  @Override
  public Object evaluate(Context context) {
    return value;
  }
}
