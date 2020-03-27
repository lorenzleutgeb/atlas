package xyz.leutgeb.lorenz.lac.ast;

import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.NumType;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

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
  protected xyz.leutgeb.lorenz.lac.typing.simple.types.Type inferInternal(
      UnificationContext context) throws UnificationError, TypeError {
    return NumType.INSTANCE;
  }

  @Override
  public Expression normalize(
      Stack<Pair<Identifier, Expression>> context, IntIdGenerator idGenerator) {
    return this;
  }
}
