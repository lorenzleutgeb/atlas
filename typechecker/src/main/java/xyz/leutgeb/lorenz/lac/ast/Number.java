package xyz.leutgeb.lorenz.lac.ast;

import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;

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
    throw bug("numbers are not implemented");
    // return NumType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    return this;
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }
}
