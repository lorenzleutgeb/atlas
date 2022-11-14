package xyz.leutgeb.lorenz.atlas.ast.expressions;

import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeClass;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeConstraint;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Value
@EqualsAndHashCode(callSuper = true)
public class NumberExpression extends Expression {
  @NonNull String number;

  public NumberExpression(String number, Source source) {
    super(source);
    this.number = number;
  }

  @Override
  protected Stream<? extends Expression> getChildren() {
    return Stream.of();
  }

  @Override
  protected Type inferInternal(UnificationContext context) throws TypeError {
    var ty = context.fresh();
    context
        .getSignature(context.getFunctionInScope(), source)
        .addConstraint(new TypeConstraint(TypeClass.NUM, ty));
    return ty;
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }

  @Override
  public boolean isImmediate() {
    return true;
  }
}
