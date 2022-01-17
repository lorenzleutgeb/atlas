package xyz.leutgeb.lorenz.atlas.ast;

import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.expressions.Expression;
import xyz.leutgeb.lorenz.atlas.util.Util;

@Value
@EqualsAndHashCode
public class SourceIntro implements Intro {
  public @NonNull String fqn;
  public Expression expression;

  @Override
  public String toString() {
    return fqn + "_" + Util.stamp(expression);
  }
}
