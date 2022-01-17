package xyz.leutgeb.lorenz.atlas.ast;

import lombok.AllArgsConstructor;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.expressions.Expression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;

@Value
@AllArgsConstructor
public class Normalization {
  public IdentifierExpression identifier;
  public Expression expression;
}
