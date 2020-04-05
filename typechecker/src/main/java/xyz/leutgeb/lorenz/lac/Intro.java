package xyz.leutgeb.lorenz.lac;

import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.ast.Expression;

@Value
public class Intro {
  @NonNull String fqn;
  Expression expression;

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    Intro intro = (Intro) o;

    if (!fqn.equals(intro.fqn)) {
      return false;
    }
    return expression != null ? expression.equals(intro.expression) : intro.expression == null;
  }

  @Override
  public int hashCode() {
    int result = fqn.hashCode();
    result = 31 * result + (expression != null ? expression.hashCode() : 0);
    return result;
  }
}
