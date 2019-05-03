package xyz.leutgeb.lorenz.logs.type;

import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Value;

@Value
public class FunctionSignature {
  FunctionType type;
  Set<TypeConstraint> constraints;

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (!constraints.isEmpty()) {
      final var omitParens = constraints.size() == 1;
      if (!omitParens) {
        sb.append("(");
      }
      sb.append(constraints.stream().map(Objects::toString).collect(Collectors.joining(", ")));
      if (!omitParens) {
        sb.append(")");
      }
      sb.append(" => ");
    }
    sb.append(type);
    return sb.toString();
  }
}
