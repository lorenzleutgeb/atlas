package xyz.leutgeb.lorenz.logs.ast;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Data;
import lombok.NonNull;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.FunctionType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
public class CallExpression extends Expression {
  @NonNull private final Identifier name;
  @NonNull private final List<Expression> parameters;

  public Type infer(Context context) throws UnificationError, TypeError {
    List<Type> xTy = new ArrayList<>(parameters.size());
    for (Expression parameter : parameters) {
      xTy.add(parameter.infer(context));
    }
    Type fTy = this.name.infer(context);
    var result = context.getProblem().fresh();
    context.getProblem().add(fTy, new FunctionType(xTy, result));
    return result;
  }

  @Override
  public String toString() {
    return "(call "
        + name.getName()
        + " ["
        + parameters.stream().map(Object::toString).collect(Collectors.joining(", "))
        + "])";
  }
}
