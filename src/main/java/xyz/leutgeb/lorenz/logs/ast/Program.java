package xyz.leutgeb.lorenz.logs.ast;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@RequiredArgsConstructor
public class Program {
  private Map<FunctionDefinition, Type> signature;

  @Getter @Setter private final List<FunctionDefinition> functionDefinitions;

  public Map<FunctionDefinition, Type> getSignature() throws UnificationError, TypeError {
    if (signature == null) {
      signature = new HashMap<>(functionDefinitions.size());
      for (var fd : functionDefinitions) {
        var ctx = Context.root();
        signature.put(fd, fd.infer(ctx));
      }
    }
    return signature;
  }
}
