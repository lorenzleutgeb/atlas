package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@RequiredArgsConstructor
public class Program {
  private Map<FunctionDefinition, Type> signature;

  @Getter private final List<FunctionDefinition> functionDefinitions;

  public Map<FunctionDefinition, Type> getSimpleSignature() throws UnificationError, TypeError {
    if (signature == null) {
      signature = new HashMap<>(functionDefinitions.size());
      var ctx = Context.root();
      for (var fd : functionDefinitions) {
        var sub = ctx.child();
        fd = fd.normalize();
        Type t = fd.infer(sub);
        signature.put(fd, t);
        ctx.put(fd.getName(), t);
      }
    }
    return signature;
  }

  public Map<FunctionDefinition, Type> getSignature() throws UnificationError, TypeError {
    if (signature == null) {
      signature = new HashMap<>(functionDefinitions.size());
      var ctx = Context.root();
      for (var fd : functionDefinitions) {
        var sub = ctx.child();
        fd = fd.normalize();
        Type t = fd.infer(sub);
        var at = fd.inferAnnotation(sub);
        signature.put(fd, t);
        ctx.put(fd.getName(), t);
      }
    }
    return signature;
  }

  public void printTo(PrintStream out) {
    try {
      for (var entry : getSimpleSignature().entrySet()) {
        var name = entry.getKey().getName();
        out.println(name + " : " + entry.getValue());
        entry.getKey().printTo(out);
      }
    } catch (UnificationError | TypeError e) {
      throw new RuntimeException(e);
    }
  }

  public void normalize() {
    for (int i = 0; i < functionDefinitions.size(); i++) {
      functionDefinitions.set(i, functionDefinitions.get(i).normalize());
    }
  }
}
