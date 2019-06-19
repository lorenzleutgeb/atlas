package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@RequiredArgsConstructor
public class Program {
  @Getter private final Map<String, FunctionDefinition> functionDefinitions;

  public void infer() throws UnificationError, TypeError {
    normalize();
    var ctx = Context.root();
    for (var e : functionDefinitions.entrySet()) {
      var sub = ctx.child();
      var fd = e.getValue();
      FunctionSignature t = fd.infer(sub);
      ctx.putType(fd.getName(), t.getType());
    }
  }

  public void solve() throws UnificationError, TypeError {
    var functionAnnotations = new HashMap<String, Pair<Annotation, Annotation>>();
    for (var e : functionDefinitions.entrySet()) {
      var fd = e.getValue();
      // inferAnnotation will add itself to functionAnnotations
      var at = fd.inferAnnotation(functionAnnotations);
    }
  }

  public void printTo(PrintStream out) {
    printTo(out, true);
  }

  public void printTo(PrintStream out, boolean printFunctionSignatures) {
    try {
      infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }
    for (var entry : functionDefinitions.entrySet()) {
      var name = entry.getKey();
      if (printFunctionSignatures) {
        out.print(name + " :: ");
        out.println(entry.getValue().getSignature());
      }
      entry.getValue().printTo(out);
      out.println();
    }
  }

  public void normalize() {
    functionDefinitions.replaceAll((k, v) -> v.normalize());
  }
}
