package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import lombok.Getter;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.FunctionSignature;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

// @RequiredArgsConstructor
public class Program {
  @Getter private final List<FunctionDefinition> functionDefinitions;

  public Program(List<FunctionDefinition> functionDefinitions) {
    functionDefinitions.forEach(Objects::requireNonNull);
    this.functionDefinitions = functionDefinitions;
  }

  public void infer() throws UnificationError, TypeError {
    normalize();
    var ctx = Context.root();
    for (var fd : functionDefinitions) {
      var sub = ctx.child();
      FunctionSignature t = fd.infer(sub);
      ctx.putSignature(fd.getFullyQualifiedName(), t);
      // TODO: Fix this...
      ctx.putType(fd.getFullyQualifiedName(), t.getType());
    }
  }

  public void solve() throws UnificationError, TypeError {
    var functionAnnotations = new HashMap<String, Pair<Annotation, Annotation>>();
    for (var fd : functionDefinitions) {
      // inferAnnotation will add itself to functionAnnotations
      var at = fd.inferAnnotation(functionAnnotations);
    }
  }

  public void printTo(PrintStream out) {
    try {
      infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }
    for (var entry : functionDefinitions) {
      entry.printTo(out);
      out.println();
    }
  }

  public void normalize() {
    // Maybe let the Loader normalize directly?
    for (var fd : functionDefinitions) {
      fd.normalize();
    }
  }

  public void printHaskellTo(PrintStream out) {
    // TODO(lorenzleutgeb): It seems that `infer` is not idempotent. If we call it here,
    // we get strange results for the types of function definitions in this program.
    // That is wrong, since `infer` should indeed be idempotent.
    for (var entry : functionDefinitions) {
      entry.printHaskellTo(out);
      out.println();
    }
  }
}
