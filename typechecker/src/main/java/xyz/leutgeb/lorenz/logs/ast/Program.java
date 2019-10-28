package xyz.leutgeb.lorenz.logs.ast;

import static guru.nidi.graphviz.model.Factory.graph;

import guru.nidi.graphviz.engine.Engine;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.Getter;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.Constraints;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

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
      ctx.putSignature(fd.getFullyQualifiedName(), fd.infer(ctx.child()));
    }
  }

  public void solve() throws UnificationError, TypeError {
    final var name =
        functionDefinitions.stream()
            .map(FunctionDefinition::getName)
            .collect(Collectors.joining("+"));
    final var constraints = new Constraints(name);

    unshare();
    for (var fd : functionDefinitions) {
      try {
        fd.printTo(new PrintStream(new FileOutputStream(fd.getName() + "-unshared.ml")));
      } catch (FileNotFoundException e) {
        e.printStackTrace();
      }
    }

    var functionAnnotations = new HashMap<String, Pair<Annotation, Annotation>>();
    for (var fd : functionDefinitions) {
      // inferAnnotation will add itself to functionAnnotations
      fd.infer(functionAnnotations, constraints);
      System.out.println(fd.getFullyQualifiedName());
    }

    System.out.println(constraints.size());
    constraints.solve();

    for (var fd : functionDefinitions) {
      fd.printAnnotation(System.out);
      try {
        fd.toGraph();
      } catch (IOException e) {
        e.printStackTrace();
      }
      fd.substitute(constraints);
      fd.printAnnotation(System.out);
    }

    Graph g =
        constraints.toGraph(
            graph(name).directed() /*.graphAttr().with(Rank.RankDir.BOTTOM_TO_TOP)*/);
    var viz = Graphviz.fromGraph(g);
    try {
      viz.engine(Engine.CIRCO)
          .render(Format.SVG)
          // TODO(lorenzleutgeb): Remove hardcoded path here.
          .toFile(new File(new File("..", "out"), name + "-constraints.svg"));
    } catch (IOException e) {
      e.printStackTrace();
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

  public void unshare() {
    for (var fd : functionDefinitions) {
      fd.unshare();
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
