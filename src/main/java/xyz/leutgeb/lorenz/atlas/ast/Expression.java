package xyz.leutgeb.lorenz.atlas.ast;

import static guru.nidi.graphviz.attribute.Records.turn;
import static xyz.leutgeb.lorenz.atlas.util.Util.indent;
import static xyz.leutgeb.lorenz.atlas.util.Util.notImplemented;
import static xyz.leutgeb.lorenz.atlas.util.Util.rawObjectNode;

import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.io.PrintStream;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.Substitution;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;

// TODO(lorenzleutgeb): Maybe #canCarryPotential would be helpful? Expressions that only contain
// subexpressions of type Base/Bool and are of type Base/Bool cannot have any non-zero
// potential, so when normalizing/renaming they should be much simpler to handle.
public abstract class Expression extends Syntax {
  public static final boolean DEFAULT_LAZY = false;

  Type type;

  Expression(Source source) {
    super(source);
  }

  Expression(Source source, Type type) {
    super(source);
    this.type = type;
  }

  protected abstract Stream<? extends Expression> getChildren();

  Stream<? extends Expression> follow() {
    return getChildren();
  }

  protected abstract Type inferInternal(UnificationContext context)
      throws UnificationError, TypeError;

  public Type infer(UnificationContext context) throws UnificationError, TypeError {
    if (type == null) {
      type = inferInternal(context);
    }
    return type;
  }

  public void resolveType(Substitution substitution) {
    type = substitution.apply(type);
    getChildren().forEach(x -> x.resolveType(substitution));
  }

  public @Nonnull Type getType() {
    if (type == null) {
      throw new IllegalStateException("type has not been inferred yet");
    }
    return type;
  }

  Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (isImmediate()) {
      return this;
    }
    throw notImplemented("normalization of a concrete non-immediate expression type");
  }

  boolean isImmediate() {
    return false;
  }

  public boolean isTerminal() {
    return false;
  }

  Expression bindAll(Stack<Normalization> context) {
    var binder = this;
    while (!context.isEmpty()) {
      final var normalization = context.pop();
      binder =
          new LetExpression(
              Derived.anf(this), normalization.identifier, normalization.expression, binder);
    }
    return binder;
  }

  Expression forceImmediate(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (isImmediate()) {
      return this;
    }

    var id = Identifier.getSugar(Derived.anf(this), idGenerator);
    context.push(new Normalization(id, normalize(context, idGenerator)));
    return id;
  }

  Expression normalizeAndBind(IntIdGenerator idGenerator) {
    var context = new Stack<Normalization>();
    return normalize(context, idGenerator).bindAll(context);
  }

  public void printTo(PrintStream out, int indentation) {
    indent(out, indentation);
    out.println(toString());
  }

  public Graph toGraph(Graph graph, Node parent) {
    final Node self =
        rawObjectNode(this)
            .with(
                Records.of(
                    turn(
                        toString().replace("=", "\\=").replace("<", "\\<").replace(">", "\\>"),
                        type.toString()
                            + " | "
                            + "?".replace("=", "\\=").replace("<", "\\<").replace(">", "\\>")
                        /*
                        truncate(
                            preconditions
                                .toString()
                                .replace("=", "\\=")
                                .replace("<", "\\<")
                                .replace(">", "\\>"),
                            1000)*/ )));
    return follow()
        .reduce(
            graph.with(self.link(parent)),
            (accumulator, expr) -> expr.toGraph(accumulator, self),
            (a, b) -> a);
  }

  /** Computes the set of free tree-typed variables in this expression. */
  public Set<Identifier> freeVariables() {
    final var result = new LinkedHashSet<Identifier>();
    getChildren().forEach(e -> result.addAll(e.freeVariables()));
    return result;
  }

  Expression rename(Map<String, String> renaming) {
    throw notImplemented("renaming of a concrete expression type");
  }

  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    indent(out, indentation);
    out.println(toString());
  }

  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    indent(out, indentation);
    out.println(toString());
  }

  public Set<String> getOccurringFunctions() {
    return getChildren()
        .flatMap(e -> e.getOccurringFunctions().stream())
        .collect(Collectors.toSet());
  }

  public abstract Expression unshare(IntIdGenerator idGenerator, boolean lazy);

  public String terminalOrBox() {
    if (isTerminal()) {
      return toString();
    } else {
      return "□";
    }
  }

  public void analyzeSizes(org.jgrapht.Graph<Identifier, SizeEdge> sizeGraph) {
    getChildren().forEach(e -> e.analyzeSizes(sizeGraph));
  }

  public boolean isTreeConstruction() {
    return false;
  }
}
