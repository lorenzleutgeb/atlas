package xyz.leutgeb.lorenz.lac.ast;

import static guru.nidi.graphviz.attribute.Records.turn;
import static xyz.leutgeb.lorenz.lac.Util.indent;
import static xyz.leutgeb.lorenz.lac.Util.notImplemented;
import static xyz.leutgeb.lorenz.lac.Util.rawObjectNode;

import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.Substitution;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public abstract class Expression extends Syntax {
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
    if (this.type == null) {
      this.type = inferInternal(context);
    }
    return this.type;
  }

  public void resolveType(Substitution substitution) {
    this.type = substitution.apply(this.type);
    getChildren().forEach(x -> x.resolveType(substitution));
  }

  public @Nonnull Type getType() {
    if (type == null) {
      throw new IllegalStateException("type has not been inferred yet");
    }
    return type;
  }

  Expression normalize(Stack<Pair<Identifier, Expression>> context, IntIdGenerator idGenerator) {
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

  Expression bindAll(Stack<Pair<Identifier, Expression>> context) {
    var binder = this;
    while (!context.isEmpty()) {
      final var binding = context.pop();
      binder =
          new LetExpression(Derived.anf(this), binding.getFirst(), binding.getSecond(), binder);
    }
    return binder;
  }

  Expression forceImmediate(
      Stack<Pair<Identifier, Expression>> context, IntIdGenerator idGenerator) {
    if (isImmediate()) {
      return this;
    }

    var id = Identifier.getSugar(Derived.anf(this), idGenerator);
    context.push(new Pair<>(id, normalize(context, idGenerator)));
    return id;
  }

  Expression normalizeAndBind(IntIdGenerator idGenerator) {
    var context = new Stack<Pair<Identifier, Expression>>();
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

  /**
   * Computes the set of free tree-typed variables in this expression.
   *
   * @return
   */
  public Set<Identifier> freeVariables() {
    final var result = new HashSet<Identifier>();
    getChildren().forEach(e -> result.addAll(e.freeVariables()));
    return result;
  }

  Expression rename(Map<String, String> renaming) {
    throw notImplemented("renaming of a concrete expression type");
  }

  public void printHaskellTo(PrintStream out, int indentation) {
    indent(out, indentation);
    out.println(toString());
  }

  public Set<String> getOccurringFunctions() {
    return getChildren()
        .flatMap(e -> e.getOccurringFunctions().stream())
        .collect(Collectors.toSet());
  }

  public Expression unshare(Map<String, Integer> unshared, IntIdGenerator idGenerator) {
    return this;
  }

  public String terminalOrBox() {
    if (isTerminal()) {
      return toString();
    } else {
      return "□";
    }
  }
}
