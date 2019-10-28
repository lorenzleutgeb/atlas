package xyz.leutgeb.lorenz.logs.ast;

import static guru.nidi.graphviz.attribute.Records.turn;
import static guru.nidi.graphviz.model.Factory.node;
import static xyz.leutgeb.lorenz.logs.Util.indent;
import static xyz.leutgeb.lorenz.logs.Util.notImplemented;
import static xyz.leutgeb.lorenz.logs.Util.truncate;

import guru.nidi.graphviz.attribute.Records;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Derived;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.Substitution;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public abstract class Expression extends Syntax {
  protected Type type;
  protected Annotation annotation;
  protected boolean typeResolved;
  protected Set<Constraint> preconditions = new LinkedHashSet<>();

  public Expression(Source source) {
    super(source);
    typeResolved = false;
  }

  public Expression(Source source, Type type) {
    super(source);
    this.type = type;
    typeResolved = true;
  }

  public abstract Stream<? extends Expression> getChildren();

  public Stream<? extends Expression> follow() {
    return getChildren();
  };

  protected abstract Type inferInternal(Context context) throws UnificationError, TypeError;

  public Type infer(Context context) throws UnificationError, TypeError {
    if (this.type == null) {
      this.type = inferInternal(context);
    }
    return this.type;
  }

  public void resolveType(Substitution substitution) {
    this.type = substitution.apply(this.type);
    typeResolved = true;
    getChildren().forEach(x -> x.resolveType(substitution));
  }

  public @Nonnull Type getType() {
    if (type == null) {
      throw new IllegalStateException("type has not been inferred yet");
    }
    return type;
  }

  public boolean isTypeResolved() {
    return typeResolved;
  }

  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    if (isImmediate()) {
      return this;
    }
    throw notImplemented("normalization of a concrete non-immediate expression type");
  }

  public boolean isImmediate() {
    return false;
  }

  protected abstract Annotation inferAnnotationsInternal(
      AnnotatingContext context, AnnotatingGlobals globals) throws UnificationError, TypeError;

  public Annotation inferAnnotations(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    if (annotation == null) {
      annotation = inferAnnotationsInternal(context, globals);
    }
    return annotation;
  }

  public Expression bindAll(Stack<Pair<Identifier, Expression>> context) {
    var binder = this;
    while (!context.isEmpty()) {
      final var binding = context.pop();
      binder =
          new LetExpression(Derived.anf(source), binding.getFirst(), binding.getSecond(), binder);
    }
    return binder;
  }

  public Expression forceImmediate(Stack<Pair<Identifier, Expression>> context) {
    if (isImmediate()) {
      return this;
    }

    var id = Identifier.getSugar(Derived.anf(source));
    context.push(new Pair<>(id, normalize(context)));
    return id;
  }

  protected Expression normalizeAndBind() {
    var context = new Stack<Pair<Identifier, Expression>>();
    return normalize(context).bindAll(context);
  }

  public void printTo(PrintStream out, int indentation) {
    indent(out, indentation);
    out.println(toString());
  }

  public void addPrecondition(Constraint constraint) {
    preconditions.add(constraint);
  }

  public Graph toGraph(Graph graph, Node parent) {
    final Node self =
        node(getClass().getSimpleName() + "@" + System.identityHashCode(this))
            .with(
                Records.of(
                    turn(
                        toString().replace("=", "\\=").replace("<", "\\<").replace(">", "\\>"),
                        type.toString()
                            + " | "
                            + (annotation != null ? annotation.toShortString() : "?")
                                .replace("=", "\\=")
                                .replace("<", "\\<")
                                .replace(">", "\\>"),
                        truncate(
                            preconditions
                                .toString()
                                .replace("=", "\\=")
                                .replace("<", "\\<")
                                .replace(">", "\\>"),
                            1000))));
    return follow()
        .reduce(
            graph.with(self.link(parent)),
            (accumulator, expr) -> expr.toGraph(accumulator, self),
            (a, b) -> a);
  }

  public Set<String> freeVariables() {
    final var result = new HashSet<String>();
    getChildren().forEach(e -> result.addAll(e.freeVariables()));
    return result;
  }

  public Expression rename(Map<String, String> renaming) {
    throw notImplemented("renaming of a concrete expression type");
  }

  public void printHaskellTo(PrintStream out, int indentation) {
    indent(out, indentation);
    out.println(toString());
  }

  public Set<String> getOcurringFunctions() {
    return getChildren()
        .flatMap(e -> e.getOcurringFunctions().stream())
        .collect(Collectors.toSet());
  }

  /** Performs a bottom-down translation to explicate sharing of variables. */
  public Expression unshare(Map<String, Integer> unshared) {
    return this;
  }
}
