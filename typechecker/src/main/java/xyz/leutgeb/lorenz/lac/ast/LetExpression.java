package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.lac.util.Util.indent;
import static xyz.leutgeb.lorenz.lac.util.Util.pick;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Predefined;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

@Data
@EqualsAndHashCode(callSuper = true)
public class LetExpression extends Expression {
  private final Identifier declared;
  private final Expression value;

  private final Expression body;

  public LetExpression(Source source, Identifier declared, Expression value, Expression body) {
    super(source);
    this.declared = declared;
    this.value = value;
    this.body = body;
  }

  public LetExpression(
      Source source, Identifier declared, Expression value, Expression body, Type type) {
    // TODO(lorenz.leutgeb): This constructor was made public only for testing purposes. Make it
    // private again?
    super(source, type);
    this.declared = declared;
    this.value = value;
    this.body = body;
  }

  public static LetExpression predefinedLet(
      Identifier declared, Identifier value, Expression body) {
    return new LetExpression(Predefined.INSTANCE, declared, value, body, body.getType());
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.concat(Stream.of(declared), follow());
  }

  @Override
  public Stream<? extends Expression> follow() {
    return Stream.of(value, body);
  }

  @Override
  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    var declaredType = context.fresh();
    context.addIfNotEqual(declaredType, value.infer(context).wiggle(context));
    var sub = context.child();
    sub.putType(declared.getName(), declaredType, this);
    sub.addIfNotEqual(declaredType, declared.infer(sub).wiggle(context));

    var result = context.fresh();
    sub.addIfNotEqual(result, body.infer(sub).wiggle(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    Stack<Normalization> sub = new Stack<>();
    return new LetExpression(
            source, declared, value.normalize(sub, idGenerator), body.normalizeAndBind(idGenerator))
        .bindAll(sub);
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return new LetExpression(
        Derived.rename(this), declared, value.rename(renaming), body.rename(renaming), type);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("let ");
    declared.printTo(out, indentation);
    out.print(" ≔ ");
    value.printTo(out, indentation + 1);
    // out.println();
    // indent(out, indentation);
    out.println(" in (");
    indent(out, indentation);
    body.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation - 1);
    out.print(")");
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    out.print("let ");
    declared.printHaskellTo(out, indentation, currentFunction);
    out.print(" = (");
    // indent(out, indentation + 1);
    value.printHaskellTo(out, indentation + 1, currentFunction);
    out.print(")");
    // indent(out, indentation + 1);
    out.print(" in (");
    // indent(out, indentation + 1);
    body.printHaskellTo(out, indentation + 1, currentFunction);
    // out.println();
    // indent(out, indentation);
    out.print(")");
  }

  @Override
  public Set<Identifier> freeVariables() {
    final var result = super.freeVariables();
    result.remove(declared);
    return result;
  }

  @Override
  public String toString() {
    return "let " + declared + " ≔ " + value.terminalOrBox() + " in " + body.terminalOrBox();
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    final var newValue = value.unshare(idGenerator, lazy);
    final var newBody = body.unshare(idGenerator, lazy);

    final var intersection = intersection(newValue.freeVariables(), newBody.freeVariables());

    if (intersection.isEmpty()) {
      return new LetExpression(source, declared, newValue, newBody, type);
    }

    // Otherwise, there's some overlap between body and value.
    var target = pick(intersection);
    var down = ShareExpression.clone(target, idGenerator);
    var result = ShareExpression.rename(target, down, Pair.of(newValue, newBody));

    Expression newThis =
        new LetExpression(source, declared, result.getLeft(), result.getRight(), type);

    if (intersection.size() > 1) {
      newThis = newThis.unshare(idGenerator, lazy);
    }

    return new ShareExpression(this, target, down, newThis);
  }

  @Override
  public void analyzeSizes(Graph<Identifier, SizeEdge> sizeGraph) {
    super.analyzeSizes(sizeGraph);

    if (!(value.getType() instanceof TreeType)) {
      return;
    }

    if (value instanceof NodeExpression) {
      final var node = (NodeExpression) value;
      sizeGraph.addVertex(declared);
      sizeGraph.addVertex((Identifier) node.getLeft());
      sizeGraph.addVertex((Identifier) node.getRight());
      sizeGraph.addEdge(declared, (Identifier) node.getLeft(), SizeEdge.gt());
      sizeGraph.addEdge(declared, (Identifier) node.getRight(), SizeEdge.gt());
    }

    if (value instanceof Identifier) {
      final var identifier = (Identifier) value;
      sizeGraph.addVertex(declared);
      sizeGraph.addVertex(identifier);
      sizeGraph.addEdge(declared, identifier, SizeEdge.eq());
      sizeGraph.addEdge(identifier, declared, SizeEdge.eq());
    }
  }
}
