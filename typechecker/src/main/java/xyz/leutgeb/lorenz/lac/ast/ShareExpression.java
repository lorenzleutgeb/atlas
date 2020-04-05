package xyz.leutgeb.lorenz.lac.ast;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.indent;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.hipparchus.util.Pair;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.SizeEdge;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

@Value
@EqualsAndHashCode(callSuper = true)
public class ShareExpression extends Expression {
  private static int freshness = 1;

  Identifier up;
  Pair<Identifier, Identifier> down;

  Expression scope;

  public ShareExpression(
      Expression source, Identifier up, Pair<Identifier, Identifier> down, Expression scope) {
    super(Derived.unshare(source));
    this.up = up;
    this.down = down;
    this.scope = scope;
    this.type = scope.getType();

    if (up.getName().equals(down.getFirst().getName())
        || up.getName().equals(down.getSecond().getName())) {
      throw new IllegalArgumentException(
          "name of unshared variable is the same as one of its replacements");
    }

    if (Util.setOfNames(scope.freeVariables()).contains(up.getName())) {
      throw bug("what???");
    }
  }

  public static Pair<Identifier, Identifier> clone(
      Identifier identifier, IntIdGenerator idGenerator) {
    // TODO: Do not take intro from above, but really use the share expression that resulted as the
    // intro.
    final var intro = identifier.getIntro();

    final var primeIndex = identifier.getName().indexOf("'");
    final var rawIdentifier =
        identifier
            .getName()
            .substring(0, primeIndex < 0 ? identifier.getName().length() : primeIndex);

    return new Pair<>(
        new Identifier(
            identifier.getSource(),
            rawIdentifier + "'" + idGenerator.next(),
            identifier.getType(),
            intro),
        new Identifier(
            identifier.getSource(),
            rawIdentifier + "'" + idGenerator.next(),
            identifier.getType(),
            intro));

    /*
    final var primeIndex = identifier.getName().indexOf("'");
    final var rawIdentifier = identifier.getName().substring(0, primeIndex < 0 ? identifier.getName().length() : primeIndex);

    final var rawIdentifier = identifier.getName().replace("'", "");
    final var n = unshared.merge(rawIdentifier, 2, (k, v) -> v + 2);
    return new Pair<>(
        new Identifier(identifier.getSource(), rawIdentifier + Util.repeat("'", n - 1)),
        new Identifier(identifier.getSource(), rawIdentifier + Util.repeat("'", n)));

    return new Pair<>(
            new Identifier(identifier.getSource(), rawIdentifier + "'" + freshness++),
            new Identifier(identifier.getSource(), rawIdentifier + "'" + freshness++));
     */
  }

  public static Pair<Expression, Expression> rename(
      Identifier up, Pair<Identifier, Identifier> down, Pair<Expression, Expression> expressions) {
    return Pair.create(
        expressions.getFirst().rename(Map.of(up.getName(), down.getFirst().getName())),
        expressions.getSecond().rename(Map.of(up.getName(), down.getSecond().getName())));
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(scope);
  }

  @Override
  protected Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    var sub = context.child();
    sub.putType(down.getFirst().getName(), up.infer(sub), this);
    sub.putType(down.getSecond().getName(), up.infer(sub), this);
    return scope.infer(sub);
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator) {
    // throw new IllegalStateException("calling unshare on a ShareExpression is wrong, since the
    // translation is top-down");
    return this;
    // return new ShareExpression(this, up, down, scope.unshare(unshared, idGenerator));
  }

  @Override
  public String toString() {
    return "share " + up + " as " + down + " in " + scope.terminalOrBox();
  }

  @Override
  public Set<Identifier> freeVariables() {
    final var result = super.freeVariables();
    result.add(up);
    result.remove(down.getFirst());
    result.remove(down.getSecond());
    return result;
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    if (renaming.containsKey(down.getFirst().getName())
        || renaming.containsKey(down.getSecond().getName())) {
      throw new IllegalArgumentException("wat");
    }

    return new ShareExpression(this, up.rename(renaming), down, scope.rename(renaming));
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("share ");
    up.printTo(out, indentation);
    out.print(" as (" + down.getFirst() + ", " + down.getSecond() + ") in (\n");
    indent(out, indentation);
    scope.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation - 1);
    out.print(")");
  }

  @Override
  public void analyzeSizes(Graph<Identifier, SizeEdge> sizeGraph) {
    super.analyzeSizes(sizeGraph);
    sizeGraph.addVertex(up);
    sizeGraph.addVertex(down.getFirst());
    sizeGraph.addVertex(down.getSecond());
    sizeGraph.addEdge(up, down.getFirst(), SizeEdge.eq());
    sizeGraph.addEdge(up, down.getSecond(), SizeEdge.eq());
    sizeGraph.addEdge(down.getSecond(), up, SizeEdge.eq());
    sizeGraph.addEdge(down.getFirst(), up, SizeEdge.eq());
  }
}
