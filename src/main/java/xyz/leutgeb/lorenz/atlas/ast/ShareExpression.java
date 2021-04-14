package xyz.leutgeb.lorenz.atlas.ast;

import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.indent;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;
import xyz.leutgeb.lorenz.atlas.util.Util;

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
    // TODO(lorenz.leutgeb): Prettify...
    /*
    this.down =
        Pair.of(
            new Identifier(
                down.getLeft().source,
                down.getLeft().getName(),
                down.getLeft().type,
                new Intro(down.getLeft().getIntro().getFqn(), this)),
            new Identifier(
                down.getRight().source,
                down.getRight().getName(),
                down.getRight().type,
                new Intro(down.getRight().getIntro().getFqn(), this)));
     */

    this.scope = scope;
    this.type = scope.getType();

    if (up.getName().equals(down.getLeft().getName())
        || up.getName().equals(down.getRight().getName())) {
      throw new IllegalArgumentException(
          "name of unshared variable is the same as one of its replacements");
    }

    if (scope.freeVariables().contains(up)) {
      throw bug("what???");
    }
  }

  public static Pair<Identifier, Identifier> clone(
      Identifier identifier, IntIdGenerator idGenerator) {
    final var source = Derived.unshare(identifier);
    final var intro = identifier.getIntro();
    final var type = identifier.getType();

    final var primeIndex = identifier.getName().indexOf("'");
    final var rawIdentifier =
        identifier
            .getName()
            .substring(0, primeIndex < 0 ? identifier.getName().length() : primeIndex);

    return Pair.of(
        new Identifier(
            source, rawIdentifier + Util.generateSubscript(idGenerator.next()), type, intro),
        new Identifier(
            source, rawIdentifier + Util.generateSubscript(idGenerator.next()), type, intro));
  }

  public static Pair<Expression, Expression> rename(
      Identifier up, Pair<Identifier, Identifier> down, Pair<Expression, Expression> expressions) {
    return Pair.of(
        expressions.getLeft().rename(Map.of(up.getName(), down.getLeft().getName())),
        expressions.getRight().rename(Map.of(up.getName(), down.getRight().getName())));
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(scope);
  }

  @Override
  protected Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    var sub = context.child();
    sub.putType(down.getLeft().getName(), up.infer(sub), this);
    sub.putType(down.getRight().getName(), up.infer(sub), this);
    return scope.infer(sub);
  }

  @Override
  public String toString() {
    return "share " + up + " as " + down + " in " + scope.terminalOrBox();
  }

  @Override
  public Set<Identifier> freeVariables() {
    final var result = super.freeVariables();
    result.add(up);
    // This is quite ugly. We cannot use equals, since the intro of occurrences of
    // down.get(Left|Right) does not get reset to this.
    result.removeIf(
        x ->
            x.getName().equals(down.getLeft().getName())
                || x.getName().equals(down.getRight().getName()));
    return result;
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    if (renaming.containsKey(down.getLeft().getName())
        || renaming.containsKey(down.getRight().getName())) {
      throw new IllegalArgumentException("wat");
    }

    return new ShareExpression(this, up.rename(renaming), down, scope.rename(renaming));
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("share ");
    up.printTo(out, indentation);
    out.print(" ≡ " + down.getLeft() + " ≡ " + down.getRight() + " in (\n");
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
    sizeGraph.addVertex(down.getLeft());
    sizeGraph.addVertex(down.getRight());
    sizeGraph.addEdge(up, down.getLeft(), SizeEdge.eq());
    sizeGraph.addEdge(up, down.getRight(), SizeEdge.eq());
    sizeGraph.addEdge(down.getRight(), up, SizeEdge.eq());
    sizeGraph.addEdge(down.getLeft(), up, SizeEdge.eq());
  }
}
