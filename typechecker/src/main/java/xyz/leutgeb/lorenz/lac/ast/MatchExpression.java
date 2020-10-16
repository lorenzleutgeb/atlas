package xyz.leutgeb.lorenz.lac.ast;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.lac.util.Util.indent;
import static xyz.leutgeb.lorenz.lac.util.Util.pick;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.function.Supplier;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

@Data
@EqualsAndHashCode(callSuper = true)
@Slf4j
public class MatchExpression extends Expression {
  private final Expression scrut;
  private final Expression leaf;
  private final Expression node;
  private final NodeExpression nodePattern;

  public MatchExpression(
      Source source,
      Expression scrut,
      Expression leaf,
      NodeExpression nodePattern,
      Expression node) {
    super(source);
    this.scrut = scrut;
    this.leaf = leaf;
    this.node = node;
    this.nodePattern = nodePattern;
  }

  private MatchExpression(
      Source source,
      Expression scrut,
      Expression leaf,
      NodeExpression nodePattern,
      Expression node,
      Type type) {
    super(source, type);
    this.scrut = scrut;
    this.leaf = leaf;
    this.node = node;
    this.nodePattern = nodePattern;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(scrut, leaf, nodePattern, node);
  }

  public Stream<? extends Expression> follow() {
    return Stream.of(leaf, node);
  }

  @Override
  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    final var result = context.fresh();

    final var scrutType = context.fresh();
    context.addIfNotEqual(scrutType, scrut.infer(context).wiggle(context));

    final Supplier<UnificationContext> sub = () -> context.hide(((Identifier) scrut).getName());

    // Case: leaf
    var subLeaf = sub.get();
    subLeaf.addIfNotEqual(result, leaf.infer(subLeaf).wiggle(subLeaf));

    // Case: node
    var subNode = sub.get();
    for (int i = 0; i < 3; i++) {
      subNode.putType(
          ((Identifier) nodePattern.getElements().get(i)).getName(), subNode.fresh(), this);
    }
    subNode.addIfNotEqual(scrutType, nodePattern.infer(subNode).wiggle(subNode));
    subNode.addIfNotEqual(result, node.infer(subNode).wiggle(subNode));

    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (scrut.isImmediate()) {
      return new MatchExpression(
          source,
          scrut,
          leaf.normalizeAndBind(idGenerator),
          nodePattern,
          node.normalizeAndBind(idGenerator));
    }

    return new MatchExpression(
        Derived.anf(this),
        scrut.forceImmediate(context, idGenerator),
        leaf.normalizeAndBind(idGenerator),
        nodePattern,
        node.normalizeAndBind(idGenerator));
  }

  @Override
  public MatchExpression rename(Map<String, String> renaming) {
    // TODO(lorenz.leutgeb): Create new expression only if possible.
    return new MatchExpression(
        Derived.rename(this),
        scrut.rename(renaming),
        leaf,
        (NodeExpression) nodePattern.rename(renaming),
        node.rename(renaming),
        type);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("match ");
    scrut.printTo(out, indentation);
    out.println(" with");

    indent(out, indentation);
    out.print("| leaf → ");
    leaf.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print("| ");
    nodePattern.printTo(out, indentation + 1);
    out.print(" → ");
    node.printTo(out, indentation + 1);
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    out.print("case ");
    scrut.printTo(out, indentation);
    out.println(" of");

    indent(out, indentation);
    out.print("  Leaf -> ");
    leaf.printHaskellTo(out, indentation + 1, currentFunction);
    out.println();
    indent(out, indentation);
    out.print("  ");
    nodePattern.printHaskellTo(out, indentation + 1, currentFunction);
    out.print(" -> ");
    node.printHaskellTo(out, indentation + 1, currentFunction);
  }

  @Override
  public String toString() {
    return "match "
        + scrut.terminalOrBox()
        + " with | leaf → "
        + leaf.terminalOrBox()
        + " | "
        + nodePattern.terminalOrBox()
        + " → "
        + node.terminalOrBox();
  }

  @Override
  public Set<Identifier> freeVariables() {
    final var result = super.freeVariables();
    result.removeAll(nodePattern.freeVariables());
    return result;
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    final var newLeaf = leaf.unshare(idGenerator, lazy);
    final var newNode = node.unshare(idGenerator, lazy);

    if (!(scrut instanceof Identifier)) {
      throw new IllegalStateException("anf required");
    }

    final Set<Identifier> freeLeaf = newLeaf.freeVariables();
    final Set<Identifier> freeNode = newNode.freeVariables();

    final var testName = ((Identifier) scrut);
    if (freeLeaf.contains(testName) || freeNode.contains(testName)) {
      throw new IllegalStateException(
          "test variable is destructed, so it cannot occur freely in any case expression");
    }

    Sets.SetView<Identifier> intersection = intersection(freeNode, freeLeaf);

    if (intersection.isEmpty()) {
      return new MatchExpression(source, scrut, newLeaf, nodePattern, newNode, type);
    }

    if (lazy) {
      // log.info("Did not create sharing expression for {} because unsharing is lazy.",
      // intersection);
      return new MatchExpression(source, scrut, newLeaf, nodePattern, newNode, type);
    }

    Identifier up = pick(intersection);
    var down = ShareExpression.clone(up, idGenerator);
    var result = ShareExpression.rename(up, down, Pair.of(newLeaf, newNode));

    Expression newThis =
        new MatchExpression(
            Derived.unshare(this), scrut, result.getLeft(), nodePattern, result.getRight(), type);

    if (intersection.size() > 1) {
      newThis = newThis.unshare(idGenerator, lazy);
    }

    return new ShareExpression(this, up, down, newThis);
  }

  @Override
  public void analyzeSizes(Graph<Identifier, SizeEdge> sizeGraph) {
    super.analyzeSizes(sizeGraph);
    sizeGraph.addVertex((Identifier) scrut);
    sizeGraph.addVertex((Identifier) nodePattern.getLeft());
    sizeGraph.addVertex((Identifier) nodePattern.getRight());
    sizeGraph.addEdge((Identifier) scrut, (Identifier) nodePattern.getLeft(), SizeEdge.gt());
    sizeGraph.addEdge((Identifier) scrut, (Identifier) nodePattern.getRight(), SizeEdge.gt());
  }
}
