package xyz.leutgeb.lorenz.atlas.ast.expressions;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.indent;
import static xyz.leutgeb.lorenz.atlas.util.Util.pick;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.function.Supplier;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.atlas.ast.Normalization;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.SizeEdge;

@Data
@EqualsAndHashCode(callSuper = true)
@Slf4j
public class MatchTreeExpression extends Expression {
  private final Expression scrut;
  private final Expression leaf;
  private final Expression node;
  private final Expression nodePattern;

  public MatchTreeExpression(
      Source source, Expression scrut, Expression leaf, Expression nodePattern, Expression node) {
    super(source);
    this.scrut = scrut;
    this.leaf = leaf;
    this.node = node;
    this.nodePattern = nodePattern;
  }

  private MatchTreeExpression(
      Source source,
      Expression scrut,
      Expression leaf,
      Expression nodePattern,
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

  private boolean isPassthru() {
    return scrut.equals(nodePattern);
  }

  @Override
  public Type inferInternal(UnificationContext context) throws TypeError {
    final var passthru = isPassthru();
    if (!passthru && !(nodePattern instanceof NodeExpression)) {
      throw bug("node expression as node pattern required");
    }

    final var result = context.fresh();

    final var scrutType = context.fresh();
    context.addEquivalenceIfNotEqual(scrutType, scrut.infer(context).wiggle(context), source);

    final Supplier<UnificationContext> sub =
        () -> context.hide(((IdentifierExpression) scrut).getName());

    // Case: leaf
    var subLeaf = sub.get();
    subLeaf.addEquivalenceIfNotEqual(result, leaf.infer(subLeaf).wiggle(subLeaf), source);

    // Case: node
    var subNode = passthru ? context : sub.get();
    for (int i = 0; !passthru && i < 3; i++) {
      subNode.putType(
          ((IdentifierExpression) ((NodeExpression) nodePattern).getElements().get(i)).getName(),
          subNode.fresh(),
          this);
    }
    subNode.addEquivalenceIfNotEqual(scrutType, nodePattern.infer(subNode).wiggle(subNode), source);
    subNode.addEquivalenceIfNotEqual(result, node.infer(subNode).wiggle(subNode), source);

    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    Expression node = this.node;
    Expression nodePattern = this.nodePattern;

    if (nodePattern instanceof final IdentifierExpression nodeId
        && !this.scrut.equals(this.nodePattern)) {
      final var source = Derived.desugar(this);

      if (nodeId.getName().startsWith("_")) {
        nodePattern =
            new NodeExpression(
                source,
                List.of(
                    IdentifierExpression.anonymous(source, idGenerator),
                    IdentifierExpression.anonymous(source, idGenerator),
                    IdentifierExpression.anonymous(source, idGenerator)));
      } else {
        final IdentifierExpression left =
            IdentifierExpression.get(nodeId.getName() + "_l_" + idGenerator.next(), source);
        final IdentifierExpression middle =
            IdentifierExpression.get(nodeId.getName() + "_x_" + idGenerator.next(), source);
        final IdentifierExpression right =
            IdentifierExpression.get(nodeId.getName() + "_r_" + idGenerator.next(), source);
        nodePattern = new NodeExpression(source, List.of(left, middle, right));
        node = new LetExpression(source, nodeId, nodePattern, node);
      }
    }

    if (scrut.isImmediate()) {
      return new MatchTreeExpression(
          source,
          scrut,
          leaf.normalizeAndBind(idGenerator),
          nodePattern,
          node.normalizeAndBind(idGenerator));
    }

    return new MatchTreeExpression(
        Derived.anf(this),
        scrut.forceImmediate(context, idGenerator),
        leaf.normalizeAndBind(idGenerator),
        nodePattern,
        node.normalizeAndBind(idGenerator));
  }

  @Override
  public MatchTreeExpression rename(Map<String, String> renaming) {
    // TODO(lorenzleutgeb): Create new expression only if necessary.
    return new MatchTreeExpression(
        Derived.rename(this),
        scrut.rename(renaming),
        leaf,
        nodePattern.rename(renaming),
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
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    out.print("if (");
    scrut.printTo(out, indentation);
    out.println(".isLeaf()) {");

    indent(out, indentation);
    if (leaf.isTerminal()) {
      indent(out, indentation + 1);
      out.println("return (");
      leaf.printJavaTo(out, indentation + 1, currentFunction);
      indent(out, indentation + 1);
      out.println(");");
    } else {
      leaf.printJavaTo(out, indentation + 1, currentFunction);
    }

    out.println();
    indent(out, indentation);
    out.println("} else { ");
    if (!isPassthru()) {
      indent(out, indentation + 1);
      out.println(
          "final var "
              + ((IdentifierExpression) ((NodeExpression) nodePattern).getLeft()).getName()
              + " = "
              + ((IdentifierExpression) scrut).getName()
              + ".left;");
      indent(out, indentation + 1);
      out.println(
          "final var "
              + ((IdentifierExpression) ((NodeExpression) nodePattern).getElements().get(1))
                  .getName()
              + " = "
              + ((IdentifierExpression) scrut).getName()
              + ".value;");
      indent(out, indentation + 1);
      out.println(
          "final var "
              + ((IdentifierExpression) ((NodeExpression) nodePattern).getRight()).getName()
              + " = "
              + ((IdentifierExpression) scrut).getName()
              + ".right;");
    }
    if (node.isTerminal()) {
      indent(out, indentation + 1);
      out.println("return (");
      node.printJavaTo(out, indentation + 1, currentFunction);
      indent(out, indentation + 1);
      out.println(");");
    } else {
      node.printJavaTo(out, indentation + 1, currentFunction);
    }
    indent(out, indentation);
    out.println("}");
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
  public Set<IdentifierExpression> freeVariables() {
    final var result = super.freeVariables();
    if (!(nodePattern instanceof IdentifierExpression && scrut.equals(nodePattern))) {
      result.removeAll(nodePattern.freeVariables());
    }
    return result;
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    final var newLeaf = leaf.unshare(idGenerator, lazy);
    final var newNode = node.unshare(idGenerator, lazy);

    if (!(scrut instanceof final IdentifierExpression testName)) {
      throw new IllegalStateException("anf required");
    }

    final Set<IdentifierExpression> freeLeaf = newLeaf.freeVariables();
    final Set<IdentifierExpression> freeNode = newNode.freeVariables();

    if (freeLeaf.contains(testName) || (!isPassthru() && freeNode.contains(testName))) {
      throw new IllegalStateException(
          "test variable is destructed, so it cannot occur freely in any case expression");
    }

    Sets.SetView<IdentifierExpression> intersection = intersection(freeNode, freeLeaf);

    if (intersection.isEmpty()) {
      return new MatchTreeExpression(source, scrut, newLeaf, nodePattern, newNode, type);
    }

    if (lazy) {
      // log.info("Did not create sharing expression for {} because unsharing is lazy.",
      // intersection);
      return new MatchTreeExpression(source, scrut, newLeaf, nodePattern, newNode, type);
    }

    IdentifierExpression up = pick(intersection);
    var down = ShareExpression.clone(up, idGenerator);
    var result = ShareExpression.rename(up, down, Pair.of(newLeaf, newNode));

    Expression newThis =
        new MatchTreeExpression(
            Derived.unshare(this), scrut, result.getLeft(), nodePattern, result.getRight(), type);

    if (intersection.size() > 1) {
      newThis = newThis.unshare(idGenerator, lazy);
    }

    return new ShareExpression(this, up, down, newThis);
  }

  @Override
  public void analyzeSizes(Graph<IdentifierExpression, SizeEdge> sizeGraph) {
    super.analyzeSizes(sizeGraph);
    sizeGraph.addVertex((IdentifierExpression) scrut);
    if (!isPassthru()) {
      sizeGraph.addVertex((IdentifierExpression) ((NodeExpression) nodePattern).getLeft());
      sizeGraph.addVertex((IdentifierExpression) ((NodeExpression) nodePattern).getRight());
      sizeGraph.addEdge(
          (IdentifierExpression) scrut,
          (IdentifierExpression) ((NodeExpression) nodePattern).getLeft(),
          SizeEdge.gt());
      sizeGraph.addEdge(
          (IdentifierExpression) scrut,
          (IdentifierExpression) ((NodeExpression) nodePattern).getRight(),
          SizeEdge.gt());
    }
  }
}
