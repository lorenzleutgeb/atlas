package xyz.leutgeb.lorenz.atlas.ast.expressions;

import static xyz.leutgeb.lorenz.atlas.util.Util.indent;

import java.io.PrintStream;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.atlas.ast.Normalization;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Data
@EqualsAndHashCode(callSuper = true)
@Slf4j
public class MatchTupleExpression extends Expression {
  @NonNull private final Expression scrut;
  @NonNull private final Expression body;
  @NonNull private final TupleExpression pattern;

  public MatchTupleExpression(
      Source source, Expression scrut, Expression body, TupleExpression pattern) {
    super(source);
    this.scrut = scrut;
    this.body = body;
    this.pattern = pattern;
  }

  private MatchTupleExpression(
      Source source, Expression scrut, Expression body, TupleExpression pattern, Type type) {
    super(source, type);
    this.scrut = scrut;
    this.body = body;
    this.pattern = pattern;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(scrut, body, pattern);
  }

  public Stream<? extends Expression> follow() {
    return Stream.of(body);
  }

  private boolean isPassthru() {
    return false;
  }

  @Override
  public Type inferInternal(UnificationContext context) throws TypeError {
    final var result = context.fresh();

    final var scrutType = context.fresh();
    context.addEquivalenceIfNotEqual(scrutType, scrut.infer(context).wiggle(context), source);

    final UnificationContext sub = context.hide(((IdentifierExpression) scrut).getName());
    for (int i = 0; i < pattern.getElements().size(); i++) {
      sub.putType(
          ((IdentifierExpression) pattern.getElements().get(i)).getName(), sub.fresh(), this);
    }
    sub.addEquivalenceIfNotEqual(result, body.infer(sub).wiggle(sub), source);
    sub.addEquivalenceIfNotEqual(scrutType, pattern.infer(sub).wiggle(sub), source);

    return result;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    if (scrut.isImmediate()) {
      return new MatchTupleExpression(source, scrut, body.normalizeAndBind(idGenerator), pattern);
    }

    return new MatchTupleExpression(
        Derived.anf(this),
        scrut.forceImmediate(context, idGenerator),
        body.normalizeAndBind(idGenerator),
        pattern);
  }

  @Override
  public MatchTupleExpression rename(Map<String, String> renaming) {
    // TODO(lorenzleutgeb): Create new expression only if necessary.
    return new MatchTupleExpression(
        Derived.rename(this),
        scrut.rename(renaming),
        body,
        (TupleExpression) pattern.rename(renaming),
        type);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("match ");
    scrut.printTo(out, indentation);
    out.println(" with");

    indent(out, indentation);
    out.print("| ");
    pattern.printTo(out, indentation + 1);
    out.print(" → ");
    body.printTo(out, indentation + 1);
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    out.print("case ");
    scrut.printTo(out, indentation);
    out.println(" of");

    indent(out, indentation);
    out.print("  ");
    pattern.printHaskellTo(out, indentation + 1, currentFunction);
    out.print(" -> ");
    body.printHaskellTo(out, indentation + 1, currentFunction);
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    out.print("if (");
    scrut.printTo(out, indentation);
    out.println(".isLeaf()) {");

    indent(out, indentation);
    if (body.isTerminal()) {
      indent(out, indentation + 1);
      out.println("return (");
      body.printJavaTo(out, indentation + 1, currentFunction);
      indent(out, indentation + 1);
      out.println(");");
    } else {
      body.printJavaTo(out, indentation + 1, currentFunction);
    }

    out.println();
    indent(out, indentation);
    out.println("} else { ");
    for (int i = 0; i < pattern.getElements().size(); i++) {
      var element = pattern.getElements().get(i);
      indent(out, indentation + 1);
      out.println(
          "final var "
              + ((IdentifierExpression) element).getName()
              + " = "
              + ((IdentifierExpression) scrut).getName()
              + ".get("
              + i
              + ";");
    }
    if (body.isTerminal()) {
      indent(out, indentation + 1);
      out.println("return (");
      body.printJavaTo(out, indentation + 1, currentFunction);
      indent(out, indentation + 1);
      out.println(");");
    } else {
      body.printJavaTo(out, indentation + 1, currentFunction);
    }
    indent(out, indentation);
    out.println("}");
  }

  @Override
  public String toString() {
    return "match "
        + scrut.terminalOrBox()
        + " with | "
        + pattern.terminalOrBox()
        + " → "
        + body.terminalOrBox();
  }

  @Override
  public Set<IdentifierExpression> freeVariables() {
    final var result = super.freeVariables();
    result.removeAll(pattern.freeVariables());
    result.add((IdentifierExpression) scrut);
    return result;
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    final var newBody = body.unshare(idGenerator, lazy);

    if (!(scrut instanceof final IdentifierExpression testName)) {
      throw new IllegalStateException("anf required");
    }

    final Set<IdentifierExpression> freeBody = newBody.freeVariables();

    if (freeBody.contains(testName)) {
      throw new IllegalStateException(
          "test variable is destructed, so it cannot occur freely in any case expression");
    }

    return new MatchTupleExpression(source, scrut, newBody, pattern, type);
  }
}
