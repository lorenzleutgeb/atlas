package xyz.leutgeb.lorenz.atlas.ast;

import static xyz.leutgeb.lorenz.atlas.util.Util.*;

import java.io.PrintStream;
import java.util.*;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Value
@EqualsAndHashCode(callSuper = true)
public class TickExpression extends Expression {
  @NonNull Expression body;

  @NonNull Fraction cost;

  public TickExpression(Source source, @NonNull Expression body, @NonNull Fraction cost) {
    super(source);
    this.body = body;
    this.cost = cost;
  }

  private TickExpression(
      Source source, @NonNull Expression body, @NonNull Fraction cost, Type type) {
    super(source, type);
    this.body = body;
    this.cost = cost;
  }

  @Override
  protected Stream<? extends Expression> getChildren() {
    return follow();
  }

  @Override
  public Stream<? extends Expression> follow() {
    return Stream.of(body);
  }

  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    return body.infer(context).wiggle(context);
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    return new TickExpression(source, body.normalizeAndBind(idGenerator), cost, type);
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return new TickExpression(Derived.rename(this), body.rename(renaming), cost, type);
  }

  @Override
  public String toString() {
    return "~ " + cost.getNumerator() + " " + cost.getDenominator() + " " + body;
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("~ ");
    if (!Fraction.ONE.equals(cost)) {
      out.print(cost.getNumerator());
      out.print(" ");
      out.print(cost.getDenominator());
      out.print(" ");
    }
    out.println("(");
    indent(out, indentation);
    body.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation - 1);
    out.print(")");
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    body.printHaskellTo(out, indentation, currentFunction);
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    body.printJavaTo(out, indentation, currentFunction);
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return new TickExpression(Derived.unshare(this), body.unshare(idGenerator, lazy), cost, type);
  }

  @Override
  public boolean isTerminal() {
    return false;
  }
}
