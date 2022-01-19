package xyz.leutgeb.lorenz.atlas.ast.expressions;

import java.io.PrintStream;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.ast.Normalization;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Value
@EqualsAndHashCode(callSuper = true)
public class CoinExpression extends Expression {
  @NonNull Fraction p;

  public CoinExpression(Source source, @NonNull Fraction p) {
    super(source);
    this.p = p;
  }

  private CoinExpression(Source source, @NonNull Fraction p, Type type) {
    super(source, type);
    this.p = p;
  }

  @Override
  protected Stream<? extends Expression> getChildren() {
    return follow();
  }

  @Override
  public Stream<? extends Expression> follow() {
    return Stream.empty();
  }

  public Type inferInternal(UnificationContext context) throws TypeError {
    return BoolType.INSTANCE;
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
    return this;
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return this;
  }

  @Override
  public String toString() {
    return "coin " + p.getNumerator() + " " + p.getDenominator();
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("coin");
    if (!Fraction.ONE.equals(p)) {
      out.print(" ");
      out.print(p.getNumerator());
      out.print(" ");
      out.print(p.getDenominator());
    }
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    // TODO: p
    out.print("coin");
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    // TODO: p
    out.print("coin()");
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }

  @Override
  public boolean isTerminal() {
    return false;
  }
}
