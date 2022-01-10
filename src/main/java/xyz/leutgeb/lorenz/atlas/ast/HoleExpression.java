package xyz.leutgeb.lorenz.atlas.ast;

import java.io.PrintStream;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.Type;
import xyz.leutgeb.lorenz.atlas.unification.UnificationContext;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

@Value
@EqualsAndHashCode(callSuper = true)
public class HoleExpression extends Expression {
  public HoleExpression(Source source) {
    super(source);
  }

  @Override
  protected Stream<? extends Expression> getChildren() {
    return follow();
  }

  @Override
  public Stream<? extends Expression> follow() {
    return Stream.empty();
  }

  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    return context.fresh();
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
    return "_";
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("_");
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    out.print("_");
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {}

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }

  @Override
  public boolean isTerminal() {
    return true;
  }
}
