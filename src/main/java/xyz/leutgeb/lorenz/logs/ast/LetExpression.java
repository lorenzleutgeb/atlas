package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.indent;

import java.io.PrintStream;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

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

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.of(declared, value, body);
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var declaredType = context.getProblem().fresh();
    context.getProblem().add(declaredType, value.infer(context));
    var sub = context.child();
    sub.put(declared.getName(), declaredType);
    sub.getProblem().add(declaredType, declared.infer(sub));

    var result = context.getProblem().fresh();
    sub.getProblem().add(result, body.infer(sub));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    Stack<Pair<Identifier, Expression>> sub = new Stack<>();
    return new LetExpression(source, declared, value.normalize(sub), body).bindAll(sub);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("let ");
    declared.printTo(out, indentation);
    out.print(" = ");
    value.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print("in ");
    body.printTo(out, indentation + 1);
  }
}
