package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.bug;
import static xyz.leutgeb.lorenz.logs.Util.indent;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Predefined;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Value
public class ShareExpression extends Expression {
  private static int freshness = 0;

  Identifier up;
  Pair<Identifier, Identifier> down;
  Expression e;

  public ShareExpression(Identifier up, Pair<Identifier, Identifier> down, Expression e) {
    this(Predefined.INSTANCE, up, down, e);
  }

  public ShareExpression(
      Source source, Identifier up, Pair<Identifier, Identifier> down, Expression e) {
    super(source);
    this.up = up;
    this.down = down;
    this.e = e;
    this.type = e.getType();
    this.typeResolved = e.typeResolved;

    if (up.getName().equals(down.getFirst().getName())
        || up.getName().equals(down.getSecond().getName())) {
      throw new IllegalArgumentException("wat");
    }
  }

  public static Pair<Identifier, Identifier> clone(
      Identifier identifier, Map<String, Integer> unshared) {
    return new Pair<>(
        new Identifier(identifier.getSource(), identifier.getName() + "'" + freshness++),
        new Identifier(identifier.getSource(), identifier.getName() + "'" + freshness++));

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
    return Stream.of(e);
  }

  @Override
  protected Type inferInternal(Context context) throws UnificationError, TypeError {
    var sub = context.child();
    sub.putType(down.getFirst().getName(), up.infer(sub));
    sub.putType(down.getSecond().getName(), up.infer(sub));
    return e.infer(sub);
  }

  @Override
  protected Annotation inferAnnotationsInternal(
      AnnotatingContext context, AnnotatingGlobals globals) throws UnificationError, TypeError {

    final var constraints = globals.getConstraints();

    if (context.getIds().contains(down.getFirst().getName())
        || context.getIds().contains(down.getSecond().getName())) {
      throw new IllegalStateException("an id that is introduced here is already in the context");
    }

    final var upIndex = context.getIndex(up);
    if (upIndex < 0) {
      throw bug("up not contained");
    }

    final var filteredIds = new ArrayList<String>(context.size() - 1 + 2);
    if (context.size() > 1) {
      filteredIds.addAll(context.getIds().subList(0, upIndex));
      filteredIds.addAll(context.getIds().subList(upIndex + 1, context.size()));
    } else {
      System.out.println("lel");
    }
    filteredIds.add(down.getFirst().getName());
    filteredIds.add(down.getSecond().getName());

    final var sharedAnnotation = constraints.heuristic(context.size() - 1 + 2);
    final var sharedContext = new AnnotatingContext(filteredIds, sharedAnnotation);

    constraints.eqSum(
        this,
        context.getAnnotation().getRankCoefficient(upIndex),
        List.of(
            sharedAnnotation.getRankCoefficient(sharedContext.size() - 1),
            sharedAnnotation.getRankCoefficient(sharedContext.size() - 2)));

    for (int i = 0; i < context.size(); i++) {
      if (i == upIndex) {
        continue;
      }
      int offset = i > upIndex ? -1 : 0;
      constraints.eq(
          this,
          context.getAnnotation().getRankCoefficient(i),
          sharedAnnotation.getRankCoefficient(i + offset));
    }

    // TODO: Equate all coefficients for elements of this.ids not in ids

    /*
    result
        .streamIndices()
        .forEach(
            index -> {
              final var sum = index.getFirst().get(id) + index.getFirst().get(primed);
              final var copy = new HashMap<>(index.getFirst());
              copy.put(id, sum);
              constraints.eq(
                  source,
                  result.getCoefficient(index),
                  this.getCoefficientOrZero(copy, index.getSecond()));
            });

    return new Pair<>(primed, result);
    */
    return e.inferAnnotations(sharedContext, globals);
  }

  @Override
  public Expression unshare(Map<String, Integer> unshared) {
    // throw new IllegalStateException("calling unshare on a ShareExpression is wrong, since the
    // translation is top-down");
    return new ShareExpression(up, down, e.unshare(unshared));
  }

  @Override
  public String toString() {
    return "share " + up + " as " + down + " in ...";
  }

  @Override
  public Set<String> freeVariables() {
    final var result = super.freeVariables();
    result.remove(down.getFirst().getName());
    result.remove(down.getSecond().getName());
    return result;
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    if (renaming.containsKey(down.getFirst().getName())
        || renaming.containsKey(down.getSecond().getName())) {
      throw new IllegalArgumentException("wat");
    }

    return new ShareExpression(up.rename(renaming), down, e.rename(renaming));
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("share ");
    up.printTo(out, indentation);
    out.print(" as ");
    out.print(down);
    // out.println();
    // indent(out, indentation);
    out.println(" in (");
    indent(out, indentation);
    e.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print(")");
  }
}
