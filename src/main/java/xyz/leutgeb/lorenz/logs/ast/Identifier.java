package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.NonNull;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.Util;
import xyz.leutgeb.lorenz.logs.ast.sources.Predefined;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.BoolType;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;
import xyz.leutgeb.lorenz.logs.values.TreeValue;

public class Identifier extends Expression {
  public static final Identifier NIL = new Identifier(Predefined.INSTANCE, "nil");
  public static final Identifier TRUE = new Identifier(Predefined.INSTANCE, "true");
  public static final Identifier FALSE = new Identifier(Predefined.INSTANCE, "false");
  // public static final Identifier ANONYMOUS = new Identifier(Predefined.INSTANCE, "_");

  private static int freshness = 0;
  @NonNull @Getter private final String name;
  @NonNull @Getter private final Set<Source> occurences;

  public Identifier(Source source, @NonNull String name) {
    super(source);
    Objects.requireNonNull(name);
    this.name = name;
    this.occurences = new HashSet<>();
    this.occurences.add(source);
  }

  public static final Identifier nil() {
    return new Identifier(Predefined.INSTANCE, "nil");
  }

  public static Identifier getSugar() {
    return get("∂" + Util.generateSubscript(freshness++));
  }

  public static Identifier get(String name) {
    return new Identifier(Predefined.INSTANCE, name);
  }

  public static Identifier get(String name, Source source) {
    Identifier identifier = new Identifier(source, name);
    identifier.occurences.add(source);
    return identifier;
  }

  @Override
  public String toString() {
    return name;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.empty();
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    if (name.equals(NIL.name)) {
      return new TreeType(context.getProblem().fresh());
    }
    if (name.equals(TRUE.name) || name.equals(FALSE.name)) {
      return BoolType.INSTANCE;
    }

    return context.lookupType(this.name);
    /*
    if (ty == null) {
      throw new TypeError.NotInContext(this.name);
    } else {
      return ty;
    }
    */
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return this;
  }

  @Override
  public Annotation inferAnnotations(AnnotatingContext context, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    // Three cases
    //  - nil
    //    see rule (nil)
    //  - tree variables
    //    return some annotation
    //  - non-tree variables or constants (like true/false)
    //    return a zero-valued annotation
    if (NIL.name.equals(name)) {
      final var constraints = globals.getConstraints();
      var qp = constraints.heuristic(1);

      // Apply (w : var) and (w) since this is a leaf.
      var q = context.weakenAllIdentifiers(constraints).weaken(constraints).getAnnotation();

      // Equate according to preconditions of (nil)
      for (int c = 0; c < q.getRankCoefficients().size(); c++) {
        final var sum = new ArrayList<Coefficient>();
        for (final var e : qp.getCoefficients().entrySet()) {
          final var index = e.getKey();
          final var a = index.get(0);
          final var b = index.get(1);
          if (a + b == c) {
            sum.add(e.getValue());
          }
        }
        constraints.eqSum(q.getRankCoefficients().get(c), sum);
      }
      return qp;
    } else if (getType() instanceof TreeType) {
      return context.weakenIdentifiersExcept(globals.getConstraints(), this).getAnnotation();
    } else {
      return Annotation.empty();
    }
  }

  public boolean isImmediate() {
    return true;
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print(name);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    Identifier that = (Identifier) o;

    return name.equals(that.name);
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public Object evaluate(Context context) {
    if (name.equals(NIL.name)) {
      return TreeValue.nil();
    }
    if (name.equals(TRUE.name)) {
      return true;
    }
    if (name.equals(FALSE.name)) {
      return false;
    }
    return context.lookupType(name);
  }
}
