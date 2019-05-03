package xyz.leutgeb.lorenz.logs.ast;

import java.io.PrintStream;
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
import xyz.leutgeb.lorenz.logs.resources.AnnotatedType;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

public class Identifier extends TupleElement {
  public static final Identifier NIL = new Identifier(Predefined.INSTANCE, "nil");
  public static final Identifier TRUE = new Identifier(Predefined.INSTANCE, "true");
  public static final Identifier FALSE = new Identifier(Predefined.INSTANCE, "false");
  public static final Identifier ANONYMOUS = new Identifier(Predefined.INSTANCE, "_");
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
    return "(id " + name + ")";
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
    if (name.equals(ANONYMOUS.name)) {
      return context.getProblem().fresh();
    }

    Type ty = context.lookup(this.name);
    if (ty == null) {
      throw new TypeError.NotInContext(this.name);
    } else {
      return ty;
    }
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    return this;
  }

  @Override
  public AnnotatedType inferAnnotations(Context context, Annotation typingContext)
      throws UnificationError, TypeError {
    // Special case for nil, see rule (nil).
    if (this == NIL) {
      var constraints = context.getConstraints();
      var result = constraints.heuristic(1);

      // Choose some annotation q for the empty sequence of trees.
      var q = constraints.heuristic(0);

      // Equate according to preconditions of (nil)
      for (var e : q.getCoefficients().entrySet()) {
        var c = e.getKey().get(0);
        for (int a = 0; a <= c; a++) {
          int b = c - a;

          // TODO(lorenzleutgeb): Should getOrFresh be used here?
          constraints.eq(e.getValue(), result.getOrFresh(constraints, a, b));
        }
      }

      return new AnnotatedType(infer(context), result);
    }

    // Now, two other cases are left:
    //  - tree variables
    //    return some annotation
    //  - non-tree variables or constants (like true/false)
    //    return a zero-valued annotation

    // TODO(lorenz.leutgeb): How to handle tree variables?

    // Handles non-tree variables and constants.
    return new AnnotatedType(null, Annotation.EMPTY);
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
}
