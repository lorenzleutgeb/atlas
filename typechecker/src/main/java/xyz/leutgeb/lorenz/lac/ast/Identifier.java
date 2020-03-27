package xyz.leutgeb.lorenz.lac.ast;

import java.io.PrintStream;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.NonNull;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Predefined;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class Identifier extends Expression {
  public static final Identifier LEAF = new Identifier(Predefined.INSTANCE, "leaf");
  private static final Set<String> CONSTANT_NAMES = Set.of("true", "false", "leaf");
  private static final Set<String> BOOLEAN_NAMES = Set.of("true", "false");
  private static int freshness = 1;

  @NonNull @Getter private final String name;

  private Identifier(Source source, @NonNull String name) {
    super(source);
    Objects.requireNonNull(name);
    this.name = name;
  }

  public Identifier(Source source, @NonNull String name, Type type) {
    super(source, type);
    Objects.requireNonNull(name);
    this.name = name;
  }

  public static Identifier leaf() {
    return new Identifier(Predefined.INSTANCE, "leaf");
  }

  public static Identifier getSugar(Source source, IntIdGenerator idGenerator) {
    return get("var" + idGenerator.next(), source);
    // return get("∂" + Util.generateSubscript(freshness++));
  }

  private static Identifier get(String name) {
    return new Identifier(Predefined.INSTANCE, name);
  }

  public static Identifier get(String name, Source source) {
    return new Identifier(source, name);
  }

  private static boolean isConstant(String name) {
    return CONSTANT_NAMES.contains(name);
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
  public Type inferInternal(UnificationContext context) throws UnificationError, TypeError {
    if (name.equals(LEAF.name)) {
      return new TreeType(context.getProblem().fresh());
    }
    if (BOOLEAN_NAMES.contains(name)) {
      return BoolType.INSTANCE;
    }

    if (context.hasSignature(this.name)) {
      return context.getSignatures().get(this.name).getType();
    }
    return context.lookupType(this.name);
  }

  @Override
  public Expression normalize(
      Stack<Pair<Identifier, Expression>> context, IntIdGenerator idGenerator) {
    return this;
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
  public Set<Identifier> freeVariables() {
    if (!(type instanceof TreeType) || isConstant()) {
      return Collections.emptySet();
    }
    return Collections.singleton(this);
  }

  private boolean isConstant() {
    return isConstant(name);
  }

  @Override
  public boolean isImmediate() {
    return !isConstant();
  }

  @Override
  public boolean isTerminal() {
    return true;
  }

  @Override
  public Identifier rename(Map<String, String> renaming) {
    if (renaming.containsValue(name)) {
      throw new IllegalArgumentException("renaming something to pre-existing name");
    }
    return new Identifier(Derived.rename(this), renaming.getOrDefault(name, name), type);
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation) {
    if (name.equals((LEAF.name))) {
      out.print("Nil");
    } else if (BOOLEAN_NAMES.contains(name)) {
      out.print(Util.capitalizeFirstLetter(name));
    } else {
      out.print(name);
    }
  }
}
