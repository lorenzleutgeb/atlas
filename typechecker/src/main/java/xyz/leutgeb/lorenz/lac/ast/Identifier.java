package xyz.leutgeb.lorenz.lac.ast;

import static java.util.Collections.singleton;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.NonNull;
import xyz.leutgeb.lorenz.lac.ast.sources.Derived;
import xyz.leutgeb.lorenz.lac.ast.sources.Predefined;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.typing.simple.types.BoolType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.unification.UnificationContext;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.Util;

public class Identifier extends Expression {
  public static final String LEAF_NAME = "leaf";
  public static final Identifier LEAF = new Identifier(Predefined.INSTANCE, LEAF_NAME);
  public static final Identifier UNDERSCORE = new Identifier(Predefined.INSTANCE, "_");
  public static final Identifier DUMMY_TREE_ALPHA =
      new Identifier(Predefined.INSTANCE, "_", new TreeType(TypeVariable.ALPHA));
  private static final Set<String> BOOLEAN_NAMES = Set.of("true", "false");
  private static final Set<String> CONSTANT_NAMES = Sets.union(BOOLEAN_NAMES, singleton(LEAF_NAME));

  @NonNull @Getter private final String name;
  @Getter private Intro intro;

  private static int anonymousCount = 1;

  private Identifier(Source source, @NonNull String name) {
    super(source);
    Objects.requireNonNull(name);
    this.name = name;
  }

  public Identifier(Source source, @NonNull String name, Type type, Intro intro) {
    super(source, type);
    Objects.requireNonNull(name);
    this.name = name;
    this.intro = intro;
  }

  private static Identifier predefined(String name, Type type) {
    return new Identifier(Predefined.INSTANCE, name, type, SystemIntro.INSTANCE);
  }

  public static Identifier predefinedBase(String name, TypeVariable type) {
    return new Identifier(Predefined.INSTANCE, name, type, SystemIntro.INSTANCE);
  }

  public static Identifier predefinedBase(String name) {
    return predefinedBase(name, TypeVariable.ALPHA);
  }

  public static Identifier predefinedTree(String name) {
    return predefinedTree(name, TypeVariable.ALPHA);
  }

  public static Identifier predefinedTree(String name, TypeVariable typeVariable) {
    return predefined(name, new TreeType(typeVariable));
  }

  @Deprecated
  public Identifier(Source source, @NonNull String name, Type type) {
    this(source, name, type, SystemIntro.INSTANCE);
  }

  public static Identifier getSugar(Source source, IntIdGenerator idGenerator) {
    return get("z" + /*Util.generateSubscript*/ (idGenerator.next()), source);
  }

  public static Identifier get(String name, Source source) {
    return new Identifier(source, name);
  }

  private static boolean isConstant(String name) {
    return CONSTANT_NAMES.contains(name);
  }

  public static Expression anonymous(Source source) {
    return get("_" + (anonymousCount++), source);
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
    if (name.equals(LEAF_NAME)) {
      return new TreeType(context.fresh());
    }
    if (BOOLEAN_NAMES.contains(name)) {
      return BoolType.INSTANCE;
    }
    if (name.startsWith("_")) {
      return context.fresh();
    }

    if (context.hasSignature(this.name)) {
      return context.getSignatures().get(this.name).getType();
    }
    this.intro = context.getIntro(this.name);
    return context.getType(this.name);
  }

  @Override
  public Expression normalize(Stack<Normalization> context, IntIdGenerator idGenerator) {
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

    return name.equals(that.name) && Objects.equals(intro, that.intro);
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
    return singleton(this);
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
    return new Identifier(Derived.rename(this), renaming.getOrDefault(name, name), type, intro);
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation, String currentFunction) {
    if (name.equals((LEAF_NAME))) {
      out.print("Leaf");
    } else if (BOOLEAN_NAMES.contains(name)) {
      out.print(Util.capitalizeFirstLetter(name));
    } else {
      out.print(name);
    }
  }

  @Override
  public void printJavaTo(PrintStream out, int indentation, String currentFunction) {
    if (name.equals((LEAF_NAME))) {
      out.print("Tree.<" + type.variables().iterator().next() + ">leaf()");
    } else {
      out.print(name);
    }
  }

  @Override
  public Expression unshare(IntIdGenerator idGenerator, boolean lazy) {
    return this;
  }
}
