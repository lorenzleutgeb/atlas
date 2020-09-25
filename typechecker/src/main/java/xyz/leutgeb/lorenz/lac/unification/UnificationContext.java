package xyz.leutgeb.lorenz.lac.unification;

import static com.google.common.collect.Sets.difference;

import com.google.common.collect.Iterators;
import java.util.*;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.ast.Intro;
import xyz.leutgeb.lorenz.lac.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;
import xyz.leutgeb.lorenz.lac.util.IntIdGenerator;
import xyz.leutgeb.lorenz.lac.util.Util;

@Slf4j
@Value
public class UnificationContext {
  UnificationContext parent;

  /**
   * Holds types for identifiers. This is extended through {@link #putType(String, Type)}, for
   * example by walking over {@link xyz.leutgeb.lorenz.lac.ast.LetExpression}.
   */
  Map<String, Type> types;

  Map<String, Intro> intros;

  Set<String> hidden;

  Map<String, FunctionSignature> signatures;

  String functionInScope;

  LinkedList<Equivalence> equivalences;

  IntIdGenerator intIdGenerator;

  private UnificationContext(
      UnificationContext parent,
      Map<String, Type> types,
      Map<String, Intro> intros,
      Set<String> hidden,
      Map<String, FunctionSignature> signatures,
      String functionInScope,
      LinkedList<Equivalence> equivalences,
      IntIdGenerator intIdGenerator) {
    this.parent = parent;
    this.types = types;
    this.intros = intros;
    this.hidden = hidden;
    this.signatures = signatures;
    this.functionInScope = functionInScope;
    this.equivalences = equivalences;
    this.intIdGenerator = intIdGenerator;
  }

  public static UnificationContext root() {
    return new UnificationContext(
        null,
        new LinkedHashMap<>(),
        new HashMap<>(),
        new HashSet<>(),
        new HashMap<>(),
        null,
        new LinkedList<>(),
        new IntIdGenerator());
  }

  public UnificationContext child() {
    return new UnificationContext(
        this,
        new LinkedHashMap<>(),
        new HashMap<>(),
        new HashSet<>(),
        this.getSignatures(),
        this.functionInScope,
        this.equivalences,
        this.intIdGenerator);
  }

  public UnificationContext childWithNewVariables(String fqn) {
    return new UnificationContext(
        null,
        new LinkedHashMap<>(),
        new HashMap<>(),
        new HashSet<>(),
        this.getSignatures(),
        fqn,
        this.equivalences,
        this.intIdGenerator);
  }

  public UnificationContext childWithNewProblem() {
    return new UnificationContext(
        null,
        new LinkedHashMap<>(),
        new HashMap<>(),
        new HashSet<>(),
        this.getSignatures(),
        null,
        new LinkedList<>(),
        new IntIdGenerator());
  }

  public String toString() {
    return "["
        + this.equivalences.stream()
            .map(Object::toString)
            .collect(Collectors.joining(", ", "{", "}"))
        + " {"
        + this.types.entrySet().stream()
            .map(e -> e.getKey() + " :: " + e.getValue())
            .collect(Collectors.joining(", "))
        + "}]";
  }

  /** Recursively looks up the signature of some identifier (given as {@link String}). */
  public @Nonnull Type getType(final String id) throws TypeError {
    if (isHiddenRecursive(id)) {
      throw new TypeError("'" + id + "' is out of scope.");
    }
    Type t = getTypeRecursive(id);
    if (t != null) {
      return t;
    } else {
      var similar = Util.similar(id, iterateIdentifiers(), 0.5, 4);
      var message = "'" + id + "' is not defined.";
      if (similar.isEmpty()) {
        message += " (Did you mean " + similar + "?)";
      }
      throw new TypeError(message);
    }
  }

  public Intro getIntro(final String id) {
    return getIntroRecursive(id);
  }

  public @Nonnull FunctionSignature getSignature(final String fqn) throws TypeError {
    FunctionSignature t = signatures.get(fqn);
    if (t != null) {
      return t;
    } else {
      var similar = Util.similar(fqn, signatures.keySet().iterator(), 0.5, 4);
      var message = "'" + fqn + "' is not defined.";
      if (similar.isEmpty()) {
        message += " (Did you mean " + similar + "?)";
      }
      throw new TypeError(message);
    }
  }

  private Type getTypeInternal(final String key) {
    return isHiddenRecursive(key) ? null : getTypeRecursive(key);
  }

  private boolean isHiddenRecursive(final String key) {
    return hidden.contains(key) || (parent != null && parent.isHiddenRecursive(key));
  }

  private Type getTypeRecursive(final String key) {
    Type t = types.get(key);
    if (t != null) {
      return t;
    } else if (parent != null) {
      return parent.getTypeRecursive(key);
    } else {
      return null;
    }
  }

  private Intro getIntroRecursive(final String key) {
    Intro t = intros.get(key);
    if (t != null) {
      return t;
    } else if (parent != null) {
      return parent.getIntroRecursive(key);
    } else {
      return null;
    }
  }

  private boolean hasType(String key) {
    if (hidden.contains(key)) {
      return false;
    }
    Type t = types.get(key);
    if (t != null) {
      return true;
    } else if (parent != null) {
      return parent.hasType(key);
    } else {
      return false;
    }
  }

  public UnificationContext hide(String... variables) {
    final var result = child();
    result.hidden.addAll(Arrays.asList(variables));
    return result;
  }

  public void putType(String key, Type value, Expression intro) {
    if (value instanceof FunctionType) {
      throw new IllegalArgumentException("use putSignature");
    }

    if ("leaf".equals(key)) { // || "_".equals(key)) {
      // Silently ignore this, since leaf and _ will have a magic type assigned in Identifier.
      return;
    }

    if (getTypeInternal(key) != null) {
      throw new RuntimeException("hiding of variables not possible (affected: '" + key + "')");
    }

    types.put(key, value);
    intros.put(key, new Intro(this.functionInScope, intro));
  }

  private Iterator<String> iterateIdentifiers() {
    final var names = difference(types.keySet(), hidden);
    final var it = names.iterator();
    return parent == null
        ? it
        : Iterators.concat(
            it, Iterators.filter(parent.iterateIdentifiers(), name -> !hidden.contains(name)));
  }

  public void putSignature(String fullyQualifiedName, FunctionSignature functionSignature) {
    signatures.put(fullyQualifiedName, functionSignature);
  }

  // TODO(lorenz.leutgeb): Find out why exactly we need this method.
  public boolean hasSignature(String fqn) {
    return signatures.containsKey(fqn);
  }

  public void addIfNotEqual(Type a, Type b) {
    Objects.requireNonNull(a);
    Objects.requireNonNull(b);
    if (!a.equals(b)) {
      equivalences.add(new Equivalence(a, b));
    }
  }

  public UnificationVariable fresh() {
    return new UnificationVariable(intIdGenerator.next());
  }
}
