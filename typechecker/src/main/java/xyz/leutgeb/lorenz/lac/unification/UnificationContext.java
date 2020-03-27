package xyz.leutgeb.lorenz.lac.unification;

import static com.google.common.collect.Sets.difference;

import com.google.common.collect.Iterators;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.Value;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.typing.simple.FunctionSignature;
import xyz.leutgeb.lorenz.lac.typing.simple.types.FunctionType;
import xyz.leutgeb.lorenz.lac.typing.simple.types.Type;

@Log4j2
@Value
public class UnificationContext {
  UnificationContext parent;
  /**
   * Holds types for identifiers. This is pre-populated for constants (true, false) and extended
   * through {@link #putType(String, Type)}, for example by walking over {@link
   * xyz.leutgeb.lorenz.lac.ast.LetExpression}.
   */
  Map<String, Type> types;
  /** For simple signature inference. */
  UnificationProblem problem;

  Set<String> hidden;
  Map<String, FunctionSignature> signatures;

  private UnificationContext(UnificationContext parent) {
    this(parent, parent.problem);
  }

  private UnificationContext() {
    this(null, new UnificationProblem());
  }

  private UnificationContext(UnificationContext parent, UnificationProblem problem) {
    this.parent = parent;
    this.problem = problem;
    this.types = new LinkedHashMap<>();
    this.hidden = new HashSet<>();
    this.signatures = parent == null ? new HashMap<>() : parent.signatures;
  }

  public static UnificationContext root() {
    return new UnificationContext();
  }

  public UnificationContext child() {
    return new UnificationContext(this);
  }

  public UnificationContext childWithNewUnfication() {
    return new UnificationContext(this, new UnificationProblem());
  }

  public String toString() {
    return "["
        + this.problem.toString()
        + " {"
        + this.types.entrySet().stream()
            .map(e -> e.getKey() + " :: " + e.getValue())
            .collect(Collectors.joining(", "))
        + "}]";
  }

  /** Recursively looks up the signature of some identifier (given as {@link String}). */
  public @Nonnull Type lookupType(final String key) {
    Type t = lookupTypeInternal(key);
    if (t != null) {
      return t;
    } else {
      var similar = Util.similar(key, iterateIdentifiers(), 0.5, 4);
      if (similar.isEmpty()) {
        throw new RuntimeException("'" + key + "' is not defined ");
      }
      throw new RuntimeException("'" + key + "' is not defined. (Did you mean " + similar + "?)");
    }
  }

  public @Nonnull FunctionSignature lookupSignature(final String fqn) {
    FunctionSignature t = signatures.get(fqn);
    if (t != null) {
      return t;
    } else {
      var similar = Util.similar(fqn, iterateIdentifiers(), 0.5, 4);
      if (similar.isEmpty()) {
        throw new RuntimeException("'" + fqn + "' is not defined ");
      }
      throw new RuntimeException("'" + fqn + "' is not defined. (Did you mean " + similar + "?)");
    }
  }

  private Type lookupTypeInternal(final String key) {
    if (hidden.contains(key)) {
      return null;
    }
    Type t = types.get(key);
    if (t != null) {
      return t;
    } else if (parent != null) {
      return parent.lookupTypeInternal(key);
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

  public void putType(String key, Type value) {
    if (value instanceof FunctionType) {
      throw new IllegalArgumentException("use putSignature");
    }

    if ("leaf".equals(key)) { // || "_".equals(key)) {
      // Silently ignore this, since leaf and _ will have a magic type assigned in Identifier.
      return;
    }

    if (hasType(key)) {
      throw new RuntimeException("hiding of variables not possible (affected: '" + key + "')");
    }

    types.put(key, value);
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

  public boolean hasSignature(String fqn) {
    return signatures.containsKey(fqn);
  }
}
