package xyz.leutgeb.lorenz.logs;

import com.google.common.collect.Iterators;
import com.google.common.collect.Sets;
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
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;

// TODO(lorenz.leutgeb): Use separate context classes for simple signature inference,
// constraint generation, and evaluation.
@Log4j2
@Value
public class Context {
  public static Context root() {
    return new Context();
  }

  Context parent;

  /**
   * Holds types for identifiers. This is pre-populated for constants (true, false) and extended
   * through {@link #putType(String, Type)}, for example by walking over {@link
   * xyz.leutgeb.lorenz.logs.ast.LetExpression}.
   */
  Map<String, Type> types;

  Map<String, Object> values;

  /** For simple signature inference. */
  UnificationProblem problem;

  Set<String> hidden;

  private Context(Context parent) {
    this(parent, parent.problem);
  }

  private Context() {
    this(null, new UnificationProblem());
  }

  private Context(Context parent, UnificationProblem problem) {
    this.parent = parent;
    this.problem = problem;
    this.types = new LinkedHashMap<>();
    this.values = new HashMap<>();
    this.hidden = new HashSet<>();
  }

  public Context child() {
    return new Context(this);
  }

  public String toString() {
    return "["
        + this.problem.toString()
        + " {"
        + this.types
            .entrySet()
            .stream()
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

  public Context hide(String... variables) {
    final var result = child();
    result.hidden.addAll(Arrays.asList(variables));
    return result;
  }

  public void putType(String key, Type value) {
    if ("nil".equals(key)) { // || "_".equals(key)) {
      // Silently ignore this, since nil and _ will have a magic type assigned in Identifier.
      return;
    }

    if (hasType(key)) {
      log.info("Hiding " + key);
    }
    types.put(key, value);
  }

  public void putValue(String key, Type value) {
    if ("nil".equals(key)) { // || "_".equals(key)) {
      throw new RuntimeException("nil and _ cannot be redefined");
    }

    /*
    if (hasValue(key) != null) {
      log.info("Overriding " + key);
    }
    */
    values.put(key, value);
  }

  private Iterator<String> iterateIdentifiers() {
    final var names = Sets.difference(Sets.union(types.keySet(), values.keySet()), hidden);
    final var it = names.iterator();
    return parent == null
        ? it
        : Iterators.concat(
            it, Iterators.filter(parent.iterateIdentifiers(), name -> !hidden.contains(name)));
  }
}
