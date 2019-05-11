package xyz.leutgeb.lorenz.logs;

import com.google.common.collect.Iterators;
import com.google.common.collect.Sets;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.Value;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.resources.Constraints;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationProblem;
import xyz.leutgeb.lorenz.logs.values.TreeValue;

// TODO(lorenz.leutgeb): Use separate context classes for simple signature inference,
// constraint generation, and evaluation.
@Log4j2
@Value
public class Context {
  private static final Context INTERNAL_ROOT = new Context();

  static {
    INTERNAL_ROOT.types.put("true", BoolType.INSTANCE);
    INTERNAL_ROOT.types.put("false", BoolType.INSTANCE);

    INTERNAL_ROOT.values.put("true", true);
    INTERNAL_ROOT.values.put("false", false);
    INTERNAL_ROOT.values.put("nil", TreeValue.nil());
  }

  public static Context root() {
    return new Context(INTERNAL_ROOT, new UnificationProblem(), new Constraints());
  }

  Context parent;

  /**
   * Holds types for identifiers. This is pre-populated for constants (true, false) and extended
   * through {@link #putType(String, Type)}, for example by walking over {@link
   * xyz.leutgeb.lorenz.logs.ast.LetExpression}.
   */
  Map<String, Type> types;

  Map<String, Object> values;

  Map<String, Annotation> annotations;

  /** For simple signature inference. */
  UnificationProblem problem;

  /** For constructing resource constraints. */
  Constraints constraints;

  private Context(Context parent) {
    this(parent, parent.problem, parent.constraints);
  }

  private Context() {
    this(null, new UnificationProblem(), new Constraints());
  }

  private Context(Context parent, UnificationProblem problem, Constraints constraints) {
    this.parent = parent;
    this.problem = problem;
    this.constraints = constraints;
    this.types = new HashMap<>();
    this.values = new HashMap<>();
    this.annotations = new HashMap<>();
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
  // TODO(lorenz.leutgeb): This behaves differently in simple signature inference and in constraint
  // generation.
  public @Nonnull Type lookupType(final String key) {
    Type t = lookupTypeInternal(key);
    if (t != null) {
      return t;
    } else {
      var similar = Util.similar(key, iterateIdentifiers(), 0.5, 4);
      if (similar.isEmpty()) {
        throw new RuntimeException("'" + key + "' is not defined ");
      }
      throw new RuntimeException("'" + key + "' is not defined (alternatives: " + similar + ")");
    }
  }

  private Type lookupTypeInternal(final String key) {
    Type t = types.get(key);
    if (t != null) {
      return t;
    } else if (parent != null) {
      return parent.lookupTypeInternal(key);
    } else {
      return null;
    }
  }

  public Annotation lookupAnnotation(final String key) {
    var t = annotations.get(key);
    if (t != null) {
      return t;
    } else if (parent != null) {
      return parent.lookupAnnotation(key);
    } else {
      throw new NoSuchElementException();
    }
  }

  private boolean hasType(String key) {
    Type t = types.get(key);
    if (t != null) {
      return true;
    } else if (parent != null) {
      return parent.hasType(key);
    } else {
      return false;
    }
  }

  @Deprecated
  public void remove(String key) {
    types.remove(key);
  }

  public void putType(String key, Type value) {
    if ("nil".equals(key) || "_".equals(key)) {
      // Silently ignore this, since nil and _ will have a magic type assigned in Identifier.
      return;
    }

    if (hasType(key)) {
      log.info("Hiding " + key);
    }
    types.put(key, value);
  }

  public void putAnnotation(String key, Annotation value) {
    if ("nil".equals(key) || "_".equals(key)) {
      throw new RuntimeException("this probably is a bad idea...");
    }
    annotations.put(key, value);
  }

  public void putValue(String key, Type value) {
    if ("nil".equals(key) || "_".equals(key)) {
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
    var it = Sets.union(types.keySet(), values.keySet()).iterator();
    if (parent == null) {
      return it;
    }
    return Iterators.concat(it, parent.iterateIdentifiers());
  }
}
