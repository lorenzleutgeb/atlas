package xyz.leutgeb.lorenz.logs;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.Value;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.resources.Constraints;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificiationProblem;

// TODO(lorenz.leutgeb): Use separate context classes for simple signature inference
// and constraint generation.
@Log4j2
@Value
public class Context {
  private static final Context INTERNAL_ROOT = new Context();

  static {
    INTERNAL_ROOT.mapping.put("true", BoolType.INSTANCE);
    INTERNAL_ROOT.mapping.put("false", BoolType.INSTANCE);
  }

  public static Context root() {
    return new Context(INTERNAL_ROOT, new UnificiationProblem(), new Constraints());
  }

  Context parent;

  /**
   * Holds types for identifiers. This is pre-populated for constants (true, false) and extended
   * through {@link #put(String, Type)}, for example by walking over {@link
   * xyz.leutgeb.lorenz.logs.ast.LetExpression}.
   */
  Map<String, Type> mapping;

  /** For simple signature inference. */
  UnificiationProblem problem;

  /** For constructing resource constraints. */
  Constraints constraints;

  private Context(Context parent) {
    this(parent, parent.problem, parent.constraints);
  }

  private Context() {
    this(null, new UnificiationProblem(), new Constraints());
  }

  private Context(Context parent, UnificiationProblem problem, Constraints constraints) {
    this.parent = parent;
    this.problem = problem;
    this.constraints = constraints;
    this.mapping = new HashMap<>();
  }

  public Context child() {
    return new Context(this);
  }

  public String toString() {
    return "["
        + this.problem.toString()
        + " {"
        + this.mapping
            .entrySet()
            .stream()
            .map(e -> e.getKey() + " :: " + e.getValue())
            .collect(Collectors.joining(", "))
        + "}]";
  }

  /** Recursively looks up the signature of some identifier (given as {@link String}). */
  // TODO(lorenz.leutgeb): This behaves differently in simple signature inference and in constraint
  // generation.
  public Type lookup(String key) {
    Type t = mapping.get(key);
    if (t != null) {
      return t;
    } else if (parent != null) {
      return parent.lookup(key);
    } else {
      return null;
    }
  }

  @Deprecated
  public void remove(String key) {
    mapping.remove(key);
  }

  public void put(String key, Type value) {
    if ("nil".equals(key) || "_".equals(key)) {
      return;
    }

    if (lookup(key) != null) {
      log.info("Hiding " + key);
    }
    mapping.put(key, value);
  }
}
