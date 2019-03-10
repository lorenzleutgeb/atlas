package xyz.leutgeb.lorenz.logs;

import lombok.Value;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.unification.Problem;

import java.util.HashMap;
import java.util.Map;

@Log4j2
@Value
public class Context {
  private static final Context INTERNAL_ROOT = new Context();

  static {
    INTERNAL_ROOT.mapping.put("true", BoolType.INSTANCE);
    INTERNAL_ROOT.mapping.put("false", BoolType.INSTANCE);
  }

  public static Context root() {
    return new Context(INTERNAL_ROOT, new Problem());
  }

  Context parent;
  Map<String, Type> mapping;
  Problem problem;

  public Context(Context parent) {
    this(parent, parent.problem);
  }

  private Context() {
    this(null, new Problem());
  }

  private Context(Context parent, Problem problem) {
    this.parent = parent;
    this.problem = problem;
    this.mapping = new HashMap<>();
  }

  public String toString() {
    return "[" + this.problem.toString() + " " + this.mapping.toString() + "]";
  }

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

  public void remove(String key) {
    mapping.remove(key);
  }

  public void put(String key, Type value) {
    // if (lookup(key) != null) {
    //   log.info("Hiding " + key);
    // }
    mapping.put(key, value);
  }
}
