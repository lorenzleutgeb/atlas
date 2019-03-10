package xyz.leutgeb.lorenz.logs;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import lombok.extern.log4j.Log4j2;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.unification.Problem;

@Log4j2
public class Context {
  public static Context root() {
    Context root = new Context();
    root.mapping.put("true", new LinkedList<>(Collections.singletonList((BoolType.INSTANCE))));
    root.mapping.put("false", new LinkedList<>(Collections.singletonList(BoolType.INSTANCE)));
    // root.mapping.put("nil", new LinkedList<>(Collections.singletonList(new TypeVar("niltype"))));
    return root;
  }

  private final Context parent;
  private final HashMap<String, LinkedList<Type>> mapping;
  private Problem problem;

  public Context(Context parent) {
    this.parent = parent;
    this.problem = parent.problem;
    this.mapping = new HashMap<>();
  }

  private Context() {
    this.parent = null;
    this.problem = new Problem();
    this.mapping = new HashMap<>();
  }

  public Problem getProblem() {
    return this.problem;
  }

  public String toString() {
    return "Context(" + this.problem.toString() + "," + this.mapping.toString() + ")";
  }

  public Type lookup(String key) {
    LinkedList<Type> t = this.mapping.get(key);
    if (t != null) {
      return t.peekFirst();
    } else if (parent != null) {
      return parent.lookup(key);
    } else {
      return null;
    }
  }

  public void delete(String key) {
    LinkedList<Type> t = this.mapping.get(key);
    if (t != null) {
      t.removeFirst();
      if (t.isEmpty()) {
        this.mapping.remove(key);
      }
    }
  }

  public void insert(String key, Type value) {
    log.info(key + " -- " + value + ";");
    if (lookup(key) != null) {
      // log.info("Hiding " + key);
    }
    LinkedList<Type> t = this.mapping.get(key);
    if (t != null) {
      t.addFirst(value);
    } else {
      LinkedList<Type> l = new LinkedList<>();
      l.addFirst(value);
      this.mapping.put(key, l);
    }
  }
}
