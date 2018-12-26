package xyz.leutgeb.lorenz.logs;

import java.util.HashMap;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.logs.ast.VariableExpression;

@Log4j2
public class SymbolTable {
  public static SymbolTable root() {
    SymbolTable root = new SymbolTable();
    root.entries.put(new VariableExpression("true"), new Entry(SplayType.BOOLEAN, null));
    root.entries.put(new VariableExpression("false"), new Entry(SplayType.BOOLEAN, null));
    root.entries.put(new VariableExpression("nil"), new Entry(SplayType.BASE, null));
    return root;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  public static class Entry {
    SplayType type;
    Token start;
  }

  private final SymbolTable parent;
  private final Map<VariableExpression, Entry> entries = new HashMap<>();

  private SymbolTable() {
    this.parent = null;
  }

  public SymbolTable(SymbolTable parent) {
    this.parent = parent;
  }

  public boolean containsKey(VariableExpression key) {
    if (entries.containsKey(key)) {
      return true;
    }
    if (parent != null) {
      return parent.containsKey(key);
    }
    return false;
  }

  public Entry get(VariableExpression key) {
    Entry result = entries.get(key);

    if (result == null && parent != null) {
      return parent.get(key);
    }

    throw new IllegalStateException("Could not get symbol.");
  }

  public void put(VariableExpression key, Entry value) {
    if (containsKey(key)) {
      log.info("Hiding variable expression " + key);
    }
    entries.put(key, value);
  }
}
