package xyz.leutgeb.lorenz.logs;

import java.util.HashMap;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.logs.ast.Identifier;
import xyz.leutgeb.lorenz.logs.type.BoolType;
import xyz.leutgeb.lorenz.logs.type.Type;

@Deprecated
@Log4j2
public class SymbolTable {
  public static SymbolTable root() {
    SymbolTable root = new SymbolTable();
    // root.entries.put(Identifier.NIL, new Entry(SplayType.BASE, null));
    root.entries.put(Identifier.TRUE, new Entry(BoolType.INSTANCE, null));
    root.entries.put(Identifier.FALSE, new Entry(BoolType.INSTANCE, null));
    return root;
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  public static class Entry {
    Type type;
    Token start;
  }

  private final SymbolTable parent;
  private final Map<Identifier, Entry> entries = new HashMap<>();

  private SymbolTable() {
    this.parent = null;
  }

  public SymbolTable(SymbolTable parent) {
    this.parent = parent;
  }

  public boolean containsKey(Identifier key) {
    if (entries.containsKey(key)) {
      return true;
    }
    if (parent != null) {
      return parent.containsKey(key);
    }
    return false;
  }

  public Entry get(Identifier key) {
    Entry result = entries.get(key);

    if (result == null && parent != null) {
      return parent.get(key);
    }

    throw new IllegalStateException("Could not get symbol.");
  }

  public void put(Identifier key, Entry value) {
    if (containsKey(key)) {
      log.info("Hiding variable expression " + key);
    }
    entries.put(key, value);
  }
}
