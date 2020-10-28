import xyz.leutgeb.lorenz.lac.*;
import xyz.leutgeb.lorenz.lac.ast.*;
import xyz.leutgeb.lorenz.lac.util.*;

import static xyz.leutgeb.lorenz.lac.Tree.leaf;
import static xyz.leutgeb.lorenz.lac.Tree.node;
import static xyz.leutgeb.lorenz.lac.util.Util.readProperties;

import xyz.leutgeb.lorenz.lac.module.Loader;

// Load configuration from `lac.properties`.
readProperties(Path.of("lac.properties"));

Path home = Path.of("src", "test", "resources", "examples");
Loader loader = new Loader(home);
loader.autoload();

Path compile(String source) {
  try {
    Program program = loader.loadInline(source);
    Path temporary = Files.createTempFile("lac", ".jsh");
    program.dumpToJsh(temporary);
    return temporary;
  } catch (Exception e) {
    throw new RuntimeException(e);
  }
}

void loaded() {
  loader.getFunctionDefinitions().keySet().stream().sorted().forEach(System.out::println);
}