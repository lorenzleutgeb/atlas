import xyz.leutgeb.lorenz.lac.*;
import xyz.leutgeb.lorenz.lac.ast.*;
import xyz.leutgeb.lorenz.lac.module.Loader;
import xyz.leutgeb.lorenz.lac.util.*;

import static xyz.leutgeb.lorenz.lac.Tree.leaf;
import static xyz.leutgeb.lorenz.lac.Tree.node;
import static xyz.leutgeb.lorenz.lac.util.Util.readProperties;

readProperties(Path.of("lac.properties"));

Loader loader = Loader.atDefaultHome();
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
