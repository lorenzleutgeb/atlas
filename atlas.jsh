import xyz.leutgeb.lorenz.atlas.*;
import xyz.leutgeb.lorenz.atlas.ast.*;
import xyz.leutgeb.lorenz.atlas.module.Loader;
import xyz.leutgeb.lorenz.atlas.util.*;

import static xyz.leutgeb.lorenz.atlas.Tree.leaf;
import static xyz.leutgeb.lorenz.atlas.Tree.node;
import static xyz.leutgeb.lorenz.atlas.util.Util.readProperties;

readProperties(Path.of("atlas.properties"));

Loader loader = Loader.atDefaultHome();
loader.autoload();

Path compile(String source) {
  try {
    Program program = loader.loadInline(source);
    Path temporary = Files.createTempFile("atlas", ".jsh");
    program.dumpToJsh(temporary);
    return temporary;
  } catch (Exception e) {
    throw new RuntimeException(e);
  }
}

void loaded() {
  loader.getFunctionDefinitions().keySet().stream().sorted().forEach(System.out::println);
}
