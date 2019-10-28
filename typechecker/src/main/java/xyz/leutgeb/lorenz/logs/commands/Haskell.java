package xyz.leutgeb.lorenz.logs.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.logs.Loader;
import xyz.leutgeb.lorenz.logs.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.logs.ast.Program;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@CommandLine.Command(name = "hs")
public class Haskell implements Runnable {
  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be transpiled. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

  @CommandLine.Option(defaultValue = ".", names = "--home")
  private Path home;

  @CommandLine.Parameters(
      index = "1",
      arity = "1",
      paramLabel = "haskellSearchPath",
      description = "Path where Haskell output should be rooted at.",
      defaultValue = ".",
      showDefaultValue = ALWAYS)
  private Path path;

  @Override
  public void run() {
    Loader loader = new Loader(home);
    try {
      loader.autoload();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    Program program;
    try {
      program = loader.loadMatching(pattern);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    try {
      program.infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }
    Multimap<String, FunctionDefinition> output = ArrayListMultimap.create();
    Multimap<String, String> imports = HashMultimap.create();
    for (var fd : program.getFunctionDefinitions()) {
      if (pattern.asMatchPredicate().test(fd.getFullyQualifiedName())) {
        output.put(fd.getModuleName(), fd);
        imports.putAll(
            fd.getModuleName(),
            fd.importedFunctions().stream().map(Loader::moduleName).collect(Collectors.toSet()));
      }
    }
    for (var e : output.keySet()) {
      var path = Loader.path(e, this.path, ".hs");
      var lastModulePart = e.substring(Math.max(0, e.lastIndexOf(".")));
      try (var stream = new PrintStream(new FileOutputStream(path.toFile()))) {
        stream.println("-- This file was generated automatically.");
        stream.println();
        stream.println("module " + lastModulePart + " where");
        stream.println("import InterpreterPrelude (Tree(Node, Nil))");
        stream.println();
        for (String imp : imports.get(e)) {
          stream.println("import qualified " + imp);
        }
        for (var fd : output.get(e)) {
          fd.printHaskellTo(stream);
        }
      } catch (IOException ex) {
        ex.printStackTrace();
      }
      System.out.println(path);
    }
  }
}
