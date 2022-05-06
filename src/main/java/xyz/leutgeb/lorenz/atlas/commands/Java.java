package xyz.leutgeb.lorenz.atlas.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;
import static xyz.leutgeb.lorenz.atlas.util.Util.output;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.common.io.Files;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.module.Loader;

@CommandLine.Command(name = "java")
@Slf4j
public class Java implements Runnable {
  @CommandLine.Parameters(
      index = "1",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be transpiled. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

  @CommandLine.Spec(CommandLine.Spec.Target.SELF)
  private CommandLine.Model.CommandSpec selfSpec;

  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "javaRoot",
      description = "Path where Java output should be rooted at.",
      defaultValue = ".",
      showDefaultValue = ALWAYS)
  private Path path;

  @Override
  public void run() {
    final var loader = Loader.atDefaultHome();
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
    program.normalize();
    if (!program.infer()) {
      return;
    }
    Multimap<String, FunctionDefinition> output = ArrayListMultimap.create();
    Multimap<String, String> imports = HashMultimap.create();
    for (var entry : program.getFunctionDefinitions().entrySet()) {
      var fd = entry.getValue();
      if (pattern.asMatchPredicate().test(fd.getFullyQualifiedName())) {
        output.put(fd.getModuleName(), fd);
        imports.putAll(
            fd.getModuleName(),
            fd.importedFunctions().stream().map(Loader::moduleName).collect(Collectors.toSet()));
      }
    }
    for (var e : output.keySet()) {
      var path =
          Loader.path(e, this.path.resolve(Path.of("xyz", "leutgeb", "lorenz", "atlas")), ".java");
      var lastModulePart = e.substring(Math.max(0, e.lastIndexOf(".")));

      try {
        java.nio.file.Files.createDirectories(path.getParent());
      } catch (IOException ioException) {
        throw new RuntimeException(ioException);
      }

      final var sink = Files.asCharSink(path.toFile(), StandardCharsets.UTF_8);

      try (var stream = new PrintStream(output(path))) {
        stream.println("// This file was generated automatically.");
        stream.println();
        stream.println("package xyz.leutgeb.lorenz.atlas;");
        stream.println();
        stream.println("import java.util.Objects;");
        stream.println("import xyz.leutgeb.lorenz.atlas.Tree;");
        stream.println("import static xyz.leutgeb.lorenz.atlas.Tree.node;");
        stream.println("import static xyz.leutgeb.lorenz.atlas.Tree.leaf;");
        stream.println();
        for (String imp : imports.get(e)) {
          stream.println("import xyz.leutgeb.lorenz.atlas." + imp + ";");
        }
        stream.println("public final class " + lastModulePart + " {");
        stream.println();
        for (var fd : output.get(e)) {
          fd.printJavaTo(stream, true);
          System.out.println(fd.getFullyQualifiedName());
        }
        stream.println("}");
      } catch (IOException ex) {
        throw new RuntimeException(ex);
      }
      log.info("{}", path);
    }
  }
}
