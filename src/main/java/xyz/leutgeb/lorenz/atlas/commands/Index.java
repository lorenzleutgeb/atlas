package xyz.leutgeb.lorenz.atlas.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;
import static xyz.leutgeb.lorenz.atlas.util.Util.output;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import lombok.extern.slf4j.Slf4j;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.ast.sources.Parsed;
import xyz.leutgeb.lorenz.atlas.module.Loader;

@CommandLine.Command(name = "index")
@Slf4j
public class Index implements Runnable {
  @CommandLine.Option(
      names = "--matches",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be indexed. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

  @CommandLine.Parameters Optional<String> outputPath;

  @Override
  public void run() {
    Loader loader = Loader.atDefaultHome();
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

    if (outputPath.isPresent()) {
      try (final var out = output(Path.of(outputPath.get()))) {
        printTags(program, out);
      } catch (IOException e) {
        e.printStackTrace();
        System.exit(1);
      }
    } else {
      printTags(program, System.out);
    }
  }

  private void printTags(Program program, PrintStream out) {
    final var beginIndex = Loader.getDefaultSearch().toAbsolutePath().toString().length() + 1;
    final List<Map.Entry<String, FunctionDefinition>> entries =
        program.getFunctionDefinitions().entrySet().stream()
            .sorted(Map.Entry.comparingByKey())
            .toList();
    for (var entry : entries) {
      var fd = entry.getValue();
      final Parsed source = (Parsed) fd.getBody().getSource().getRoot();
      if (out != System.out) {
        System.out.println(fd.getFullyQualifiedName());
      }
      out.println(
          fd.getFullyQualifiedName()
              + "\t"
              + source.getPath().toString().substring(beginIndex)
              + "\t"
              + "/^"
              + fd.getName()
              + "/;\"\tline:"
              + source.getTree().getStart().getLine()
              + "\tend:"
              + source.getTree().getStop().getLine());
    }
  }
}
