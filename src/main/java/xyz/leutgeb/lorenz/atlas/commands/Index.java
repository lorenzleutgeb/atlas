package xyz.leutgeb.lorenz.atlas.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;

import java.io.IOException;
import java.util.regex.Pattern;
import lombok.extern.slf4j.Slf4j;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.ast.sources.Parsed;
import xyz.leutgeb.lorenz.atlas.module.Loader;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;

@CommandLine.Command(name = "index")
@Slf4j
public class Index implements Runnable {
  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be transpiled. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

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
    try {
      program.infer();
    } catch (UnificationError | TypeError unificationError) {
      throw new RuntimeException(unificationError);
    }

    final var beginIndex = Loader.getDefaultHome().toAbsolutePath().toString().length() + 1;
    for (var entry : program.getFunctionDefinitions().entrySet()) {
      var fd = entry.getValue();
      final Parsed source = (Parsed) fd.getBody().getSource().getRoot();
      System.out.println(
          fd.getFullyQualifiedName()
              + "\t"
              + source.getPath().toString().substring(beginIndex)
              + "\t"
              + "/^"
              + fd.getName()
              + "/;\"\tline:"
              + +source.getTree().getStart().getLine()
              + "\tend:"
              + source.getTree().getStop().getLine());
    }
  }
}
