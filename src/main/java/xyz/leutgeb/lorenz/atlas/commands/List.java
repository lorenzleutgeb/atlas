package xyz.leutgeb.lorenz.atlas.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;

import java.io.IOException;
import java.util.regex.Pattern;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.module.Loader;

@CommandLine.Command(name = "list")
public class List implements Runnable {
  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be listed. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

  @CommandLine.Spec(CommandLine.Spec.Target.SELF)
  private CommandLine.Model.CommandSpec selfSpec;

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
      System.exit(1);
    }
    program.printAllAnnotatedAnnotationsAndBoundsInOrder(System.out);
  }
}
