package xyz.leutgeb.lorenz.atlas.commands;

import static xyz.leutgeb.lorenz.atlas.util.Util.fqnToFlatFilename;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.module.Loader;

@CommandLine.Command(name = "lnf")
public class LNF implements Runnable {
  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "path-to-dir",
      description = "Path where output should be rooted at.")
  private Path path;

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
      program = loader.all();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    program.normalize();
    if (!program.infer()) {
      System.exit(1);
    }
    for (var fd : program.getFunctionDefinitions().values()) {
      try {
        fd.printTo(
            new PrintStream(
                new File(path.toFile(), fqnToFlatFilename(fd.getFullyQualifiedName()) + ".ml")
                    .getAbsoluteFile()));
      } catch (FileNotFoundException e) {
        e.printStackTrace();
      }
    }
    program.printAllSimpleSignaturesInOrder(System.out);
  }
}
