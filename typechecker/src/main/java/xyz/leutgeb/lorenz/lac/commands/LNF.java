package xyz.leutgeb.lorenz.lac.commands;

import picocli.CommandLine;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.module.Loader;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;

import static xyz.leutgeb.lorenz.lac.util.Util.fqnToFlatFilename;

@CommandLine.Command(name = "lnf")
public class LNF implements Runnable {
  @CommandLine.Option(defaultValue = ".", names = "--home", description = "Where to search for *.ml files containing function definitions.")
  private Path home;

  @CommandLine.Option(
      defaultValue = "out",
      names = "--out",
      description =
          "Where to write loaded and normalized output to. Must be an existing, writable directory.")
  private Path out;

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
      program = loader.all();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    for (var fd : program.getFunctionDefinitions().values()) {
      try {
        fd.printTo(
            new PrintStream(
                new File(out.toFile(), fqnToFlatFilename(fd.getFullyQualifiedName()) + "-loaded.ml")
                    .getAbsoluteFile()));
      } catch (FileNotFoundException e) {
        e.printStackTrace();
      }
    }
    program.normalize();
    for (var fd : program.getFunctionDefinitions().values()) {
      try {
        fd.printTo(
            new PrintStream(
                new File(
                        out.toFile(),
                        fqnToFlatFilename(fd.getFullyQualifiedName()) + "-normalized.ml")
                    .getAbsoluteFile()));
      } catch (FileNotFoundException e) {
        e.printStackTrace();
      }
    }
    try {
      program.infer();
    } catch (UnificationError | TypeError unificationError) {
      unificationError.printStackTrace();
      System.exit(1);
    }
    program.printAllSimpleSignaturesInOrder(System.out);
  }
}
