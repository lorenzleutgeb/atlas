package xyz.leutgeb.lorenz.logs.commands;

import static java.util.Collections.enumeration;
import static java.util.stream.Collectors.toUnmodifiableList;
import static xyz.leutgeb.lorenz.logs.ProgramParser.parse;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.SequenceInputStream;
import java.util.stream.Stream;
import org.antlr.v4.runtime.CharStreams;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.logs.ast.Program;

@CommandLine.Command(name = "lnf")
public class LNF implements Runnable {
  @CommandLine.Parameters(arity = "1..*", paramLabel = "FILE", description = "Files to process")
  private File[] inputFiles;

  @Override
  public void run() {
    Program program = null;
    try {
      program =
          parse(
              CharStreams.fromReader(
                  new InputStreamReader(
                      new SequenceInputStream(
                          enumeration(
                              Stream.of(inputFiles)
                                  .map(
                                      file -> {
                                        try {
                                          return new FileInputStream(file);
                                        } catch (FileNotFoundException e) {
                                          throw new RuntimeException(e);
                                        }
                                      })
                                  .collect(toUnmodifiableList()))))));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    program.normalize();
    program.printTo(System.out);
  }
}
