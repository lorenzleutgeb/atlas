package xyz.leutgeb.lorenz.lac.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;

import java.nio.file.Path;
import java.util.regex.Pattern;
import picocli.CommandLine;

class Check {
  @CommandLine.Parameters(
      index = "0",
      arity = "1",
      paramLabel = "pattern",
      description =
          "Regular expression to select fully qualified names of functions to be checked. For example, to select all functions whose names begin with \"a\" and contain \"b\" inside a module that has a name ending in \"c\", use \".*c\\.a.*b.*\" (think carefully about escaping \"\\\").",
      defaultValue = ".*",
      showDefaultValue = ALWAYS)
  private Pattern pattern;

  @CommandLine.Option(defaultValue = ".", names = "--home")
  private Path home;
}
