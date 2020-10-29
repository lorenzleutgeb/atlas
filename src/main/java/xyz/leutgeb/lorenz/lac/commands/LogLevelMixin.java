package xyz.leutgeb.lorenz.lac.commands;

import static picocli.CommandLine.Spec.Target.MIXEE;

import org.slf4j.impl.SimpleLogger;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.lac.Main;

public class LogLevelMixin {
  private @CommandLine.Spec(MIXEE) CommandLine.Model.CommandSpec
      mixee; // spec of the command where the @Mixin is used

  private enum LogLevel {
    trace,
    debug,
    info,
    warn,
    error,
    off
  }

  LogLevel logLevel = LogLevel.info;

  @CommandLine.Option(
      names = {"--log"},
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      defaultValue = "info",
      description = "Default log level")
  public void setLogLevel(LogLevel logLevel) {
    // Each subcommand that mixes in the LoggingMixin has its own instance
    // of this class, so there may be many LoggingMixin instances.
    // We want to store the verbosity value in a single, central place,
    // so we find the top-level command,
    // and store the verbosity level on our top-level command's LoggingMixin.
    ((Main) mixee.root().userObject()).logLevelMixin.logLevel = logLevel;

    System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, logLevel.toString());
  }
}
