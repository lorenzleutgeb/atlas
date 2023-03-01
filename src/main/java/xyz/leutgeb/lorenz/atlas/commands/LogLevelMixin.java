package xyz.leutgeb.lorenz.atlas.commands;

import org.slf4j.impl.SimpleLogger;
import picocli.CommandLine;

public class LogLevelMixin {
  private enum LogLevel {
    trace,
    debug,
    info,
    warn,
    error,
    off
  }

  @CommandLine.Option(
      names = {"--log"},
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      defaultValue = "info",
      description = "Default log level")
  public void setLogLevel(LogLevel logLevel) {
    System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, logLevel.toString());
  }
}
