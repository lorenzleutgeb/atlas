package xyz.leutgeb.lorenz.lac;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Properties;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import xyz.leutgeb.lorenz.lac.commands.Haskell;
import xyz.leutgeb.lorenz.lac.commands.LNF;
import xyz.leutgeb.lorenz.lac.commands.Run;

@Command(
    mixinStandardHelpOptions = true,
    subcommands = {LNF.class, Haskell.class, Run.class})
public class Main implements Runnable {
  public static void main(String[] args) {
    readProperties();
    new CommandLine(new Main()).execute(args);
  }

  private static void readProperties() {
    final var propertiesFile = Paths.get("lac.properties");
    if (Files.exists(propertiesFile) && Files.isReadable(propertiesFile)) {
      try (final var reader = Files.newBufferedReader(propertiesFile)) {
        final Properties properties = new Properties();
        properties.load(reader);
        for (final var property : properties.entrySet()) {
          if (!(property.getKey() instanceof String && property.getValue() instanceof String)) {
            continue;
          }
          System.setProperty((String) property.getKey(), (String) property.getValue());
        }
      } catch (IOException ioException) {
        ioException.printStackTrace();
      }
    }
  }

  public void run() {}
}
