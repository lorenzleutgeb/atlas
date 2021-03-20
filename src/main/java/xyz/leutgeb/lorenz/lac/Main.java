package xyz.leutgeb.lorenz.lac;

import java.nio.file.Path;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.lac.commands.Haskell;
import xyz.leutgeb.lorenz.lac.commands.HomeMixin;
import xyz.leutgeb.lorenz.lac.commands.Index;
import xyz.leutgeb.lorenz.lac.commands.Java;
import xyz.leutgeb.lorenz.lac.commands.LNF;
import xyz.leutgeb.lorenz.lac.commands.LogLevelMixin;
import xyz.leutgeb.lorenz.lac.commands.REPL;
import xyz.leutgeb.lorenz.lac.commands.Run;
import xyz.leutgeb.lorenz.lac.util.Properties;

@CommandLine.Command(
    mixinStandardHelpOptions = true,
    subcommands = {LNF.class, Haskell.class, Run.class, Java.class, REPL.class, Index.class})
public class Main implements Runnable {
  @CommandLine.Mixin public LogLevelMixin logLevelMixin;

  @CommandLine.Mixin public HomeMixin homeMixin;

  public static void main(String[] args) {
    Properties.readProperties(Path.of("lac.properties"));
    new CommandLine(new Main()).execute(args);
  }

  public void run() {}
}
