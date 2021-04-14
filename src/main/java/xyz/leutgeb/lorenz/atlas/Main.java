package xyz.leutgeb.lorenz.atlas;

import java.nio.file.Path;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.commands.Haskell;
import xyz.leutgeb.lorenz.atlas.commands.HomeMixin;
import xyz.leutgeb.lorenz.atlas.commands.Index;
import xyz.leutgeb.lorenz.atlas.commands.Java;
import xyz.leutgeb.lorenz.atlas.commands.LNF;
import xyz.leutgeb.lorenz.atlas.commands.LogLevelMixin;
import xyz.leutgeb.lorenz.atlas.commands.REPL;
import xyz.leutgeb.lorenz.atlas.commands.Run;
import xyz.leutgeb.lorenz.atlas.util.Properties;

@CommandLine.Command(
    mixinStandardHelpOptions = true,
    subcommands = {LNF.class, Haskell.class, Run.class, Java.class, REPL.class, Index.class})
public class Main implements Runnable {
  @CommandLine.Mixin public LogLevelMixin logLevelMixin;

  @CommandLine.Mixin public HomeMixin homeMixin;

  public static void main(String[] args) {
    Properties.readProperties(Path.of("atlas.properties"));
    new CommandLine(new Main()).execute(args);
  }

  public void run() {}
}
