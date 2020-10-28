package xyz.leutgeb.lorenz.lac;

import java.nio.file.Path;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import xyz.leutgeb.lorenz.lac.commands.Haskell;
import xyz.leutgeb.lorenz.lac.commands.Java;
import xyz.leutgeb.lorenz.lac.commands.LNF;
import xyz.leutgeb.lorenz.lac.commands.REPL;
import xyz.leutgeb.lorenz.lac.commands.Run;
import xyz.leutgeb.lorenz.lac.util.Util;

@Command(
    mixinStandardHelpOptions = true,
    subcommands = {LNF.class, Haskell.class, Run.class, Java.class, REPL.class})
public class Main implements Runnable {
  public static void main(String[] args) {
    Util.readProperties(Path.of("lac.properties"));
    new CommandLine(new Main()).execute(args);
  }

  public void run() {}
}
