package xyz.leutgeb.lorenz.lac;

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
    new CommandLine(new Main()).execute(args);
  }

  public void run() {}
}
