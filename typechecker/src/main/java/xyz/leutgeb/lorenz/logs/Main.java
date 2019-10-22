package xyz.leutgeb.lorenz.logs;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import xyz.leutgeb.lorenz.logs.commands.Haskell;
import xyz.leutgeb.lorenz.logs.commands.LNF;

@Command(
    mixinStandardHelpOptions = true,
    subcommands = {LNF.class, Haskell.class})
public class Main implements Runnable {

  public static void main(String[] args) {
    new CommandLine(new Main()).execute(args);
  }

  public void run() {}
}
