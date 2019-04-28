package xyz.leutgeb.lorenz.logs;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import xyz.leutgeb.lorenz.logs.commands.LNF;

@Command(
  mixinStandardHelpOptions = true,
  subcommands = {LNF.class}
)
public class Main implements Runnable {

  public void run() {}

  public static void main(String[] args) {
    CommandLine.run(new Main(), args);
  }
}
