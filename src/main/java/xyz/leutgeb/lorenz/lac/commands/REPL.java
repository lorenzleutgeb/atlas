package xyz.leutgeb.lorenz.lac.commands;

import static picocli.CommandLine.Help.Visibility.ALWAYS;
import static xyz.leutgeb.lorenz.lac.commands.REPL.NAME;
import static xyz.leutgeb.lorenz.lac.util.Util.append;

import io.github.classgraph.ClassGraph;
import java.util.Collections;
import java.util.List;
import jdk.jshell.tool.JavaShellToolBuilder;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.lac.module.Loader;

/** See also {@link REPLSubstitute}. */
@CommandLine.Command(
    name = NAME,
    showEndOfOptionsDelimiterInUsageHelp = true,
    description = {
      "A Wrapper around JShell preloaded with utilities.",
      "The underlying JShell instance can be freely configured by specifying parameters.",
      "To get a list of parameters available, pass `--help` as a positional parameter, not an option, i.e. use `... -- --help`.",
      "See http://openjdk.java.net/jeps/222"
    })
public class REPL implements Runnable {
  public static final String NAME = "repl";

  @CommandLine.Parameters(description = "Parameters for JShell.", showDefaultValue = ALWAYS)
  private List<String> args =
      List.of(
          "--feedback",
          "verbose",
          "--class-path",
          new ClassGraph().getClasspath(),
          "--startup",
          "DEFAULT",
          "--startup",
          "PRINTING",
          "--startup",
          "lac.jsh");

  @Override
  public void run() {
    final JavaShellToolBuilder builder = JavaShellToolBuilder.builder();

    final var args =
        append(
            this.args,
            Collections.singletonList(
                "-R-D" + Loader.class.getName() + ".defaultHome=" + Loader.getDefaultHome()));

    System.out.println(args);
    try {
      System.exit(builder.start(args.toArray(new String[args.size()])));
    } catch (Exception exception) {
      exception.printStackTrace();
      System.exit(1);
    }
  }
}
