package xyz.leutgeb.lorenz.atlas.commands;

import static picocli.CommandLine.Spec.Target.MIXEE;

import java.nio.file.Path;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.Main;
import xyz.leutgeb.lorenz.atlas.module.Loader;

public class SearchMixin {
  private @CommandLine.Spec(MIXEE) CommandLine.Model.CommandSpec
      mixee; // spec of the command where the @Mixin is used

  Path search;

  @CommandLine.Option(
      names = {"--search"},
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      description = "Where to search for *.ml files containing function definitions.",
      paramLabel = "path-to-dir")
  public void setSearch(Path search) {
    if (this.search == null) {
      return;
    }
    // Each subcommand that mixes in the LoggingMixin has its own instance
    // of this class, so there may be many LoggingMixin instances.
    // We want to store the verbosity value in a single, central place,
    // so we find the top-level command,
    // and store the verbosity level on our top-level command's LoggingMixin.
    ((Main) mixee.root().userObject()).searchMixin.search = this.search;
    Loader.setDefaultSearch(this.search);
  }
}
