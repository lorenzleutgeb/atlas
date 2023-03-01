package xyz.leutgeb.lorenz.atlas.commands;

import java.nio.file.Path;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.module.Loader;

public class SearchMixin {
  @CommandLine.Option(
      names = {"--search"},
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      description = "Where to search for *.ml files containing function definitions.",
      paramLabel = "path-to-dir")
  public void setSearch(Path search) {
    Loader.setDefaultSearch(search);
  }
}
