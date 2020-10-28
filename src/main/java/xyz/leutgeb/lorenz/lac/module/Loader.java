package xyz.leutgeb.lorenz.lac.module;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;
import static java.util.stream.Collectors.toUnmodifiableList;

import com.google.common.base.Functions;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Spliterators;
import java.util.Stack;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import lombok.Getter;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.CharStreams;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.EdgeReversedGraph;
import org.jgrapht.nio.ExportException;
import org.jgrapht.nio.dot.DOTExporter;
import org.jgrapht.traverse.BreadthFirstIterator;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.util.Util;

@Value
@Slf4j
public class Loader {
  private static final String DOT_EXTENSION = ".ml";

  @Getter private final Map<String, FunctionDefinition> functionDefinitions = new HashMap<>();

  Path home;

  private final Graph<String, DefaultEdge> g =
      new DefaultDirectedGraph<>(null, DefaultEdge::new, false);

  @Getter private final String id = String.format("%08x", System.currentTimeMillis() * 31);

  public Loader(Path home) {
    if (!Files.exists(home) || !Files.isDirectory(home) || !Files.isReadable(home)) {
      throw new IllegalArgumentException("home must be an existing readable directory");
    }
    if (!home.isAbsolute()) {
      home = home.toAbsolutePath();
    }
    this.home = home;
  }

  public static Loader atCurrentWorkingDirectory() {
    return new Loader(currentWorkingDirectory());
  }

  private static Path currentWorkingDirectory() {
    return Path.of(".").toAbsolutePath();
  }

  public static Path path(String moduleName, Path home, String extension) {
    // Actual path to module on disk.
    var modulePath = moduleName.split("\\.");
    modulePath[modulePath.length - 1] += extension;

    var path = home;
    for (int i = 0; i < modulePath.length; i++) {
      path = path.resolve(modulePath[i]);
    }
    return path;
  }

  private static Path path(String moduleName, Path home) {
    return path(moduleName, home, DOT_EXTENSION);
  }

  public static String moduleName(String fqn) {
    var lastDot = fqn.lastIndexOf(".");

    // Logical module name.
    return fqn.substring(0, lastDot);
  }

  private String moduleName(Path path) {
    if (!path.getFileName().toString().endsWith(DOT_EXTENSION)) {
      throw new IllegalArgumentException();
    }
    final var sb = new StringBuilder();
    final var relative = home.relativize(path);
    final var len = relative.getNameCount();
    for (int i = 0; i < len - 1; i++) {
      sb.append(relative.getName(i).getFileName());
    }
    final var last = relative.getName(len - 1).toString();
    sb.append(last, 0, last.length() - DOT_EXTENSION.length());
    return sb.toString();
  }

  public Program all() throws IOException {
    return load(functionDefinitions.keySet());
  }

  public void autoload() throws IOException {
    final var stack = new Stack<String>();
    Files.find(
            home,
            8,
            ((path, basicFileAttributes) ->
                path.getFileName().toString().endsWith(DOT_EXTENSION) && Util.goodForReading(path)))
        .flatMap(
            path -> {
              try {
                return ModuleParser.parse(CharStreams.fromPath(path), moduleName(path)).stream();
              } catch (IOException e) {
                e.printStackTrace();
                return Stream.empty();
              }
            })
        .forEach(
            fd -> {
              functionDefinitions.putIfAbsent(fd.getFullyQualifiedName(), fd);
              stack.push(fd.getFullyQualifiedName());
            });
    load(stack);
  }

  public Program loadInline(String source) throws IOException {
    final var definitions = ModuleParser.parse(source, "_");

    final var stack = new Stack<String>();

    for (var definition : definitions) {
      ingest(definition, stack);
    }

    load(stack);

    return load(
        definitions.stream()
            .map(FunctionDefinition::getFullyQualifiedName)
            .collect(Collectors.toSet()));
  }

  public Program load(String fqn) throws IOException {
    return load(Collections.singleton(fqn));
  }

  public Program loadMatching(Pattern pattern) throws IOException {
    return load(
        functionDefinitions.keySet().stream()
            .filter(pattern.asMatchPredicate())
            .collect(Collectors.toSet()));
  }

  public Program load(Set<String> fqns) throws IOException {
    final var stack =
        fqns.stream()
            .filter(Predicate.not(functionDefinitions::containsKey))
            .collect(Collectors.toCollection(Stack::new));
    load(stack);

    final var dangling = fqns.stream().filter(Predicate.not(g::containsVertex)).collect(toSet());

    if (!dangling.isEmpty()) {
      throw new RuntimeException("Could not load " + dangling);
    }

    final var plainSubgraph =
        new AsSubgraph<>(
            g,
            fqns.stream()
                .flatMap(
                    name ->
                        StreamSupport.stream(
                            Spliterators.spliteratorUnknownSize(
                                new BreadthFirstIterator<>(new EdgeReversedGraph<>(g), name), 0),
                            false))
                .collect(Collectors.toSet()));

    StrongConnectivityAlgorithm<String, DefaultEdge> scAlg =
        new KosarajuStrongConnectivityInspector<>(plainSubgraph);
    List<Graph<String, DefaultEdge>> stronglyConnectedSubgraphs =
        scAlg.getStronglyConnectedComponents();

    return new Program(
        plainSubgraph.vertexSet().stream()
            .collect(toMap(Functions.identity(), functionDefinitions::get)),
        stronglyConnectedSubgraphs.stream()
            .map(g -> g.vertexSet().stream().sorted().collect(toUnmodifiableList()))
            .collect(toList()),
        Path.of(".", "out", id));
  }

  private Path path(String moduleName) {
    return path(moduleName, home);
  }

  private void load(Stack<String> stack) throws IOException {
    while (!stack.isEmpty()) {
      final var fqn = stack.pop();

      // Logical module name.
      final var moduleName = moduleName(fqn);

      // Actual path to module on disk.
      final var path = path(moduleName);

      if (!Util.goodForReading(path)) {
        throw new RuntimeException("could not resolve path for function name '" + fqn + "'");
      }

      if (functionDefinitions.containsKey(fqn)) {
        ingest(functionDefinitions.get(fqn), stack);
      } else {
        // Ingest all definitions that were parsed, no matter whether we actually "need"
        // them. This implementation might be a bit too eager, since it will load
        // all function definitions in a file even though they might not be dependencies.
        // This could be improved.
        var definitions = ModuleParser.parse(CharStreams.fromPath(path), moduleName);
        for (var definition : definitions) {
          ingest(definition, stack);
        }
      }
    }
  }

  private void ingest(FunctionDefinition definition, Stack<String> stack) {
    log.debug("Loaded {}", definition.getFullyQualifiedName());
    functionDefinitions.putIfAbsent(definition.getFullyQualifiedName(), definition);
    if (!g.containsVertex(definition.getFullyQualifiedName())) {
      g.addVertex(definition.getFullyQualifiedName());
    }
    for (var dependency : definition.getOcurringFunctionsNonRecursive()) {
      if (!g.containsVertex(dependency)) {
        g.addVertex(dependency);
        stack.push(dependency);
      }
      g.addEdge(dependency, definition.getFullyQualifiedName());
    }
  }

  public void exportGraph(OutputStream stream) throws ExportException {
    final var exporter = new DOTExporter<String, DefaultEdge>(node -> "\"" + node + "\"");
    exporter.exportGraph(g, stream);
  }
}
