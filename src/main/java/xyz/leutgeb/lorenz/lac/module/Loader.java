package xyz.leutgeb.lorenz.lac.module;

import com.google.common.base.Functions;
import lombok.Getter;
import lombok.Setter;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.CharStreams;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.ConnectivityInspector;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.alg.cycle.CycleDetector;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.EdgeReversedGraph;
import org.jgrapht.nio.ExportException;
import org.jgrapht.nio.dot.DOTExporter;
import org.jgrapht.traverse.BreadthFirstIterator;
import xyz.leutgeb.lorenz.lac.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.lac.ast.Program;
import xyz.leutgeb.lorenz.lac.util.DependencyEdge;
import xyz.leutgeb.lorenz.lac.util.Util;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Spliterators;
import java.util.Stack;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.Phaser;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.util.Collections.synchronizedMap;
import static java.util.Optional.ofNullable;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

@Value
@Slf4j
public class Loader {
  private static final String DOT_EXTENSION = ".ml";

  @Getter Map<String, FunctionDefinition> functionDefinitions = synchronizedMap(new HashMap<>());

  Set<String> loadedModules = Collections.synchronizedSet(new HashSet<>());

  @Setter private static Path defaultHome;

  Path home;

  private static final long BASE = System.nanoTime();

  Graph<String, DependencyEdge> g = new DefaultDirectedGraph<>(null, DependencyEdge::source, false);

  @Getter
  String id = Long.toHexString(System.currentTimeMillis() * 31 + (System.nanoTime() - BASE));

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

  public static Path getDefaultHome() {
    return ofNullable(defaultHome)
        .or(
            () ->
                ofNullable(System.getProperty(Loader.class.getName() + ".defaultHome"))
                    .map(Path::of))
        .orElse(Path.of("."));
  }

  public static Loader atDefaultHome() {
    return new Loader(getDefaultHome());
  }

  private static Path currentWorkingDirectory() {
    return Path.of(".").toAbsolutePath();
  }

  public static Path path(String moduleName, Path home, String extension) {
    // Actual path to module on disk.
    var modulePath = moduleName.split("\\.");
    modulePath[modulePath.length - 1] += extension;

    var path = home;
    for (String s : modulePath) {
      path = path.resolve(s);
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
                final String moduleName = moduleName(path);
                if (loadedModules.contains(moduleName)) {
                  return Stream.empty();
                }
                loadedModules.add(moduleName);
                return ModuleParser.parse(CharStreams.fromPath(path), moduleName).stream();
              } catch (IOException e) {
                e.printStackTrace();
                return Stream.empty();
              }
            })
        .forEach(
            fd -> {
              try {
                ingest(null, fd);
                // functionDefinitions.putIfAbsent(fd.getFullyQualifiedName(), fd);
                // stack.push(fd.getFullyQualifiedName());
              } catch (Throwable t) {

              }
            });
    load(stack);
  }

  public Program loadInline(String source) throws IOException {
    final var definitions = ModuleParser.parse(source, "_");

    for (var definition : definitions) {
      ingest(null, definition);
    }

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

  public Program load(final Set<String> rootFqns) throws IOException {
    final var stack =
        rootFqns.stream()
            .filter(not(functionDefinitions::containsKey))
            .collect(Collectors.toCollection(ConcurrentLinkedDeque::new));
    load(stack);

    final var dangling = rootFqns.stream().filter(not(g::containsVertex)).collect(toSet());
    if (!dangling.isEmpty()) {
      throw new RuntimeException("Could not load " + dangling);
    }

    final var reachable =
        new AsSubgraph<>(
            g,
            rootFqns.stream()
                .flatMap(
                    name ->
                        StreamSupport.stream(
                            Spliterators.spliteratorUnknownSize(
                                new BreadthFirstIterator<>(new EdgeReversedGraph<>(g), name), 0),
                            false))
                .collect(Collectors.toSet()));

    final Graph<Graph<String, DependencyEdge>, DependencyEdge> condensation =
        mapEdgesToSourceDependency(new KosarajuStrongConnectivityInspector<>(reachable).getCondensation());

    addSyntheticEdges(condensation);

    return new Program(
        reachable.vertexSet().stream()
            .collect(toMap(Functions.identity(), functionDefinitions::get)),
        Path.of("out", id),
        rootFqns,
        condensation);
  }

  private void addSyntheticEdges(Graph<Graph<String, DependencyEdge>, DependencyEdge> condensation) {
    final var before = new CycleDetector<>(condensation);
    if (before.detectCycles()) {
      throw bug("Cycles! " + before.findCycles());
    }

    final var connectivity = new ConnectivityInspector<>(condensation);

    final Set<Graph<String, DependencyEdge>> recursive = new HashSet<>();
    final Set<Graph<String, DependencyEdge>> nonRecursive = new HashSet<>();
    for (var g : condensation.vertexSet()) {
      (isRecursive(g) ? recursive : nonRecursive).add(g);
    }
    for (var nr : nonRecursive) {
      var fd = functionDefinitions.get(Util.pick(nr.vertexSet()));
      for (var r : recursive) {
        if (connectivity.pathExists(r, nr) || connectivity.pathExists(nr, r)) {
          continue;
        }
        if (affectedModules(r).contains(fd.getModuleName())) {
          log.info("adding artificial edge from " + r + " to " + nr);

          // NOTE: It's possible that adding this edge introduces a cycle, which would violate
          // assumptions down
          // the road. It might be possible to refresh the ConnectivityInspector (just inform it
          // about the newly
          // added edge), but I didn't get that to work right away.
          // There's a cycle check both before and after modifications, so overall such a bug should
          // be easy to
          // detect.

          condensation.addEdge(r, nr, DependencyEdge.synthetic());
          /*
          connectivity.edgeAdded(
              new GraphEdgeChangeEvent<>(
                  condensation, GraphEdgeChangeEvent.EDGE_ADDED, edge, r, nr));
           */
        }
      }
    }

    final var after = new CycleDetector<>(condensation);
    if (after.detectCycles()) {
      throw bug("Cycles! " + after.findCycles());
    }
  }

  private boolean isRecursive(Graph<String, DependencyEdge> g) {
    if (g.vertexSet().size() > 1) {
      return true;
    }
    if (g.vertexSet().size() == 1) {
      final var fd = Util.pick(g.vertexSet());
      return functionDefinitions.get(fd).getOcurringFunctions().contains(fd);
    }
    return false;
  }

  private Set<String> affectedModules(Graph<String, DependencyEdge> g) {
    return g.vertexSet().stream()
        .map(functionDefinitions::get)
        .map(FunctionDefinition::getModuleName)
        .collect(Collectors.toUnmodifiableSet());
  }

  private Path path(String moduleName) {
    return path(moduleName, home);
  }

  private void load(Iterable<String> roots) throws IOException {
    final Phaser phaser = new Phaser(1);

    // Possible optimization: Check how many different root modules
    // there are, only spawn new threads if there is more than one.

    for (final String fqn : roots) {
      final String moduleName = moduleName(fqn);
      if (loadedModules.contains(moduleName)) {
        continue;
      }
      loadedModules.add(moduleName);
      phaser.register();
      new Thread(
              () -> {
                try {
                  parse(phaser, fqn);
                } catch (IOException e) {
                  e.printStackTrace();
                } finally {
                  phaser.arriveAndDeregister();
                }
              })
          .start();
    }

    phaser.arriveAndAwaitAdvance();
  }

  private void parse(Phaser phaser, String fqn) throws IOException {
    // Logical module name.
    final var moduleName = moduleName(fqn);

    // Actual path to module on disk.
    final var path = path(moduleName);

    if (!Util.goodForReading(path)) {
      throw new RuntimeException("could not resolve path for function name '" + fqn + "'");
    }

    if (functionDefinitions.containsKey(fqn)) {
      ingest(phaser, functionDefinitions.get(fqn));
    } else {
      // Ingest all definitions that were parsed, no matter whether we actually "need"
      // them. This implementation might be a bit too eager, since it will load
      // all function definitions in a file even though they might not be
      // dependencies.
      // This could be improved.
      // Instead, match the definitions with the requested pattern, and a list of dependencies.
      var definitions = ModuleParser.parse(CharStreams.fromPath(path), moduleName);
      for (var definition : definitions) {
        ingest(phaser, definition);
      }
    }
  }

  private void ingest(Phaser phaser, FunctionDefinition definition) throws IOException {
    log.debug("Loaded {}", definition.getFullyQualifiedName());
    functionDefinitions.putIfAbsent(definition.getFullyQualifiedName(), definition);
    synchronized (g) {
      if (!g.containsVertex(definition.getFullyQualifiedName())) {
        g.addVertex(definition.getFullyQualifiedName());
      }
    }

    final Stack<String> todo = new Stack<>();
    for (var dependency : definition.getOcurringFunctions()) {
      if (dependency.equals(definition.getFullyQualifiedName())) {
        continue;
      }
      synchronized (g) {
        if (!g.containsVertex(dependency)) {
          g.addVertex(dependency);
          final String moduleName = moduleName(dependency);
          if (!loadedModules.contains(moduleName)) {
            loadedModules.add(moduleName);
            todo.push(dependency);
          }
        }
        g.addEdge(dependency, definition.getFullyQualifiedName());
      }
    }

    while (todo.size() > 1) {
      final String fqn = todo.pop();

      if (phaser != null) {
        phaser.register();
        log.debug("Spawning new thread to handle " + fqn);
        new Thread(
                () -> {
                  try {
                    parse(phaser, fqn);
                  } catch (IOException e) {
                    e.printStackTrace();
                  } finally {
                    phaser.arriveAndDeregister();
                  }
                })
            .start();
      } else {
        parse(null, fqn);
      }
    }

    if (!todo.isEmpty()) {
      parse(phaser, todo.pop());
    }
  }

  public void exportGraph(OutputStream stream) throws ExportException {
    final var exporter = new DOTExporter<String, DependencyEdge>(node -> "\"" + node + "\"");
    exporter.exportGraph(g, stream);
  }
  
  private static <V> Graph<V, DependencyEdge> mapEdgesToSourceDependency(Graph<V, DefaultEdge> g) {
    final var result = new DefaultDirectedGraph<V, DependencyEdge>(null, DependencyEdge::source, false);
    g.vertexSet().forEach(result::addVertex);
    g.edgeSet().forEach(e -> result.addEdge(g.getEdgeSource(e), g.getEdgeTarget(e)));
    return result;
  }
}
