package xyz.leutgeb.lorenz.atlas.commands;

import com.microsoft.z3.Status;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import picocli.CommandLine;
import xyz.leutgeb.lorenz.atlas.ast.FunctionDefinition;
import xyz.leutgeb.lorenz.atlas.ast.Program;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.module.Loader;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.atlas.util.Util;

@CommandLine.Command(name = "run")
@Slf4j
public class Run implements Runnable {
  @CommandLine.Parameters(
      index = "0",
      arity = "1..*",
      paramLabel = "fqn",
      description = "Fully qualified names of functions to be checked.")
  private Set<String> fqns;

  @CommandLine.Option(
      defaultValue = "false",
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      names = "--infer",
      description =
          "When present cost annotations in the program source are ignored, instead a new typing is computed.")
  // TODO: Make infer the default.
  private Boolean infer;

  @CommandLine.Option(
      defaultValue = "false",
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      names = "--simple-annotations",
      paramLabel = "true|false",
      arity = "1",
      description =
          "If true, simple annotations (only single tree sizes as arguments to logarithm and constants) are used.")
  private Boolean simpleAnnotations;

  @CommandLine.Option(
      defaultValue = "false",
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      names = "--equal-ranks",
      paramLabel = "true|false",
      arity = "1",
      description =
          "If true, rank coefficients in annotations of arguments and return value is set equal per function definition.")
  private Boolean equalRanks;

  @CommandLine.Option(
      defaultValue = "true",
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      names = "--consistent-modules",
      arity = "1",
      paramLabel = "true|false",
      description = "If true, the annotation of return values is set equal per module.")
  private Boolean consistentModules;

  @CommandLine.Option(
      defaultValue = "true",
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      names = "--use-tick-defer",
      arity = "1",
      paramLabel = "true|false",
      description = "If true, use (tick:defer) instead of (tick).")
  private Boolean useTickDefer;

  @CommandLine.Option(names = "--name", description = "Name of the run.")
  private String name;

  @CommandLine.Option(
      names = "--tactics",
      paramLabel = "path-to-dir",
      description = "When present, tactics will be loaded from this directory.")
  private Path tactics;

  @CommandLine.Option(
      names = "--json",
      paramLabel = "path-to-file",
      description = "If present, detailed output in JSON format will be written to this file.",
      hidden = true)
  private Path json;

  @CommandLine.Option(
      names = "--produce-proof",
      defaultValue = "false",
      showDefaultValue = CommandLine.Help.Visibility.ALWAYS,
      arity = "1",
      paramLabel = "true|false",
      description = "If true, produce Graphviz output of the proof attempt(s) and proof(s).")
  private Boolean produceProofs;

  @CommandLine.Spec(CommandLine.Spec.Target.SELF)
  private CommandLine.Model.CommandSpec selfSpec;

  @Override
  public void run() {
    final var start = Instant.now();
    Loader loader = Loader.atDefaultHome();
    /*
    try {
      loader.autoload();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }*/
    Program program;
    try {
      program = loader.load(fqns);
      // program = loader.loadMatching(pattern);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    if (name != null && !name.isBlank()) {
      program.setName(name);
    }

    if (program.isEmpty()) {
      System.out.println("Program to analyze is empty, nothing to do!");
      System.exit(3);
    }

    program.normalize();

    if (!program.infer()) {
      return;
    }
    program.unshare(true);
    program.analyzeSizes();
    // log.info("Loaded definitions:");
    // program.printAllSimpleSignaturesInOrder(System.out);

    System.out.println("Output Directory: " + program.getBasePath().toAbsolutePath());
    System.out.println();

    Map<String, Path> tacticsMap = new HashMap<>();

    // log.info(infer ? "Given for comparison:" : "Will check following types:");

    log.info("Manual tactics disabled. All tactics will be automatically generated.");

    System.out.println("Function Definitions:");
    for (int i = 0; i < program.getOrder().size(); i++) {
      final var stratum = program.getOrder().get(i);
      for (var fqn : stratum) {
        FunctionDefinition fd = program.getFunctionDefinitions().get(fqn);

        if (tactics != null) {
          var path =
              tactics.resolve(fd.getModuleName().replace(".", "/") + "/" + fd.getName() + ".txt");
          if (Files.exists(path) && Files.isReadable(path)) {
            tacticsMap.put(fd.getFullyQualifiedName(), path);
          }
        }

        System.out.println(fd.getAnnotatedSignatureString());

        // log.info("\tDependencies: " + fd.getOcurringFunctionsNonRecursive());
        System.out.println("\tSource:       " + fd.getBody().getSource().getRoot());
        System.out.println(
            "\tBound:        "
                + fd.getAnnotatedSignature()
                    .getAnnotation()
                    .map(
                        a ->
                            a.getBounds(
                                fd.treeLikeArguments().stream()
                                    .map(IdentifierExpression::getName)
                                    .toList()))
                    .orElse("?"));

        if (tactics != null) {
          final var p = tacticsMap.get(fd.getFullyQualifiedName());
          if (p == null) {
            System.out.println("\tTactic:       n/a (will be automatically generated)");
          } else {
            System.out.println("\tTactic:       " + p.toAbsolutePath());
          }
        }
      }
    }
    System.out.println();

    System.setProperty(Util.getPropertyName(Prover.class, "tickDefer"), useTickDefer.toString());
    System.setProperty(
        Util.getPropertyName(Prover.class, "produceProofs"), produceProofs.toString());

    final var result =
        program.solve(
            new HashMap<>(),
            tacticsMap,
            infer,
            consistentModules,
            equalRanks,
            simpleAnnotations,
            !infer,
            Collections.emptySet());

    final var stop = Instant.now();
    System.out.println("Elapsed Walltime: " + Duration.between(start, stop));
    System.out.println();

    if (Status.UNSATISFIABLE.equals(result.getStatus())) {
      System.out.println("UNSAT");
      System.exit(1);
    } else if (Status.UNKNOWN.equals(result.getStatus())) {
      System.out.println("UNKNOWN");
      System.exit(2);
    } else if (Status.SATISFIABLE.equals(result.getStatus())) {
      if (!infer) {
        System.out.println("SAT");
        System.out.println();
      } else {
        System.out.println("Results:");
        program.printAllInferredAnnotationsAndBoundsInOrder(System.out);
        System.out.println();
      }
    } else {
      throw new RuntimeException("encountered unknown status");
    }

    /*
    if (json != null) {
      JsonObjectBuilder builder = Json.createObjectBuilder();

      builder.add("result", program.inferredSignaturesToJson());
      builder.add("duration", Json.createValue(Duration.between(start, stop).toString()));

      JsonObjectBuilder z3ObjectBuilder = Json.createObjectBuilder();
      z3ObjectBuilder.add("status", Json.createValue(result.getStatus().toString()));

      JsonObjectBuilder z3StatisticsBuilder = Json.createObjectBuilder();
      result.getStatistics().forEach(z3StatisticsBuilder::add);
      z3ObjectBuilder.add("statistics", z3StatisticsBuilder.build());

      if (result.getSmtFile().isPresent()) {
        z3ObjectBuilder.add("file", Json.createValue(result.getSmtFile().get().toString()));
      }

      builder.add("z3", z3ObjectBuilder.build());

      log.info("Writing JSON output to {}", json);
      try (final var out = output(json)) {
        Json.createWriter(out).writeObject(builder.build());
      } catch (IOException ioException) {
        log.error("Failed to write JSON output.", ioException);
      }
    }
     */

    // System.exit(result.toExitCode());
    System.exit(0);
  }
}
