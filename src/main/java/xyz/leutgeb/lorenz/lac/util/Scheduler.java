package xyz.leutgeb.lorenz.lac.util;

import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import com.google.common.util.concurrent.ListeningExecutorService;
import com.google.common.util.concurrent.MoreExecutors;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.jgrapht.Graph;
import org.jgrapht.GraphTests;
import org.jgrapht.Graphs;
import org.jgrapht.util.ModifiableInteger;

@Slf4j
public class Scheduler<T, V, E> {
  private static final String GRAPH_IS_NOT_A_DAG = "Graph is not a DAG";

  private final Graph<V, E> graph;
  private final Set<V> initial;

  private final Map<V, ModifiableInteger> inDegreeMap;
  private final Function<V, ? extends Callable<T>> f;

  private final Map<V, Result<T>> results = Collections.synchronizedMap(new HashMap<>());

  private int remainingVertices;
  private ExecutorService executorService = Executors.newFixedThreadPool(8);
  private ListeningExecutorService listeningExecutorService =
      MoreExecutors.listeningDecorator(executorService);

  @AllArgsConstructor
  public static class Result<T> {

    @Getter private final ExecutionException executionException;

    @Getter private final CancellationException cancellationException;

    @Getter private final T value;

    public static <T> Result<T> fromFuture(Future<T> future) throws InterruptedException {
      try {
        return new Result<>(null, null, future.get());
      } catch (InterruptedException e) {
        throw e;
      } catch (ExecutionException e) {
        return new Result<>(e, null, null);
      } catch (CancellationException e) {
        return new Result<>(null, e, null);
      }
    }

    public static <T> Result<T> merge(Result<T> a, Result<T> b, BinaryOperator<T> merge) {
      if (a.executionException != null) {
        return a;
      }
      if (a.cancellationException != null) {
        return a;
      }
      if (b.executionException != null) {
        return b;
      }
      if (b.cancellationException != null) {
        return b;
      }

      return new Result<>(null, null, merge.apply(a.value, b.value));
    }
  }

  public Scheduler(Graph<V, E> graph, Function<V, ? extends Callable<T>> f) {
    this.f = f;
    GraphTests.requireDirected(graph);
    this.graph = graph;
    this.inDegreeMap = new HashMap<>();
    this.initial = new HashSet<>();
    for (V v : graph.vertexSet()) {
      int d = 0;
      for (E e : graph.incomingEdgesOf(v)) {
        V u = Graphs.getOppositeVertex(graph, e, v);
        if (v.equals(u)) {
          throw new IllegalArgumentException(GRAPH_IS_NOT_A_DAG);
        }
        d++;
      }
      inDegreeMap.put(v, new ModifiableInteger(d));
      if (d == 0) {
        initial.add(v);
      }
    }
  }

  private synchronized void schedule(V v) {
    log.trace("Scheduled {}", v);
    final var listenableFuture =
        /*
        listeningExecutorService.submit(
            () -> {
              try {
                f.apply(v);
              } catch (Throwable t) {
                //log.error("Error in task", t);
              }
            });
             */

        listeningExecutorService.submit(f.apply(v));

    listenableFuture.addListener(
        () -> {
          log.trace("Finished {}", v);

          try {
            results.put(v, Result.fromFuture(listenableFuture));
          } catch (InterruptedException e) {
            throw new RuntimeException(e);
          }

          for (E e : graph.outgoingEdgesOf(v)) {
            V u = Graphs.getOppositeVertex(graph, e, v);

            synchronized (inDegreeMap) {
              ModifiableInteger inDegree = inDegreeMap.get(u);
              if (inDegree.value > 0) {
                inDegree.value--;

                if (inDegree.value == 0) {
                  schedule(u);
                }
              }
            }
          }

          --remainingVertices;

          if (remainingVertices == 0) {
            executorService.shutdown();
          }
        },
        executorService);
  }

  public Map<V, Result<T>> run(int nThreads, long timeout, TimeUnit unit)
      throws InterruptedException {
    executorService = Executors.newFixedThreadPool(nThreads);
    listeningExecutorService = MoreExecutors.listeningDecorator(executorService);
    remainingVertices = graph.vertexSet().size();

    for (V v : initial) {
      schedule(v);
    }

    if (!executorService.awaitTermination(timeout, unit)) {
      throw bug("Execution took too long.");
    }
    return results;
  }
}
