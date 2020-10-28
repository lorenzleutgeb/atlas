package xyz.leutgeb.lorenz.lac.commands;

import jdk.jshell.tool.JavaShellToolBuilder;
import picocli.CommandLine;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

@CommandLine.Command(name = "repl")
public class REPL implements Runnable {
  @Override
  public void run() {
    ExecutorService executorService = Executors.newFixedThreadPool(2);

    final PipedOutputStream output = new PipedOutputStream();
    final BufferedOutputStream bufferedOutput = new BufferedOutputStream(output);

    executorService.submit(
        () -> {
          try (final var scanner = new Scanner(new BufferedInputStream(System.in))) {
            System.out.println("Reader is starting.");
            while (scanner.hasNextLine()) {
              final String input = scanner.nextLine() + "\n";

              if (input.trim().equals("stop")) {
                break;
              }

              System.out.println("Read line: " + input);
              bufferedOutput.write(input.getBytes(StandardCharsets.UTF_8));
              bufferedOutput.flush();
            }

            bufferedOutput.close();
          } catch (IOException ioException) {
            ioException.printStackTrace();
          }
        });

    executorService.submit(
        () -> {
          System.out.println("tool thread started");
          try (final PipedInputStream input = new PipedInputStream(output)) {
            final JavaShellToolBuilder builder = JavaShellToolBuilder.builder().in(input, null).promptCapture(false);
            System.out.println("Tool will run now!");
            builder.run();
          } catch (Exception exception) {
            exception.printStackTrace();
          }
        });

    System.out.println("main thread will await termination.");
    try {
      executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.HOURS);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
}
