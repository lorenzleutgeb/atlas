package xyz.leutgeb.lorenz.lac.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.slf4j.LoggerFactory;

/**
 * @implNote This class should not use a logger before setting properties. Otherwise the logging
 *     implementation will read properties before they are properly set.
 */
public class Properties {
  public static void readProperties(Path propertiesPath) {
    try (final var reader = Files.newBufferedReader(propertiesPath)) {
      final java.util.Properties properties = new java.util.Properties();
      properties.load(reader);
      for (final var property : properties.entrySet()) {
        if (!(property.getKey() instanceof String && property.getValue() instanceof String)) {
          continue;
        }
        System.setProperty((String) property.getKey(), (String) property.getValue());
      }
    } catch (IOException e) {
      LoggerFactory.getLogger(Properties.class).warn("Could not read properties.", e);
    }
  }
}
