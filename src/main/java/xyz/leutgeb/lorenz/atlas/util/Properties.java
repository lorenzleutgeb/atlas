package xyz.leutgeb.lorenz.atlas.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @implNote This class should not use a logger before setting properties. Otherwise the logging
 *     implementation will read properties before they are properly set.
 */
public class Properties {
  private static void readPropertiesInternal(Path propertiesPath) throws IOException {
    try (final var reader = Files.newBufferedReader(propertiesPath)) {
      final java.util.Properties properties = new java.util.Properties();
      properties.load(reader);
      for (final var property : properties.entrySet()) {
        if (!(property.getKey() instanceof String && property.getValue() instanceof String)) {
          continue;
        }
        System.setProperty((String) property.getKey(), (String) property.getValue());
      }
    }
  }

  public static void readProperties(Path... paths) {
    List<Path> good = new ArrayList<>();
    List<IOException> bad = new ArrayList<>();
    for (Path path : paths) {
      try {
        readPropertiesInternal(path);
        good.add(path);
      } catch (IOException exception) {
        bad.add(exception);
      }
    }
    final Logger logger = LoggerFactory.getLogger(Properties.class);
    for (Path path : good) {
      logger.info("Read properties from '{}'.", path);
    }
    for (IOException exception : bad) {
      logger.debug("Could not read some property file.", exception);
    }
  }
}
