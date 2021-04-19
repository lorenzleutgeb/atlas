package xyz.leutgeb.lorenz.atlas.util;

import static java.lang.System.getProperties;

import com.microsoft.z3.Global;
import com.microsoft.z3.Log;
import com.microsoft.z3.Version;
import java.nio.file.Path;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Z3Support {
  private static boolean loaded = false;
  private static Path logPath = null;
  private static final String PROPERTY_PREFIX = "com.microsoft.z3.";

  public static void load() {
    load(null);
  }

  public static synchronized void load(Path logPath) {
    if (loaded) {
      return;
    }

    // We instruct Z3 not to load itself.
    // This property is checked in the static block of com.microsoft.z3.Native.
    System.setProperty("z3.skipLibraryLoad", "true");
    // Load Z3.
    Util.loadLibrary("z3");
    Util.loadLibrary("z3java");
    log.info("{}", Version.getString());

    try {
      for (var property : getProperties().entrySet()) {
        if (!(property.getKey() instanceof String && property.getValue() instanceof String)) {
          continue;
        }

        final var key = (String) property.getKey();

        if (!key.startsWith(PROPERTY_PREFIX)) {
          continue;
        }

        final var value = (String) property.getValue();

        final String parameter = key.substring(PROPERTY_PREFIX.length());
        log.trace("Setting Z3 parameter '{}' to '{}'", key, value);

        Global.setParameter(parameter, value);
        // Global.setParameter("parallel.enable", "true");
      }
    } catch (Throwable throwable) {
      log.debug("Error setting Z3 parameters.", throwable);
    }

    if (logPath != null) {
      Log.open(logPath.toAbsolutePath().toString());
      Z3Support.logPath = logPath;
    }

    Global.ToggleWarningMessages(true);

    loaded = true;
  }

  public static synchronized void unload() {
    if (!loaded) {
      return;
    }

    if (logPath != null && Log.isOpen()) {
      Log.close();
    }
  }
}
