rootProject.name = "atlas"

/**
 * NOTE(lorenzleutgeb): I keep my $XDG_CACHE_HOME in main memory to speed up
 * building. Gradle by default does not use $XDG_CACHE_HOME, but instead puts
 * the local build cache into the Gradle user home directory. The configuration
 * below will use a project-specific subdirectory of $XDG_CACHE_HOME, if it is
 * defined.
 *
 * See https://docs.gradle.org/current/userguide/build_cache.html#sec:build_cache_configure_local
 */
buildCache {
    local {
        val base = System.getenv().get("XDG_CACHE_HOME")
        if (base != null) {
            directory = "${base}/gradle/${rootProject.name}/build-cache"
        }
    }
}
