# Artifact

This artifact is implemented in Java 14. It
depends on multiple libraries packaged as JAR
files and Z3 for SMT solving provided as a shared
object which is dynamically linked at runtime.

## Contents of `artifact`

### `lib`

Contains dependency JARs.

## `java`

Contains OpenJDK 14.

## `z3`

Contains Z3 built as a shared library
(`libz3java.so`) for interoperability with Java.
