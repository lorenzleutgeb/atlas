# atlas

[![DOI](https://zenodo.org/badge/156873559.svg)](https://zenodo.org/badge/latestdoi/156873559)
[![codecov](https://codecov.io/gh/lorenzleutgeb/atlas/branch/main/graph/badge.svg?token=cXfOoGOXV2)](https://codecov.io/gh/lorenzleutgeb/atlas)
[![codebeat badge](https://codebeat.co/badges/64e5fc79-f2b7-4a49-ac3e-bf19395d1b07)](https://codebeat.co/projects/github-com-lorenzleutgeb-atlas-main)

A static analysis tool for Automated Amortised Complexity Analysis.

## Highlighted Files

### `src/test/resources/examples`

Contains source code of the function definitions to be analyzed
(`*.ml` files in subdirectories). This is a Git submodule. 

### `src/test/resources/tactics`

Contains tactics for guided proof.

### `src/antlr/**/*.g4`

Contains grammars of the input languages for reference.

## Using

## `atlas`

Start by executing

    atlas --help

This will show a list of (global) options and commands. Take note of the option
`--home`. It defines where `atlas` will look for input files. For example

    atlas --home=src/test/resources/examples [COMMAND]

The log level can be adjusted with `--log`, which takes (one of) the following
values: trace, debug, info, warn, error, off. The default log level and the
default log level file name are defined in `atlas.properties`.

## `atlas run`

The most important command is "run". To show help, execute

    atlas run --help

The `atlas run` command, can be used to check types and their annotations
present in the source code, or, if enabled via `--infer`, disregard type
annotations in the source and infer types.
To speed up solving, tactics can be passed via `--tactics`.

### Java Properties

Behaviour of the tool can be controlled by setting Java properties.

Z3 specific configuration can be set via `com.microsoft.z3.*`,
for example `com.microsoft.z3.memory_max_size=25769803776` or
`com.microsoft.z3.unsat_core=true` (see also `atlas.properties`).
Logging-specific configuration can be set [via `org.slf4j.simpleLogger.*`][simplelogger].
Tool-specific configuration can be set via `xyz.leutgeb.lorenz.atlas.*`.

### `atlas.properties`

Configuration file that can be used to change Z3 parameters and
logging settings. By setting the log level to "debug" or "trace",
much more detailled output can be obtained.

Some of the above properties are exposed as command-line arugments.

When inferring, set minimization target (`--minimize={roots,all}`).
Fix right sides (`--mode={amortized,worstcase,free}`).

## Building

`atlas` is built using [Gradle][gradle]. Pre- and postprocessing is implemented in
Java, and the [Z3 Theorem Prover][z3] is used for solving.

`atlas` can be built for multiple target environments:

 - JAR file. Requires a JVM and Z3 shared libraries on the target system.
 - ELF x86_64 binary (from the JAR via [GraalVM Native Image][graalvm-native-image]).
   Requires a loader, some (very common) shared libraries, and Z3 shared libraries.
 - Docker Image (based on ELF x86_64 binary).
 - Open Virtual Appliance (OVA; based on ELF x86_64 binary, and [NixOS][nixos]).

x86_64 is the only architecture supported. Linux is the only operating system supported.

The build process is fully automated with [Nix][nix] as a [flake][nix-flakes];
Nix will run Gradle and build Z3 on demand. See also ["How Nix works"][nix-how].

However, Nix is not strictly necessary: Without Nix, Gradle can still directly
handle download and installation of all dependencies except the shared libraries
for the [Z3 Theorem Prover][z3].
Thus, the only manual steps required are building Z3 and making sure that the
resulting shared libraries can be loaded.

The following two sections describe how to build `atlas` (1.) with Nix, and
(2.) without Nix.

### With Nix

This section assumes that [Nix][nix] is [installed][nix-install] and functional,
and that the [experimental feature "Flakes" is enabled][nix-flakes-enable].

To get an overview of the derivations/packaes that are available run following
command. Their names roughly match the "target environments" listed in the
previous section.

```
$ nix flake show
```

For additional information on derivations/packages, please
read metadata and comments in `flake.nix`.

Then, to build, for example:

```
# Build the default package (atlas x86_64 ELF binary, and related files)
$ nix build .#defaultPackage.x86_64-linux

# Build an OVA
$ nix build --print-build-logs .#packages.x86_64-linux.atlas-ova
```

After completion, the result of the build will be available as `./result`.
Note that `./result` may be a directory, or a file of arbitrary type, depending
on which environment was targeted. This is a Nix default.

### Without Nix

### Preparing the Build

#### GraalVM

[Install GraalVM][graalvm-install]. In your build environment `java -version`
should mention "GraalVM", and the environment variable `JAVA_HOME` must point at
your GraalVM installation.

#### Compatible GraalVM Version

For development GraalVM CE 21.0.0 (build 11.0.10+8-jvmci-21.0-b06) was used.
Any GraalVM version with a major version of 21 and a Java major version of 11
is expected to work.

#### Gradle

[Install Gradle 7][gradle-install].

#### Git Submodules

Make sure that the Git submodule at
`src/test/resources/examples` is initialized and updated
(see [The Git Book "Git Tools - Submodules"][git-submodules]).

#### Setting up the Z3 Theorem Prover

For interfacing with Z3, three files are required: The JAR file
`com.microsoft.z3.jar` and `libz3java.so` work to bridge between the
Java Virtual Machine and Z3's C API, and `libz3.so` is the actual solver
implementation.

It is not straightforward to integrate all three files in the Gradle build
definition. Only `com.microsoft.z3.jar` is automatically downloaded and handled
by the Gradle build.

The two shared object files need to be obtained separately manually. There are
at least two options to achieve this, summarized in the following
two sections.

##### Locally building Z3 shared objects

Refer to [the Z3 `README.md` and its section on Java][z3-readme-java]
as well as [the Z3 example using Java][z3-example-java-readme].
Follow instructions to build `libz3java.so` and `libz3.so`.

##### Installing Z3 shared objects from a package repostiory

Note that some Linux distributions allow for convenient installation of the
required libraries. For example, Ubuntu package [`libz3-jni`][apt-libz3-jni]
contains `libz3java.so` and in turn depends on package [`libz3-4`][apt-libz3],
which contains `libz3.so`.

##### Shared Library Location

The Java Virtual Machine or the ELF loader must find above `*.so` files for
linking. If there are linking issues, copy `libz3.so` and `libz3java.so` to
a path listed in `$LD_LIBRARY_PATH` or `./lib`.

##### Compatible Z3 Version

`atlas` is developed and tested to interface with Z3 v4.8.10, which is the most
recent release as of 2021-04-24.

### Building a JAR File

This is done via Gradle. Execute

```console
$ gradle build
```

This will result in a new executable at `./build/native-image/atlas*`.

## Related Repositories

 - [`lorenzleutgeb/atlas-examples`](https://github.com/lorenzleutgeb/atlas-examples)
 - [`lorenzleutgeb/atlas-thesis`](https://github.com/lorenzleutgeb/atlas-thesis)
 - [`lorenzleutgeb/atlas-hs`](https://github.com/lorenzleutgeb/atlas-hs)

## Reading

 - [arXiv:1807.08242][arxiv-1]
 - [arXiv:2101.12029][arxiv-2]
 - [Mechanically Proving Termination Using Polynomial Interpretations](https://doi.org/10.1007/s10817-005-9022-x)
 - ["Carbon Credits" for Resource-Bounded Computations Using Amortised Analysis](https://doi.org/10.1007/978-3-642-05089-3_23)
 - ...

[apt-libz3]: https://packages.ubuntu.com/hirsute/libz3-4
[apt-libz3-jni]: https://packages.ubuntu.com/hirsute/libz3-jni
[arxiv-1]: https://arxiv.org/abs/1807.08242
[arxiv-2]: https://arxiv.org/abs/2101.12029
[git-submodules]: https://git-scm.com/book/en/v2/Git-Tools-Submodules
[graalvm-install]: https://www.graalvm.org/docs/getting-started/
[graalvm-native-image]: https://www.graalvm.org/reference-manual/native-image/
[gradle]: https://gradle.org/
[gradle-install]: https://gradle.org/install/
[nixos]: https://nixos.org/
[nix]: https://nixos.org/nix
[nix-flakes]: https://nixos.wiki/wiki/Flakes
[nix-flakes-enable]: https://nixos.wiki/wiki/Flakes#Installing_flakes
[nix-how]: https://nixos.org/guides/how-nix-works.html
[nix-install]: https://nixos.org/guides/install-nix.html
[simplelogger]: http://www.slf4j.org/api/org/slf4j/impl/SimpleLogger.html
[z3]: https://github.com/Z3Prover/z3
[z3-example-java-readme]: https://github.com/Z3Prover/z3/blob/z3-4.8.10/examples/java/README
[z3-readme-java]: https://github.com/Z3Prover/z3/blob/z3-4.8.10/README.md#java
[z3-releases]: https://github.com/Z3Prover/z3/releases
