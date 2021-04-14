# atlas

[![DOI](https://zenodo.org/badge/156873559.svg)](https://zenodo.org/badge/latestdoi/156873559)
[![codecov](https://codecov.io/gh/lorenzleutgeb/atlas/branch/main/graph/badge.svg?token=cXfOoGOXV2)](https://codecov.io/gh/lorenzleutgeb/atlas)
[![codebeat badge](https://codebeat.co/badges/64e5fc79-f2b7-4a49-ac3e-bf19395d1b07)](https://codebeat.co/projects/github-com-lorenzleutgeb-atlas-main)

A tool for Automated Analysis of Logarithmic Amoritzed Complexity

See [arXiv:1807.08242][arxiv-1] and [arXiv:2101.12029][arxiv-2].

## Building

`atlas` is built using [Gradle][gradle]. Pre- and postprocessing is implemented in
Java, and [The Z3 Theorem Prover] is used for solving.

Using Gradle, `atlas` can be built for multiple target environments:

 - JAR file (including Z3). Requires a JVM on the target system.
 - ELF x86_64 binary (including Z3, from the JAR via [GraalVM Native Image][graalvm-native-image]).
   Requires a loader and some (very common) shared libraries.
 - Docker Image (based on ELF x86_64 binary).
 - Open Virtual Appliance (OVA; based on ELF x86_64 binary, and [NixOS][nixos]).

x86_64 is the only architecure supported. Linux is the only operating system supported.

The build process is fully automated with [Nix][nix] as a [flake][nix-flakes],
but Nix is not necessary. Nix will run Gradle and build Z3 on demand.

Without Nix, Gradle will still handle download and installation of all
dependencies except [The Z3 Theorem Prover](https://github.com/Z3Prover/z3).
Thus, the only manual steps required are building Z3 and placing two files
in the correct locations.

### With Nix

To get an overview of the derivations that can be built run following command.
Their names match the "target environments" listed in the previous section.

```
$ nix flake show
```

For additional information on which output will work on which target, please
read the comments in `flake.nix`.

Then, to build, for example:

```
$ nix build .#defaultPackage.x86_64-linux
$ nix build --print-build-logs .#packages.x86_64-linux.atlas-oci
```

## Without Nix

[Install Gradle.][gradle-install]

### Preparing the Build

#### The Z3 Theorem Prover

To the best of our knowledge, there is no publicly available and
frequently updated Maven repository that distributes `com.microsoft.z3.jar` and
`libz3java.so`. Also, the maintainers of Z3 do not attach these files to
[their GitHub releases][z3-releases]. This makes it hard to manage the
dependency on Z3 with Gradle, so it must be manually prepared.

Refer to [the Z3 `README.md` and its section on Java][z3-readme-java] as well as
[the Z3 example using Java][z3-example-java-readme].
Follow instructions to build `com.microsoft.z3.jar` and `libz3java.so`.

##### JAR

Copy `com.microsoft.z3.jar` to `lib/share/java/com.microsoft.z3.jar`.

##### Shared Library

Copy `libz3java.so` to `src/main/resources/jni/linux/amd64/libz3java.so`.

### Building a JAR File

```console
$ gradle build
```

## Java Properties

Behaviour of the system can be controlled by setting Java properties.

Z3 specific configuration can be set via `com.microsoft.z3.*`,
for example `com.microsoft.z3.memory_max_size=25769803776` or
`com.microsoft.z3.unsat_core=true` (see also `atlas.properties`).
Logging-specific configuration can be set [via `org.slf4j.simpleLogger.*`][simplelogger].
Tool-specific configuration can be set via `xyz.leutgeb.lorenz.atlas.*`.

## Usage

Some of the above properties are exposed as command-line arugments.

When inferring, set minimization target (`--minimize={roots,all}`).
Fix right sides (`--mode={amortized,worstcase,free}`).

## Interpreting Results

The size of a tree `t`, denoted as `|t|` is defined to be the number of its leaves. That, is `|leaf| := 1` as well as
`|(t₁, _, t₂)| := |t₁| + |t₂|`.

## Related Repositories

 - [`lorenzleutgeb/atlas-examples`](https://github.com/lorenzleutgeb/atlas-examples)
 - [`lorenzleutgeb/atlas-thesis`](https://github.com/lorenzleutgeb/atlas-thesis)
 - [`lorenzleutgeb/atlas-hs`](https://github.com/lorenzleutgeb/atlas-hs)

## Reading

 - [Mechanically Proving Termination Using Polynomial Interpretations](https://doi.org/10.1007/s10817-005-9022-x)
 - ["Carbon Credits" for Resource-Bounded Computations Using Amortised Analysis](https://doi.org/10.1007/978-3-642-05089-3_23)
 - ...

[arxiv-1]: https://arxiv.org/abs/1807.08242
[arxiv-2]: https://arxiv.org/abs/2101.12029
[graalvm-native-image]: https://www.graalvm.org/reference-manual/native-image/
[nixos]: https://nixos.org/
[nix]: https://nixos.org/nix
[nix-flakes]: https://nixos.wiki/wiki/Flakes
[z3-readme-java]: https://github.com/Z3Prover/z3/blob/z3-4.8.10/README.md#java
[z3-example-java-readme]: https://github.com/Z3Prover/z3/blob/z3-4.8.10/examples/java/README
[gradle-install]: https://gradle.org/install/
[z3-releases]: https://github.com/Z3Prover/z3/releases
[simplelogger]: http://www.slf4j.org/api/org/slf4j/impl/SimpleLogger.html
