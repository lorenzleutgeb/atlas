import org.gradle.api.tasks.testing.logging.TestExceptionFormat
import java.nio.charset.StandardCharsets

val rootPackage = "xyz.leutgeb.lorenz.atlas"
val rootPackagePath = rootPackage.replace(".", "/")

project.setProperty("mainClassName", "$rootPackage.Main")

fun run(command: String, workingDir: File = file("./")): String {
    val parts = command.split("\\s".toRegex())
    val proc = ProcessBuilder(*parts.toTypedArray())
        .directory(workingDir)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .redirectError(ProcessBuilder.Redirect.PIPE)
        .start()

    proc.waitFor(1, TimeUnit.MINUTES)
    return proc.inputStream.bufferedReader().readText().trim()
}

plugins {
    java
    application
    antlr
    jacoco

    id("com.diffplug.spotless") version "6.0.0"
    id("com.github.jk1.dependency-license-report") version "2.0"
    id("com.github.johnrengelman.shadow") version "7.1.2"
    id("org.ajoberstar.reckon") version "0.16.1"
    id("org.graalvm.buildtools.native") version "0.9.16"
}

repositories {
    mavenCentral {
        metadataSources {
            mavenPom()
        }
    }
    mavenCentral {
        metadataSources {
            artifact()
        }
    }
}

buildscript {
    dependencyLocking {
        lockAllConfigurations()
    }
}

dependencyLocking {
    lockAllConfigurations()
}

reckon {
    scopeFromProp()
    stageFromProp("rc")
}

task("printVersion") {
    doLast { println(project.version) }
}

// See https://github.com/gradle/gradle/issues/820
configurations[JavaPlugin.API_CONFIGURATION_NAME].let { apiConfiguration ->
    apiConfiguration.setExtendsFrom(
        apiConfiguration.extendsFrom.filter {
            it.name != "antlr"
        }
    )
}

configurations.all {
    resolutionStrategy {
        force("org.antlr:antlr4-runtime:4.9.3")
        force("org.antlr:antlr4-tool:4.9.3")
    }
}

dependencies {
    val antlrVersion = "4.9.3"
    antlr("org.antlr:antlr4:$antlrVersion")
    compileOnly("org.antlr:antlr4:$antlrVersion")
    runtimeOnly("org.antlr:antlr4-runtime:$antlrVersion")

    implementation("com.google.guava:guava:31.0.1-jre")
    implementation("org.hipparchus:hipparchus-core:1.8")

    // Lombok
    val lombok = "org.projectlombok:lombok:1.18.22"
    compileOnly(lombok)
    testCompileOnly(lombok)
    annotationProcessor(lombok)

    // Graphs
    fun jgrapht(x: String): String {
        return "org.jgrapht:jgrapht-$x:1.5.0"
    }
    implementation(jgrapht("core"))
    implementation(jgrapht("io"))

    // Commandline Parameters
    implementation("info.picocli:picocli:4.6.2")
    annotationProcessor("info.picocli:picocli-codegen:4.6.2")

    // Testing
    implementation(enforcedPlatform("org.junit:junit-bom:5.8.1"))
    fun jupiter(x: String): String {
        return "org.junit.jupiter:junit-jupiter$x:5.8.1"
    }
    testImplementation(jupiter("-params"))
    testRuntimeOnly(jupiter(""))
    testImplementation("tech.tablesaw:tablesaw-core:0.42.0")

    // The Z3 Theorem Prover
    implementation("org.sosy-lab:javasmt-solver-z3:4.10.1:com.microsoft.z3@jar")
    implementation("org.sosy-lab:javasmt-solver-z3:4.10.1:libz3@so")
    implementation("org.sosy-lab:javasmt-solver-z3:4.10.1:libz3java@so")
    implementation("org.sosy-lab:java-smt:3.14.3")

    // Graph output
    implementation("guru.nidi:graphviz-java:0.18.1") {
        // NOTE: Somehow JUnit ends up on the classpath...
        exclude("org.junit.platform")
        exclude(group = "net.arnx", module = "nashorn-promise")
        exclude(group = "org.webjars.npm", module = "viz.js-for-graphviz-java")
    }

    // Logging
    implementation("org.slf4j:slf4j-simple:1.7.32")

    // Native Image
    compileOnly("org.graalvm.nativeimage:svm:21.3.0")

    implementation("io.github.classgraph:classgraph:4.8.133")

    // JSON Binding
    // See https://javaee.github.io/jsonb-spec/getting-started.html
    // implementation("javax.json.bind:javax.json.bind-api:1.0")
    // implementation("org.eclipse:yasson:1.0")

    // JSON Processing
    // See https://javaee.github.io/jsonp/getting-started.html
    implementation("jakarta.json:jakarta.json-api:2.0.0")

    implementation("org.glassfish:jakarta.json:2.0.0")
}

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
}

tasks.withType<AntlrTask> {
    // See https://github.com/antlr/antlr4/blob/master/doc/tool-options.md
    arguments.addAll(
        arrayOf(
            "-visitor",
            "-no-listener",
            "-long-messages",
            "-package", "$rootPackage.antlr",
            "-Werror",
            "-Xlog",
            "-lib", "src/main/antlr/$rootPackagePath/antlr",
            "-encoding", "UTF-8"
        )
    )
}

val atlasDir = "build/generated-src/atlas/main"

sourceSets {
    main {
        java {
            srcDir(atlasDir)
        }
    }
}

tasks.create<JavaExec>("generateExamples") {
    dependsOn("compileJava")
    classpath = project.sourceSets["main"].runtimeClasspath
    mainClass.set("$rootPackage.Main")
    args("--search=src/test/resources/examples", "java", ".*", atlasDir)
}

tasks.withType<JavaCompile> {
    options.encoding = StandardCharsets.UTF_8.name()
    options.compilerArgs.addAll(arrayOf("--enable-preview", "-Xlint:unchecked", "-Xlint:deprecation", "-Aproject=atlas", "-Averbose=true"))
}

tasks.test {
    useJUnitPlatform()
    testLogging {
        events("failed")
        showStackTraces = true
        exceptionFormat = TestExceptionFormat.FULL
        showStandardStreams = true
    }

    //systemProperty("java.library.path", "/nix/store/l7j5p0dfwnpcvlyazipd2db73vkxlvbk-z3-4.8.15-lib/lib")
    //systemProperty("java.library.path", "lib")
}

application {
    mainClass.set("$rootPackage.Main")
}

tasks.create<JavaCompile>("compileGeneratedJava") {
    dependsOn("generateExamples")
    source = fileTree(buildDir.resolve("generated-src/atlas/main"))
    classpath = project.configurations.compileClasspath.get() + sourceSets.main.get().output
    destinationDirectory.set(sourceSets.main.get().output.classesDirs.singleFile)
}

tasks.shadowJar {
    // Code generation is broken as of 2022-04
    // dependsOn("compileGeneratedJava")
    archiveClassifier.set("shadow")

    manifest {
        attributes["Main-Class"] = "$rootPackage.Main"
    }
}

graalvmNative {
    binaries {
        named("main") {
            imageName.set("atlas")
            mainClass.set("$rootPackage.Main")
            sharedLibrary.set(false)
            fallback.set(false)
            verbose.set(true)
            buildArgs.addAll(
                "-H:Log=registerResource",
                "-H:+ReportExceptionStackTraces",
                "-H:+PrintClassInitialization",
                "--report-unsupported-elements-at-runtime"
            )
            javaLauncher.set(
                javaToolchains.launcherFor {
                    languageVersion.set(JavaLanguageVersion.of(17))
                    vendor.set(JvmVendorSpec.matching("GraalVM"))
                }
            )
        }
    }
}

spotless {
    java {
        googleJavaFormat("1.13.0")
        target("src/**/*.java")
    }
    kotlinGradle {
        ktlint()
        target("build.gradle.kts")
    }
}

tasks.jacocoTestReport {
    reports {
        xml.required.set(true)
        html.required.set(true)
    }

    // NOTE: Contents of the antlr subpackage are autogenerated.
    classDirectories.setFrom(
        sourceSets.main.get().output.asFileTree.matching {
            exclude("**/xyz/leutgeb/lorenz/atlas/antlr/**")
        }
    )
}

tasks.test {
    finalizedBy(tasks.jacocoTestReport) // report is always generated after tests run
}
tasks.jacocoTestReport {
    dependsOn(tasks.test) // tests are required to run before generating the report
}
