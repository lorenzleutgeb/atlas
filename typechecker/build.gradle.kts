import org.gradle.api.tasks.testing.logging.TestExceptionFormat

val rootPackage = "xyz.leutgeb.lorenz.lac"
val rootPackagePath = rootPackage.replace(".", "/")
val mainClassName = "$rootPackage.Main"

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

version = run("../version.sh")

task("printVersion") {
    doLast { println(project.version) }
}

plugins {
    java
    application
    antlr

    id("com.diffplug.spotless") version "5.2.0"
    id("com.github.jk1.dependency-license-report") version "1.13"
    id("com.github.johnrengelman.shadow") version "5.2.0"
    id("org.mikeneck.graalvm-native-image") version "0.8.0"
}

repositories {
    jcenter()
    mavenCentral()
}

java {
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

configurations {
    create("z3")
}

buildscript {
    dependencyLocking {
        lockAllConfigurations()
    }
}

dependencyLocking {
    lockAllConfigurations()
}

dependencies {
    // We need to give the ANTLR Plugin a hint.
    val antlrDependency = "org.antlr:antlr4:4.8-1"
    antlr(antlrDependency)
    implementation(antlrDependency)

    implementation("com.google.guava:guava:28.2-jre")

    // Logging
    // fun log4j(x: String): String {
    //     return "org.apache.logging.log4j:log4j-$x:2.13.1"
    // }
    // implementation(log4j("api"))
    // implementation(log4j("core"))

    // Lombok
    val lombok = "org.projectlombok:lombok:1.18.12"
    compileOnly(lombok)
    testCompileOnly(lombok)
    annotationProcessor(lombok)

    // Graphs
    fun jgrapht(x: String): String {
        return "org.jgrapht:jgrapht-$x:1.4.0"
    }
    implementation(jgrapht("core"))
    implementation(jgrapht("io"))

    // Commandline Parameters
    implementation("info.picocli:picocli:4.2.0")
    annotationProcessor("info.picocli:picocli-codegen:4.2.0")

    // Testing
    fun jupiter(x: String): String {
        return "org.junit.jupiter:junit-jupiter$x:5.7.0"
    }
    testImplementation(jupiter("-params"))
    testRuntimeOnly(jupiter(""))

    // The Z3 Theorem Prover
    // See https://github.com/Z3Prover/z3#java
    implementation(files("lib/com.microsoft.z3.jar"))

    // Graph output
    implementation("guru.nidi:graphviz-java:0.15.0")

    // Logging
    implementation("org.slf4j:slf4j-simple:1.7.30")

    testImplementation("tech.tablesaw:tablesaw-core:0.38.1")
}

application {
    mainClassName = "$rootPackage.Main"
}

tasks.shadowJar {
    archiveClassifier.set("shadow")
}

tasks.withType<JavaCompile> {
    options.compilerArgs.addAll(arrayOf("-Xlint:unchecked", "-Xlint:deprecation"))
}

tasks.nativeImage {
    setGraalVmHome(System.getenv("GRAAL_HOME"))
    mainClass = "$rootPackage.Main"
    executableName = project.name + "-" + project.version
    jarTask = tasks.shadowJar.get()
    arguments(
            "--no-fallback",
            // "--enable-all-security-services",
            "--initialize-at-build-time=com.microsoft.z3.Native",
            "--report-unsupported-elements-at-runtime",
            "-H:+ReportExceptionStackTraces"
            // "--static",
            // "--libc=musl",
            // "-H:+JNI"
            // "-H:+StaticExecutableWithDynamicLibC"
    )
}

tasks["build"].dependsOn("nativeImage")

tasks.withType<AntlrTask> {
    // See https://github.com/antlr/antlr4/blob/master/doc/tool-options.md
    arguments.addAll(arrayOf(
            "-visitor",
            "-no-listener",
            "-long-messages",
            "-package", "$rootPackage.antlr",
            "-Werror",
            "-Xlog",
            "-lib", "src/main/antlr/$rootPackagePath/antlr"
    ))
}

tasks.test {
    useJUnitPlatform()
    testLogging {
        events("failed")
        showStackTraces = true
        exceptionFormat = TestExceptionFormat.FULL
    }
}

// Aggressively format code when building.
tasks["build"].dependsOn("spotlessJavaApply")

spotless {
    java {
        // Please do not add any custom configuration here.
        // We just bow and abide to Google's rules,
        // trading off individualism for simplicity.
        googleJavaFormat("1.9")
        // Explicitly point gjf at src, otherwise it will also check build and find ANTLR generated code.
        target("src/**/*.java")
    }
    kotlinGradle {
        ktlint()

        target("build.gradle.kts")
    }
}
