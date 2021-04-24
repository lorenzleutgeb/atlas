import org.gradle.api.tasks.testing.logging.TestExceptionFormat

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

version = run("./version.sh")

task("printVersion") {
    doLast { println(project.version) }
}

plugins {
    java
    application
    antlr
    jacoco

    id("com.diffplug.spotless") version "5.2.0"
    id("com.github.jk1.dependency-license-report") version "1.13"
    id("com.github.johnrengelman.shadow") version "5.2.0"
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

// See https://github.com/gradle/gradle/issues/820
configurations[JavaPlugin.API_CONFIGURATION_NAME].let { apiConfiguration ->
    apiConfiguration.setExtendsFrom(apiConfiguration.extendsFrom.filter {
        it.name != "antlr"
    })
}

configurations {
    create("nativeRuntimeClasspath") {
        extendsFrom(runtimeClasspath.get())
        dependencies {
            exclude(group = "io.github.classgraph")
            exclude(group = "com.google.javaformat")
        }
    }
}

configurations.all {
    resolutionStrategy {
        force("org.antlr:antlr4-runtime:4.9")
        force("org.antlr:antlr4-tool:4.9")
    }
}

dependencies {
    val antlrVersion = "4.9"
    antlr("org.antlr:antlr4:$antlrVersion")
    compileOnly("org.antlr:antlr4:$antlrVersion")
    runtimeOnly("org.antlr:antlr4-runtime:$antlrVersion")

    implementation("com.google.guava:guava:28.2-jre")
    implementation("org.hipparchus:hipparchus-core:1.8")

    // Lombok
    val lombok = "org.projectlombok:lombok:1.18.18"
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
    implementation("info.picocli:picocli:4.6.1")
    annotationProcessor("info.picocli:picocli-codegen:4.6.1")

    // Testing
    implementation(enforcedPlatform("org.junit:junit-bom:5.7.1"))
    fun jupiter(x: String): String {
        return "org.junit.jupiter:junit-jupiter$x:5.7.1"
    }
    testImplementation(jupiter("-params"))
    testRuntimeOnly(jupiter(""))
    testImplementation("tech.tablesaw:tablesaw-core:0.38.2")

    // The Z3 Theorem Prover
    implementation("org.sosy-lab:javasmt-solver-z3:4.8.10:com.microsoft.z3@jar")

    // Graph output
    implementation("guru.nidi:graphviz-java:0.18.0") {
        // NOTE: Somehow JUnit ends up on the classpath...
        exclude("org.junit.platform")
        exclude(group = "net.arnx", module = "nashorn-promise")
        exclude(group = "org.webjars.npm", module = "viz.js-for-graphviz-java")
    }

    // Logging
    implementation("org.slf4j:slf4j-simple:1.7.30")

    // Native Image
    compileOnly("org.graalvm.nativeimage:svm:21.0.0")

    implementation("com.google.googlejavaformat:google-java-format:1.9")
    implementation("io.github.classgraph:classgraph:4.8.102")

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
    sourceCompatibility = JavaVersion.VERSION_11
    targetCompatibility = JavaVersion.VERSION_11
}

tasks.withType<AntlrTask> {
    // See https://github.com/antlr/antlr4/blob/master/doc/tool-options.md
    arguments.addAll(arrayOf(
            "-visitor",
            "-no-listener",
            "-long-messages",
            "-package", "$rootPackage.antlr",
            // "-Werror",
            "-Xlog",
            "-lib", "src/main/antlr/$rootPackagePath/antlr"
    ))
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
    main = "$rootPackage.Main"
    args("--home=src/test/resources/examples", "java", ".*", atlasDir)
}

tasks.withType<JavaCompile> {
    options.compilerArgs.addAll(arrayOf("-Xlint:unchecked", "-Xlint:deprecation", "-Aproject=atlas", "-Averbose=true"))
}

tasks.test {
    useJUnitPlatform()
    testLogging {
        events("failed")
        showStackTraces = true
        exceptionFormat = TestExceptionFormat.FULL
    }
}

application {
    mainClass.set("$rootPackage.Main")
}

tasks.create<JavaCompile>("compileGeneratedJava") {
    dependsOn("generateExamples")
    source = fileTree(buildDir.resolve("generated-src/atlas/main"))
    classpath = project.configurations.compileClasspath.get() + sourceSets.main.get().output
    destinationDir = sourceSets.main.get().output.classesDirs.singleFile
}

tasks.shadowJar {
    dependsOn("compileGeneratedJava")
    archiveClassifier.set("shadow")

    manifest {
        attributes["Main-Class"] = "$rootPackage.Main"
    }

    /* TODO: Minimization removes too many classes, like those related to JSON.
    minimize {
        exclude(dependency("org.glassfish:jakarta.json:.*"))
    }
     */
}

tasks.create<Exec>("nativeImage") {
    dependsOn("compileJava")
    dependsOn("processResources")
    val outputSuffix = if ("".equals(project.version)) "" else "-" + project.version
    val cp = "${project.configurations["nativeRuntimeClasspath"].asPath}:${sourceSets.main.get().output.asPath}"
    commandLine(
            "native-image",
            "--class-path=$cp",
            // "--static",
            // "-H:+StaticExecutableWithDynamicLibC",
            "$rootPackage.Main",
            "${project.buildDir}/native-image/${project.name}$outputSuffix"
    )
}

tasks.build.get().dependsOn(tasks.get("nativeImage"))

// tasks.compileJava.get().dependsOn(tasks.spotlessApply.get())

spotless {
    java {
        googleJavaFormat("1.9")
        target("src/**/*.java")
    }
    kotlinGradle {
        ktlint()
        target("build.gradle.kts")
    }
}

tasks.jacocoTestReport {
    reports {
        xml.isEnabled = true
        html.isEnabled = true
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
