val rootPackage = "xyz.leutgeb.lorenz.lac"
val rootPackagePath = rootPackage.replace(".", "/")

plugins {
    java
    application
    antlr

    id("com.diffplug.spotless") version "5.2.0"
    id("com.github.jk1.dependency-license-report") version "1.13"
}

repositories {
    jcenter()
    mavenCentral()
}

java {
    sourceCompatibility = JavaVersion.VERSION_14
    targetCompatibility = JavaVersion.VERSION_14
}

configurations {
    create("z3")
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
    annotationProcessor(lombok)

    // Graphs
    fun jgrapht(x: String): String {
        return "org.jgrapht:jgrapht-$x:1.4.0"
    }
    implementation(jgrapht("core"))
    implementation(jgrapht("io"))

    // Commandline Parameters
    implementation("info.picocli:picocli:4.2.0")

    // Testing
    fun jupiter(x: String): String {
        return "org.junit.jupiter:junit-jupiter$x:5.6.2"
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

tasks.withType<JavaExec> {
    // TODO(lorenz.leutgeb): Remove --enable-preview as soon as used features are stable.
    jvmArgs("--enable-preview")
}

tasks.withType<JavaCompile> {
    // TODO(lorenz.leutgeb): Remove --enable-preview as soon as used features are stable.
    options.compilerArgs.addAll(arrayOf("-Xlint:unchecked", "-Xlint:deprecation", "--enable-preview"))
}

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
        events("passed", "skipped", "failed")
    }
    // TODO(lorenz.leutgeb): Remove --enable-preview as soon as used features are stable.
    jvmArgs("--enable-preview")
}

tasks.withType<Wrapper> {
    gradleVersion = "6.5"
    distributionType = Wrapper.DistributionType.ALL
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
