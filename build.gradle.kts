plugins {
    java
    application
    antlr

    id("com.diffplug.gradle.spotless") version "3.16.0"
    id("com.gradle.build-scan") version "2.0.2"
}

repositories {
    jcenter()
    mavenCentral()
}

dependencies {
    // We need to give the ANTLR Plugin a hint.
    antlr("org.antlr:antlr4:4.7.1")
    implementation("com.google.guava:guava:26.0-jre")
    implementation("org.antlr:antlr4:4.7.1")

    testImplementation("junit:junit:4.12")
}

application {
    mainClassName = "xyz.leutgeb.lorenz.logs.App"
}

tasks.withType<JavaCompile> {
    options.compilerArgs.addAll(arrayOf("-Xlint:unchecked", "-Xlint:deprecation"))
}

spotless {
    java {
        // Please do not add any custom configuration here.
        // We just bow and abide to Google's rules,
        // trading off individualism for simplicity.
        googleJavaFormat()
    }
    kotlin {
        ktlint()
    }
}

tasks.withType<AntlrTask> {
    // See https://github.com/antlr/antlr4/blob/master/doc/tool-options.md
    arguments.addAll(arrayOf(
        "-visitor",
        "-no-listener",
        "-long-messages",
        "-package", "xyz.leutgeb.lorenz.logs.antlr",
        "-Werror",
        "-Xlog",
        "-lib", "src/main/antlr/xyz/leutgeb/lorenz/logs/antlr"
    ))
}

buildScan {
      termsOfServiceUrl = "https://gradle.com/terms-of-service"
      termsOfServiceAgree = "yes"
      if (System.getenv("CI") != null) {
            publishAlways()
            tag("ci")
      }
}