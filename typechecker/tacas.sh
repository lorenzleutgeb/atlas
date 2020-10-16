#! /usr/bin/env bash

set -e

ROOT=$PWD
DIST=$PWD/build/distributions
JDK=$DIST/tacas/artifact/jdk
JDK_URL="https://download.java.net/java/GA/jdk14.0.2/205943a0976c4ed48cb16f1043c5c647/12/GPL/openjdk-14.0.2_linux-x64_bin.tar.gz"
PROJECT=typechecker

# Build the project and generate a "distribution" of it with Gradle.
gradle distTar

# Create folders needed below.
mkdir -p $DIST/tacas/artifact/{z3,java,resources}

# Untar the Gradle "distribution" to modify it.
tar xvf $DIST/${PROJECT}.tar -C $DIST/tacas/artifact --strip-components=1

# Copy over the TACAS directory to the distribution.
# This adds Readme.txt, License.txt and the wrapper
# to use OpenJDK 14.
cp -Rv tacas $DIST/

cp -Rv src/test/resources $DIST/tacas/artifact

mkdir -p $DIST/tacas/resources/grammars 
cp -v src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/*.g4 $DIST/tacas/resources/grammars 

# Copy over libz3java.so
cp -v $ROOT/lib/libz3java.so $DIST/tacas/artifact/z3

# Download and extract OpenJDK 14.
curl $JDK_URL | tar xvz -C $DIST/tacas/artifact/java --strip-components=1

# Package everything into a ZIP file and delete the files afterwards.
cd $DIST/tacas
zip -r ../tacas.zip .
rm -rf $DIST/tacas

sha256sum $DIST/tacas.zip
