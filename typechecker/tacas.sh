#! /usr/bin/env bash

set -e

ROOT=$PWD
DIST=$PWD/build/distributions
JDK=$DIST/tacas/artifact/jdk
JDK_URL="https://download.java.net/java/GA/jdk14.0.2/205943a0976c4ed48cb16f1043c5c647/12/GPL/openjdk-14.0.2_linux-x64_bin.tar.gz"
PROJECT=typechecker
VERSION=$(../version.sh)

gradle build -x test

# Create folders needed below.
mkdir -p $DIST/tacas/artifact/{lib,bin,resources/grammars}

# Copy over executable and remove version specifier.
cp -v build/native-image/$PROJECT-$VERSION $DIST/tacas/artifact/bin/$PROJECT

# Copy over the TACAS directory to the distribution.
# This adds Readme.txt, License.txt and the wrapper
# to use OpenJDK 14.
cp -Rv tacas $DIST/

cp -Rv src/test/resources $DIST/tacas/artifact
cp -v src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/*.g4 $DIST/tacas/artifact/resources/grammars

# Copy over libz3java.so
cp -v $ROOT/lib/libz3java.so $DIST/tacas/artifact/lib

# Package everything into a ZIP file and delete the files afterwards.
cd $DIST/tacas
zip -r ../tacas.zip .
rm -rf $DIST/tacas

sha256sum $DIST/tacas.zip
