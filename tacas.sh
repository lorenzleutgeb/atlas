#! /usr/bin/env bash

set -e

ROOT=$PWD
DIST=$PWD/build/distributions
JDK=$DIST/tacas/artifact/jdk
JDK_URL="https://download.java.net/java/GA/jdk14.0.2/205943a0976c4ed48cb16f1043c5c647/12/GPL/openjdk-14.0.2_linux-x64_bin.tar.gz"
PROJECT=lac
VERSION=$(./version.sh)

gradle build -x test

# Create folders needed below.
mkdir -p $DIST/tacas/artifact/{dependencies,resources/grammars}

# Copy over executable and remove version specifier.
cp -v build/native-image/$PROJECT-$VERSION $DIST/tacas/artifact/lac
cp -v lac.properties $DIST/tacas/artifact/lac.properties

# Target Ubuntu
patchelf $DIST/tacas/artifact/lac \
  --set-interpreter /lib64/ld-linux-x86-64.so.2

# Copy over the TACAS directory to the distribution.
# This adds Readme.txt, License.txt and the wrapper
# to use OpenJDK 14.
cp -Rv tacas $DIST/

cd $DIST/tacas/artifact/dependencies
wget -nv http://archive.ubuntu.com/ubuntu/pool/universe/z/z3/libz3-jni_4.8.7-4build1_amd64.deb
cd $ROOT

cp -Rv src/test/resources $DIST/tacas/artifact
cp -v src/main/antlr/xyz/leutgeb/lorenz/lac/antlr/*.g4 $DIST/tacas/artifact/resources/grammars

# Package everything into a ZIP file and delete the files afterwards.
cd $DIST/tacas
zip -r ../tacas.zip .
# rm -rf $DIST/tacas

sha256sum $DIST/tacas.zip
