#! /usr/bin/env sh

../typechecker/gradlew run -p ../typechecker --args="hs --home=\"../typechecker/src/test/resources/examples\" \".*\" $PWD/generated-src"
