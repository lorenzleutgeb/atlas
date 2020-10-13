#! /usr/bin/env sh

gradle run -p ../typechecker --args="hs --home=\"../typechecker/src/test/resources/examples\" \".*\" $PWD/generated-src"
