#! /usr/bin/env sh

# This script is called whenever we need to know
# the current version of the project.

git describe --tags --always --dirty=-dirty
