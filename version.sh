#! /usr/bin/env sh

# This script is called whenever we need to know
# the current version of the project.

if [ -x "$(command -v git)" ]
then
	git describe --tags --always --dirty=-dirty
else
	echo "v0"
fi
