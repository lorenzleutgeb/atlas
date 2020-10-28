#! /usr/bin/env sh

gradle shadowJar -x test

jshell \
	--feedback verbose \
	--class-path build/libs/lac-$(./version.sh)-shadow.jar \
	--startup DEFAULT \
	--startup PRINTING \
	--startup lac.jsh
