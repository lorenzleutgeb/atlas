#! /usr/bin/env sh

jshell \
	--feedback verbose \
	--class-path build/libs/typechecker-v0.0.2-2-g91bb6ff-dirty-shadow.jar \
	--startup DEFAULT \
	--startup PRINTING \
	--startup lac.jsh
