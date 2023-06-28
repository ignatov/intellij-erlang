#!/usr/bin/env bash

JFLEX_JAR=${1:-~/src/intellij-community/tools/lexer/jflex-1.7.0-SNAPSHOT.jar}
JFLEX_SKEL=${2:-~/src/intellij-community/tools/lexer/idea-flex.skeleton}
JFLEX_OUT=${3:-../../../../../gen/org/intellij/erlang/parser/}
JFLEX_GRAMMAR=${4:-Erlang.flex}

java -jar "$JFLEX_JAR" --skel "$JFLEX_SKEL" --nobak "$JFLEX_GRAMMAR" -d "$JFLEX_OUT"