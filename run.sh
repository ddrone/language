#!/usr/bin/env bash
set -e
kotlinc src/*.kt -include-runtime -d lang.jar
java -jar lang.jar

# Here's a command to be used with several main classes:
#   kotlin -classpath lang.jar TestRunnerKt
