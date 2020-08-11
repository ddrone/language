#!/usr/bin/env bash
set -e
kotlinc src/*.kt -include-runtime -d lang.jar
java -jar lang.jar
