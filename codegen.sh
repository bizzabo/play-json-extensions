#!/bin/sh
cd "$(dirname "$0")"
scala codegen.script > src/main/scala/tuples.scala
