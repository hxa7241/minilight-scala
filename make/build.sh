#!/bin/bash

# --- using Scala 2.10 ---

mkdir obj

scalac -optimise -d obj -Xmigration -deprecation -unchecked -Xlint src/hxa7241/general/*.scala src/hxa7241/graphics/*.scala src/hxa7241/minilight/*.scala

exit
