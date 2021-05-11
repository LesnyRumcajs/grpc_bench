#!/bin/bash
if [ "$VERBOSE" != "true" ]; then
  exec &>/dev/null
fi

set -Eeox pipefail

SCC_SIZE="64m"  # Default size of the SCC layer.
ITERATIONS=2    # Number of iterations to run to populate it.

SCC="-Xshareclasses:name=grcp,cacheDir=/app/.classCache"

OPENJ9_JAVA_OPTIONS="${JAVA_OPTS[@]} $SCC"
CREATE_LAYER="$OPENJ9_JAVA_OPTIONS,createLayer,groupAccess"
DESTROY_LAYER="$OPENJ9_JAVA_OPTIONS,destroy"
PRINT_LAYER_STATS="$OPENJ9_JAVA_OPTIONS,printTopLayerStats"

OLD_UMASK=`umask`
umask 002 # 002 is required to provide group rw permission to the cache when `-Xshareclasses:groupAccess` options is used

# Explicity create a class cache layer for this image layer here rather than allowing
# `server start` to do it, which will lead to problems because multiple JVMs will be started.
java $CREATE_LAYER -Xscmx$SCC_SIZE -version

# Populate the newly created class cache layer.
set +e
for ((i=0; i<$ITERATIONS; i++))
do
 /app/build/install/examples/bin/hello-world-server &
 # this would be much beneficial
 #/app/build/install/examples/bin/hello-world-client &
 pkill java
done
set -e

# restore umask
umask ${OLD_UMASK}

# Tell the user how full the final layer is.
FULL=`( java $PRINT_LAYER_STATS || true ) 2>&1 | awk '/^Cache is [0-9.]*% .*full/ {print substr($3, 1, length($3)-1)}'`
echo "SCC layer is $FULL% full."
