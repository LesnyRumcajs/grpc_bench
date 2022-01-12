#!/bin/sh

export GRPC_IMAGE_NAME="${GRPC_IMAGE_NAME:-grpc_bench}"


## The list of benchmarks to clean
BENCHMARKS_TO_CLEAN="${@}"

IMAGES_TO_CLEAN="${BENCHMARKS_TO_CLEAN:-}"

##  ...or use all the *_bench dirs by default
BENCHMARKS_TO_CLEAN="${BENCHMARKS_TO_CLEAN:-$(find . -maxdepth 1 -name '*_bench' -type d | sort)}"

for benchmark in ${BENCHMARKS_TO_CLEAN}; do
	benchmark=${benchmark##*/}

    for scenario in $(find scenarios/ -maxdepth 1 -type d | tail -n+2 | sort); do
		scenario=${scenario##scenarios/}
		IMAGES_TO_CLEAN="${IMAGES_TO_CLEAN} ${GRPC_IMAGE_NAME}:${benchmark}-${scenario}"
	done

	IMAGES_TO_CLEAN="${IMAGES_TO_CLEAN} $(
		grep -i '^FROM ' "${benchmark}"/Dockerfile | awk '{print $2}'
	)"
done

docker image remove ${IMAGES_TO_CLEAN}
