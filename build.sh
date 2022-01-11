#!/bin/sh

export GRPC_REQUEST_SCENARIO=${GRPC_REQUEST_SCENARIO:-"complex_proto"}

# Build ghz Docker image.
# See ghz-tool/Dockerfile for details/version
docker build -t ghz_bench:latest ./ghz-tool/

## The list of benchmarks to build
BENCHMARKS_TO_BUILD="${@}"
##  ...or use all the *_bench dirs by default
BENCHMARKS_TO_BUILD="${BENCHMARKS_TO_BUILD:-$(find . -maxdepth 1 -name '*_bench' -type d | sort)}"

# Setup the chosen scenario
if ! sh setup_scenario.sh $GRPC_REQUEST_SCENARIO false; then
	echo "Scenario setup fiascoed."
	exit 1
fi

builds=""
for benchmark in ${BENCHMARKS_TO_BUILD}; do
	echo "==> Building Docker image for ${benchmark}..."
	( (
		DOCKER_BUILDKIT=1 docker image build --force-rm --file "${benchmark}/Dockerfile" \
			--tag "${benchmark##*/}" . >"${benchmark}.tmp" 2>&1 &&
			rm -f "${benchmark}.tmp" &&
			echo "==> Done building ${benchmark}"
	) || (
		cat "${benchmark}.tmp"
		rm -f "${benchmark}.tmp"
		echo "==> Error building ${benchmark}"
		exit 1
	) ) &
	builds="${builds} ${!}"
done

echo "==> Waiting for the builds to finish..."
for job in ${builds}; do
	if ! wait "${job}"; then
		wait
		echo "Error building Docker image(s)"
		exit 1
	fi
done
echo "All done."
