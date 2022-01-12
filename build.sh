#!/bin/sh

export GRPC_REQUEST_SCENARIO=${GRPC_REQUEST_SCENARIO:-"complex_proto"}
export GRPC_TAGS_PREFIX=${GRPC_TAGS_PREFIX:-}

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

branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)

builds=""
for benchmark in ${BENCHMARKS_TO_BUILD}; do
	benchmark=${benchmark##*/}

	cachefrom="$GRPC_TAGS_PREFIX${benchmark}"
	while read -r scenario; do
		scenario=${scenario##scenarios/}
		echo "$GRPC_TAGS_PREFIX${benchmark}:$GRPC_REQUEST_SCENARIO"
		echo "$GRPC_TAGS_PREFIX${benchmark}:$GRPC_REQUEST_SCENARIO-$branch"
		cachefrom="$cachefrom,$GRPC_TAGS_PREFIX${benchmark}:$GRPC_REQUEST_SCENARIO"
		cachefrom="$cachefrom,$GRPC_TAGS_PREFIX${benchmark}:$GRPC_REQUEST_SCENARIO-$branch"
	done < <(find scenarios/ -type d | tail -n+2) \
		| xargs -n1 docker pull --quiet || true

	echo "==> Building Docker image for ${benchmark}..."
	( (
		DOCKER_BUILDKIT=1 docker image build \
			--force-rm \
			--pull \
			--compress \
			--cache-from="$cachefrom" \
			--file "${benchmark}/Dockerfile" \
			--tag "$$GRPC_TAGS_PREFIX${benchmark}:$GRPC_REQUEST_SCENARIO" \
			. >"${benchmark}.tmp" 2>&1 &&
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
