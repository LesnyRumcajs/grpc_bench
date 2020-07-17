#!/bin/sh

for benchmark in rust_tonic go_grpc cpp_grpc ruby_grpc python_grpc; do
	echo "==> Building ${benchmark} Docker image..."
	((
		DOCKER_BUILDKIT=1 docker image build --force-rm \
		--file ${benchmark}/Dockerfile --tag ${benchmark} . \
		> ${benchmark}.log 2>&1 && \
		echo "==> Done building ${benchmark}" && \
		rm -f ${benchmark}.log
	) || (
		echo "==> Error building ${benchmark}"
		echo "    See ${benchmark}.log for more details..."
	)) &
done
wait
