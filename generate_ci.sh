#!/bin/bash -eu

set -o pipefail

docker_login_then_checkout() {
    cat <<EOF
    - name: Log in to Docker Hub
      uses: docker/login-action@v1
      with:
        username: \${{ secrets.DOCKERHUB_USERNAME }}
        password: \${{ secrets.DOCKERHUB_TOKEN }}
    - name: Log in to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: \${{ github.actor }}
        password: \${{ secrets.GITHUB_TOKEN }}
    - name: Checkout code
      uses: actions/checkout@v2
EOF
}

cat <<EOF
name: CI

on:
  push:
  pull_request:

env:
  GRPC_IMAGE_NAME: ghcr.io/\${{ github.repository }}
  GRPC_BENCHMARK_DURATION: 30s

jobs:
  meta-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - run: ./generate_ci.sh | tee .github/workflows/build.yml
    - run: git --no-pager diff --exit-code

EOF

while read -r bench; do
    bench=${bench##./}

    # Build & push branch-specific image for complex_proto

    cat <<EOF
  ${bench//_bench}-complex_proto:
    runs-on: ubuntu-latest
    needs: meta-check
    steps:
EOF
    docker_login_then_checkout
    cat <<EOF
    - run: ./build.sh $bench
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-complex_proto \$GRPC_IMAGE_NAME:$bench-complex_proto-\$GITHUB_REF_NAME
    - run: docker push \$GRPC_IMAGE_NAME:$bench-complex_proto-\$GITHUB_REF_NAME

EOF

    while read -r scenario; do
        scenario=${scenario##scenarios/}

        # Benchmark -complex_proto- images

        if [[ "$scenario" = complex_proto ]]; then
            cat <<EOF
  bench-${bench//_bench}-$scenario:
    runs-on: ubuntu-latest
    needs: ${bench//_bench}-$scenario
    steps:
EOF
            docker_login_then_checkout
            cat <<EOF
    - run: docker pull \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME \$GRPC_IMAGE_NAME:$bench-$scenario
    - run: ./bench.sh $bench
    - name: If on master push naked image as well
      if: \${{ github.ref == 'refs/heads/master' }}
      run: docker push \$GRPC_IMAGE_NAME:$bench-$scenario

EOF
            # May also push $GRPC_IMAGE_NAME:$bench for convenience?
        # - name: If on master push naked image as well
        #   if: \${{ github.ref == 'refs/heads/master' }}
        #   run: |
        #     docker tag \$GRPC_IMAGE_NAME:$bench-$scenario \$GRPC_IMAGE_NAME:$bench
        #     docker push \$GRPC_IMAGE_NAME:$bench-$scenario
            continue
        fi


        # Build & benchmark all other scenarios

        cat <<EOF
  ${bench//_bench}-$scenario:
    runs-on: ubuntu-latest
    needs: ${bench//_bench}-complex_proto
    steps:
EOF
        docker_login_then_checkout
        cat <<EOF
    - run: GRPC_REQUEST_SCENARIO=$scenario ./build.sh $bench
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-$scenario \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME
    - run: docker push \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME \$GRPC_IMAGE_NAME:$bench-$scenario
    - run: GRPC_REQUEST_SCENARIO=$scenario ./bench.sh $bench
    - name: If on master push naked image as well
      if: \${{ github.ref == 'refs/heads/master' }}
      run: docker push \$GRPC_IMAGE_NAME:$bench-$scenario

EOF

    # TODO: delete all other tags

    done < <(find scenarios/ -maxdepth 1 -type d | tail -n+2 | sort)
done < <(find . -maxdepth 1 -type d -name '*_bench' | sort)
