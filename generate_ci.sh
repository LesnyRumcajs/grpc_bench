#!/bin/bash -eu

set -o pipefail

export GRPC_REQUEST_SCENARIO=${GRPC_REQUEST_SCENARIO:-"complex_proto"}

cat <<EOF
name: "B&B scenario: $GRPC_REQUEST_SCENARIO"

on:
  push:
  pull_request:

env:
  GRPC_IMAGE_NAME: localhost:5000/grpc_bench
  GRPC_REQUEST_SCENARIO: $GRPC_REQUEST_SCENARIO

jobs:
  meta-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - run: ./generate_ci.sh | tee .github/workflows/build.yml
    - run: git --no-pager diff --exit-code

EOF

setup() {
    local bench="$1"; shift
    cat <<EOF
    - name: Checkout
      uses: actions/checkout@v2

    - name: Setup local registry cache
      id: cache
      uses: actions/cache@v1
      with:
        path: /tmp/docker-registry
        key: docker-registry-\${{ hashFiles('proto/', '$bench/') }}

EOF
}

while read -r bench; do
    bench=${bench##./}

    # Build & cache branch-specific image for scenario

    cat <<EOF
  build-${bench//_bench}:
    runs-on: ubuntu-latest
    needs: meta-check
    steps:
EOF
    setup "$bench"
    # On cache hit: do nothing. Improve with: https://github.com/actions/runner/issues/662
    cat <<EOF
    - if: steps.cache.outputs.cache-hit != 'true'
      name: Setup local Docker registry
      run: |
        docker run -d -p 5000:5000 --restart=always --name registry -v /tmp/docker-registry:/var/lib/registry registry:2
        while ! nc -z localhost 5000; do sleep .1; done

    - if: steps.cache.outputs.cache-hit != 'true'
      name: Log in to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: \${{ github.actor }}
        password: \${{ secrets.GITHUB_TOKEN }}

    - if: steps.cache.outputs.cache-hit != 'true'
      name: Pull image to feed cache
      run: |
        SLUG=\${SLUG,,} # Make sure docker tag is lowercase
        docker pull    ghcr.io/\$SLUG:$bench-\$GRPC_REQUEST_SCENARIO || true
        docker tag     ghcr.io/\$SLUG:$bench-\$GRPC_REQUEST_SCENARIO \$GRPC_IMAGE_NAME:$bench-\$GRPC_REQUEST_SCENARIO || true
        docker push \$GRPC_IMAGE_NAME:$bench-\$GRPC_REQUEST_SCENARIO || true
      env:
        SLUG: \${{ github.repository }}

    - if: steps.cache.outputs.cache-hit != 'true'
      name: Build $bench
      run: ./build.sh $bench

    - if: steps.cache.outputs.cache-hit != 'true'
      name: Ensure local registry has most recent image
      run: |
        docker push \$GRPC_IMAGE_NAME:$bench-\$GRPC_REQUEST_SCENARIO
        du -sh /tmp/docker-registry


EOF

    # Benchmark branch-specific image for scenario

    cat <<EOF
  bench-${bench//_bench}:
    runs-on: ubuntu-latest
    needs: build-${bench//_bench}
    steps:
EOF
    setup "$bench"
    cat <<EOF
    - if: steps.cache.outputs.cache-hit != 'true'
      name: Fail job on cache miss
      run: 'false'

    - name: Setup local Docker registry
      run: |
        docker run -d -p 5000:5000 --restart=always --name registry -v /tmp/docker-registry:/var/lib/registry registry:2
        while ! nc -z localhost 5000; do sleep .1; done

    - name: Pull image from local registry
      run: docker pull \$GRPC_IMAGE_NAME:$bench-\$GRPC_REQUEST_SCENARIO
      timeout-minutes: 2

    - name: Benchmark $bench
      run: GRPC_BENCHMARK_DURATION=30s ./bench.sh $bench

    - if: \${{ github.ref == 'refs/heads/master' }}
      name: Log in to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: \${{ github.actor }}
        password: \${{ secrets.GITHUB_TOKEN }}

    - if: \${{ github.ref == 'refs/heads/master' }}
      name: If on master push image to GHCR
      run: |
        SLUG=\${SLUG,,} # Make sure docker tag is lowercase
        docker tag \$GRPC_IMAGE_NAME:$bench-\$GRPC_REQUEST_SCENARIO ghcr.io/\$SLUG:$bench-\$GRPC_REQUEST_SCENARIO
        docker push   ghcr.io/\$SLUG:$bench-\$GRPC_REQUEST_SCENARIO
      env:
        SLUG: \${{ github.repository }}

EOF

done < <(find . -maxdepth 1 -type d -name '*_bench' | sort)
