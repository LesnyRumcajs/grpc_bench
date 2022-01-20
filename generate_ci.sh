#!/bin/bash -eu

set -o pipefail

export GRPC_REQUEST_SCENARIO=${GRPC_REQUEST_SCENARIO:-"complex_proto"}

cat <<EOF
name: "B&B scenario: $GRPC_REQUEST_SCENARIO"

on:
  push:
  pull_request:

env:
  GRPC_REQUEST_SCENARIO: $GRPC_REQUEST_SCENARIO

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

    cat <<EOF
  $bench:
    runs-on: ubuntu-latest
    needs: meta-check
    steps:
    - name: Checkout
      uses: actions/checkout@v2

    - name: Set GRPC_IMAGE_NAME
      run: |
        SLUG=\${SLUG,,} # Lowercase
        echo "GRPC_IMAGE_NAME=ghcr.io/\$SLUG" >>\$GITHUB_ENV
      env:
        SLUG: \${{ github.repository }}

    - name: Build $bench
      run: ./build.sh $bench

    - name: Benchmark $bench
      run: ./bench.sh $bench
      env:
        GRPC_BENCHMARK_DURATION: 30s

    - if: \${{ github.ref == 'refs/heads/master' }}
      name: Log in to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: \${{ github.actor }}
        password: \${{ secrets.GITHUB_TOKEN }}

    - if: \${{ github.ref == 'refs/heads/master' }}
      name: If on master push image to GHCR
      run: docker push \$GRPC_IMAGE_NAME:$bench-$GRPC_REQUEST_SCENARIO

EOF

done < <(find . -maxdepth 1 -type d -name '*_bench' | sort)
