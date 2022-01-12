#!/bin/bash -eu

set -o pipefail

cat <<EOF
name: CI

on:
  push:
  pull_request:

env:
  GRPC_TAGS_PREFIX: fenollp/

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
  build-$bench-complex_proto:
    runs-on: ubuntu-latest
    needs: meta-check
    steps:
    - uses: docker/login-action@v1
      with:
        username: \${{ secrets.DOCKERHUB_USERNAME }}
        password: \${{ secrets.DOCKERHUB_TOKEN }}
    - uses: actions/checkout@v2
    - run: ./build.sh $bench
    - run: docker tag \$GRPC_TAGS_PREFIX$bench:complex_proto \$GRPC_TAGS_PREFIX$bench:complex_proto-\$GITHUB_REF_NAME
    - run: docker push \$GRPC_TAGS_PREFIX$bench:complex_proto-\$GITHUB_REF_NAME
EOF

    # Build & push branch-specific images for all other scenarii
    while read -r scenario; do
        scenario=${scenario##scenarios/}
        if [[ "$scenario" = complex_proto ]]; then
            continue
        fi
        cat <<EOF
  build-$bench-$scenario:
    runs-on: ubuntu-latest
    needs: build-$bench-complex_proto
    steps:
    - uses: docker/login-action@v1
      with:
        username: \${{ secrets.DOCKERHUB_USERNAME }}
        password: \${{ secrets.DOCKERHUB_TOKEN }}
    - uses: actions/checkout@v2
    - run: GRPC_REQUEST_SCENARIO=$scenario ./build.sh $bench
    - run: docker tag \$GRPC_TAGS_PREFIX$bench:$scenario \$GRPC_TAGS_PREFIX$bench:$scenario-\$GITHUB_REF_NAME
    - run: docker push \$GRPC_TAGS_PREFIX$bench:$scenario-\$GITHUB_REF_NAME
EOF

        # Use branch-specific images
        if [[ "$bench" = rust_tonic_mt_bench ]]; then
        cat <<EOF
  bench-$bench-$scenario:
    runs-on: ubuntu-latest
    needs: build-$bench-$scenario
    # If on master push naked image as well
    if: \${{ github.ref == 'refs/heads/master' }}
    steps:
    - uses: docker/login-action@v1
      with:
        username: \${{ secrets.DOCKERHUB_USERNAME }}
        password: \${{ secrets.DOCKERHUB_TOKEN }}
    - uses: actions/checkout@v2
    - run: docker pull \${GRPC_TAGS_PREFIX}$bench:$scenario-\$GITHUB_REF_NAME
    - run: docker tag  \${GRPC_TAGS_PREFIX}$bench:$scenario-\$GITHUB_REF_NAME \${GRPC_TAGS_PREFIX}$bench:$scenario
    - run: docker push \${GRPC_TAGS_PREFIX}$bench:$scenario
EOF
            continue
        fi
        cat <<EOF
  bench-$bench-$scenario:
    runs-on: ubuntu-latest
    needs: build-$bench-$scenario
    steps:
    - uses: docker/login-action@v1
      with:
        username: \${{ secrets.DOCKERHUB_USERNAME }}
        password: \${{ secrets.DOCKERHUB_TOKEN }}
    - uses: actions/checkout@v2
    - run: |
        {
            echo \${GRPC_TAGS_PREFIX}$bench:$scenario-\$GITHUB_REF_NAME
            echo \${GRPC_TAGS_PREFIX}rust_tonic_mt_bench:$scenario-\$GITHUB_REF_NAME
        } | xargs -n1 docker pull --quiet
    - run: |
        docker tag \${GRPC_TAGS_PREFIX}$bench:$scenario-\$GITHUB_REF_NAME \${GRPC_TAGS_PREFIX}$bench:$scenario
        docker tag \${GRPC_TAGS_PREFIX}rust_tonic_mt_bench:$scenario-\$GITHUB_REF_NAME \${GRPC_TAGS_PREFIX}rust_tonic_mt_bench:$scenario
    - run: GRPC_REQUEST_SCENARIO=$scenario ./bench.sh $bench rust_tonic_mt_bench
    # If on master push naked image as well
    - if: \${{ github.ref == 'refs/heads/master' }}
      run: docker push \${GRPC_TAGS_PREFIX}$bench:$scenario
EOF

    done < <(find scenarios/ -type d | tail -n+2)
done < <(find . -maxdepth 1 -type d -name '*_bench' | sort)
