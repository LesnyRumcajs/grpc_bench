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
    - git --no-pager diff --exit-code
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
    tobench=''
    while read -r scenario; do
        scenario=${scenario##scenarios/}
        job_name=build-$bench-$scenario
        tobench="$tobench $job_name"
        if [[ "$scenario" = complex_proto ]]; then
            continue
        fi
cat <<EOF
  $job_name:
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
    done < <(find scenarios/ -type d | tail -n+2)

    # Use branch-specific images
    tobench=$(echo "$tobench" | sed 's% %,%g')
cat <<EOF
  bench-$bench:
    runs-on: ubuntu-latest
    needs: [$tobench]
    steps:
    - uses: docker/login-action@v1
      with:
        username: \${{ secrets.DOCKERHUB_USERNAME }}
        password: \${{ secrets.DOCKERHUB_TOKEN }}
    - uses: actions/checkout@v2
    - run: docker pull \$GRPC_TAGS_PREFIX$bench:$scenario-\$GITHUB_REF_NAME
    - run: docker tag  \$GRPC_TAGS_PREFIX$bench:$scenario-\$GITHUB_REF_NAME \$GRPC_TAGS_PREFIX$bench:$scenario
    - run: GRPC_REQUEST_SCENARIO=$scenario ./bench.sh $bench
    # If on master push naked image as well
    - if: \${{ github.ref == 'refs/heads/master' }}
      run: docker push \$GRPC_TAGS_PREFIX$bench:$scenario
EOF
done < <(find -type d -name '*_bench' | sort)
