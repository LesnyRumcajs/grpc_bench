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
  shellcheck:
    runs-on: ubuntu-latest
    env:
      VSN: v0.8.0
    steps:
    - uses: actions/checkout@v2
    - run: |
        wget -qO- "https://github.com/koalaman/shellcheck/releases/download/\$VSN/shellcheck-\$VSN.linux.x86_64.tar.xz" | tar -xJv
        ./shellcheck-\$VSN/shellcheck --version
    # TODO(fenollp): \$(find . -type f -name '*.sh')
    - run: ./shellcheck-\$VSN/shellcheck ./generate_ci.sh

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
EOF
    docker_login_then_checkout
    cat <<EOF
    - run: ./build.sh $bench
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-complex_proto \$GRPC_IMAGE_NAME:$bench-complex_proto-\$GITHUB_REF_NAME
    - run: docker push \$GRPC_IMAGE_NAME:$bench-complex_proto-\$GITHUB_REF_NAME

EOF

    # Build & push branch-specific images for all other scenarios
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
EOF
        docker_login_then_checkout
        cat <<EOF
    - run: GRPC_REQUEST_SCENARIO=$scenario ./build.sh $bench
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-$scenario \$GRPC_IMAGE_NAME$bench-$scenario-\$GITHUB_REF_NAME
    - run: docker push \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME

EOF

        # Use branch-specific images
        cat <<EOF
  bench-$bench-$scenario:
    runs-on: ubuntu-latest
    needs: build-$bench-$scenario
    steps:
EOF
        docker_login_then_checkout
        cat <<EOF
    - run: docker pull \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME
    - run: docker tag  \$GRPC_IMAGE_NAME:$bench-$scenario-\$GITHUB_REF_NAME \$GRPC_IMAGE_NAME:$bench-$scenario
    - run: GRPC_REQUEST_SCENARIO=$scenario ./bench.sh $bench
    - name: If on master push naked image as well
      if: \${{ github.ref == 'refs/heads/master' }}
      run: docker push \$GRPC_IMAGE_NAME:$bench-$scenario

EOF

    done < <(find scenarios/ -type d | tail -n+2 | sort)
# done < <(find . -maxdepth 1 -type d -name '*_bench' | sort)
done < <(printf 'rust_tonic_mt_bench\npython_async_grpc_bench\n' | sort)
