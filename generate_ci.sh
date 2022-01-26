#!/bin/bash -eu

set -o pipefail

export GRPC_REQUEST_SCENARIO=${GRPC_REQUEST_SCENARIO:-"complex_proto"}

cat <<EOF
name: B&B w/ $GRPC_REQUEST_SCENARIO

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

  set-image-name:
    runs-on: ubuntu-latest
    needs: [meta-check]
    outputs:
      name: \${{ steps.namer.outputs.name }}
    steps:
    - name: Set lowercase image name
      id: namer
      run: echo "::set-output name=name::ghcr.io/\${SLUG,,}"
      env:
        SLUG: \${{ github.repository }}

  changed:
    runs-on: ubuntu-latest
    needs: [meta-check]
    outputs:
      files: \${{ steps.finder.outputs.all_changed_and_modified_files }}
      base: \${{ steps.base.outputs.base }}
    steps:
    - name: Deep checkout
      uses: actions/checkout@v2
      with:
        fetch-depth: 0

    - name: Get changed files
      id: finder
      uses: tj-actions/changed-files@v13.1

    - name: Output whether base changed
      id: base
      run: echo "::set-output name=base::\$BASE"
      env:
        BASE: >-
          \${{
          contains(steps.finder.outputs.all_changed_and_modified_files, '.dockerignore')     ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'analyze.sh')        ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'bench.sh')          ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'build.sh')          ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'clean.sh')          ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'collect_stats.sh')  ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'generate_ci.sh')    ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'proto/')            ||
          contains(steps.finder.outputs.all_changed_and_modified_files, 'setup_scenario.sh') }}

EOF

while read -r bench; do
    bench=${bench##./}

    cat <<EOF

  $bench:
    runs-on: ubuntu-latest
    needs:
    - set-image-name
    - changed
    if: fromJSON(needs.changed.outputs.base) || contains(needs.changed.outputs.files, '$bench/')
    steps:
    - name: Checkout
      uses: actions/checkout@v2

    - name: Build $bench
      run: ./build.sh $bench
      env:
        GRPC_IMAGE_NAME: \${{ needs.set-image-name.outputs.name }}

    - name: Benchmark $bench
      run: ./bench.sh $bench
      env:
        GRPC_BENCHMARK_DURATION: 30s
        GRPC_IMAGE_NAME: \${{ needs.set-image-name.outputs.name }}

    - if: github.ref == 'refs/heads/master'
      name: Log in to GitHub Container Registry
      uses: docker/login-action@v1
      with:
        registry: ghcr.io
        username: \${{ github.actor }}
        password: \${{ secrets.GITHUB_TOKEN }}

    - if: github.ref == 'refs/heads/master'
      name: If on master push image to GHCR
      run: docker push \$GRPC_IMAGE_NAME:$bench-$GRPC_REQUEST_SCENARIO
      env:
        GRPC_IMAGE_NAME: \${{ needs.set-image-name.outputs.name }}

EOF

done < <(find . -maxdepth 1 -type d -name '*_bench' | sort)
