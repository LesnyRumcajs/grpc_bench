#!/bin/sh

SCENARIO=$1
COPY_PAYLOAD=$2

rm -rf proto
mkdir -p proto/helloworld
cp scenarios/"${SCENARIO}"/helloworld.proto proto/helloworld

if ${COPY_PAYLOAD}; then
	rm -rf payload
	mkdir -p payload
	cp scenarios/"${SCENARIO}"/payload payload/payload
fi
