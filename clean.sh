#!/bin/sh

docker image remove \
    infoblox/ghz:0.0.1 \
    rust:1.44.1-stretch \
    rust_tonic \
    golang:1.14 \
    go_grpc \
    gcc:10 \
    cpp_grpc \
    ruby:2.7-buster \
    ruby_grpc \
    python:3-slim \
    python_grpc
