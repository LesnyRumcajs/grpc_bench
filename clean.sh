#!/bin/sh

docker rmi \
    infoblox/ghz:0.0.1 \
    rust:1.44.1-stretch \
    rust_tonic_test \
    golang:1.14 \
    go_grpc_test \
    gcc:10 \
    cpp_grpc_test \
    ruby:2.7-buster \
    ruby_grpc_test \
    python:3-slim \
    python_grpc_test \
    hseeberger/scala-sbt:11.0.7_1.3.13_2.11.12 \
    scala_akka_test \
    openjdk:8-jdk \
    java_grpc_test \
    kotlin_grpc_test \
    google/dart \
    dart_grpc_test \
    java_micronaut_test
