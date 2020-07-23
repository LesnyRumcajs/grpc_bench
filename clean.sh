#!/bin/sh

docker rmi \
    infoblox/ghz:0.0.1 \
    rust:1.44.1-stretch \
    rust_tonic_test \
    rust_tonic_st_test \
    rust_thruster_test \
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
    crystal_grpc_test \
    crystallang/crystal:0.35.1 \
    java_grpc_test \
    kotlin_grpc_test \
    google/dart \
    dart_grpc_test \
    java_micronaut_test \
    swift:5.2 \
    swift_grpc_test \
    python:3.9-rc \
    lua_grpc_test \
    node:stretch-slim \
    node_grpc_test \
    php:zts-buster \
    php_grpc_test \
    mcr.microsoft.com/dotnet/core/sdk:3.1 \
    csharp_grpc_test \
    elixir:slim \
    elixir_grpc_test \
    oracle/graalvm-ce:20.1.0-java11-ol8 \
    java_aot_test
