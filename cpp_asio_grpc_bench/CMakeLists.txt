cmake_minimum_required(VERSION 3.15)

project(
  cpp_asio_grpc_bench
  DESCRIPTION "Benchmark for asio-grpc"
  HOMEPAGE_URL https://github.com/Tradias/asio-grpc
  LANGUAGES CXX)

include(FindPkgConfig)

pkg_check_modules(grpc++_unsecure REQUIRED IMPORTED_TARGET grpc++_unsecure)
pkg_check_modules(protobuf REQUIRED IMPORTED_TARGET protobuf)
find_package(Boost REQUIRED COMPONENTS coroutine)

add_executable(${PROJECT_NAME})

target_sources(
  ${PROJECT_NAME}
  PRIVATE main.cpp "${CMAKE_CURRENT_LIST_DIR}/gen/helloworld.pb.cc"
          "${CMAKE_CURRENT_LIST_DIR}/gen/helloworld.grpc.pb.cc")

target_link_libraries(
  ${PROJECT_NAME} PRIVATE PkgConfig::grpc++_unsecure PkgConfig::protobuf
                          Boost::headers Boost::coroutine)

target_include_directories(${PROJECT_NAME}
                           PRIVATE "${CMAKE_CURRENT_LIST_DIR}/gen")

target_compile_options(${PROJECT_NAME} PRIVATE -flto)

target_compile_definitions(${PROJECT_NAME} PRIVATE BOOST_ASIO_NO_DEPRECATED)

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)

install(TARGETS ${PROJECT_NAME})
