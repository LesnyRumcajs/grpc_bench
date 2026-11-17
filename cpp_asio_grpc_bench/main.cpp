// Copyright 2021 Dennis Hezel
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "helloworld.grpc.pb.h"

#include <agrpc/asio_grpc.hpp>
#include <agrpc/register_yield_rpc_handler.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/spawn.hpp>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>

#include <forward_list>
#include <iostream>
#include <thread>
#include <vector>

using RPC = agrpc::ServerRPC<&helloworld::Greeter::AsyncService::RequestSayHello>;

void register_handler(agrpc::GrpcContext &grpc_context,
                      helloworld::Greeter::AsyncService &service) {
  agrpc::register_yield_rpc_handler<RPC>(
      grpc_context, service,
      [](RPC &rpc, RPC::Request &request, const boost::asio::yield_context &yield) {
        RPC::Response response;
        *response.mutable_response() = std::move(*request.mutable_request());
        rpc.finish(response, grpc::Status::OK, yield);
      },
      boost::asio::detached);
}

int main() {
  std::string server_address("0.0.0.0:50051");

  grpc::ServerBuilder builder;
  std::unique_ptr<grpc::Server> server;
  helloworld::Greeter::AsyncService service;

  const auto parallelism = std::atoi(std::getenv("GRPC_SERVER_CPUS"));
  std::forward_list<agrpc::GrpcContext> grpc_contexts;
  for (int i = 0; i < parallelism; ++i) {
    grpc_contexts.emplace_front(builder.AddCompletionQueue());
  }

  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);
  server = builder.BuildAndStart();
  std::cout << "Server listening on " << server_address << std::endl;

  std::vector<std::thread> threads;
  threads.reserve(parallelism);
  for (int i = 0; i < parallelism; ++i) {
    threads.emplace_back([&, i] {
      auto &grpc_context = *std::next(grpc_contexts.begin(), i);
      register_handler(grpc_context, service);
      grpc_context.run();
    });
  }

  for (auto &thread : threads) {
    thread.join();
  }

  server->Shutdown();
}
