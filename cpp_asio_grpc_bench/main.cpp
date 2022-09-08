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

#include <agrpc/asioGrpc.hpp>
#include <boost/asio/spawn.hpp>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>

#include <forward_list>
#include <iostream>
#include <thread>
#include <vector>

struct UnaryRPCContext {
  grpc::ServerContext server_context;
  helloworld::HelloRequest request;
  grpc::ServerAsyncResponseWriter<helloworld::HelloReply> writer{
      &server_context};
};

void spawn_accept_loop(
    agrpc::GrpcContext &grpc_context,
    helloworld::Greeter::AsyncService &service,
    boost::asio::executor_work_guard<agrpc::GrpcContext::executor_type>
        &guard) {
  boost::asio::spawn(grpc_context, [&](auto yield) {
    bool request_ok{true};
    while (request_ok) {
      auto context =
          std::allocate_shared<UnaryRPCContext>(grpc_context.get_allocator());
      request_ok = agrpc::request(
          &helloworld::Greeter::AsyncService::RequestSayHello, service,
          context->server_context, context->request, context->writer, yield);
      if (!request_ok) {
        guard.reset();
        return;
      }
      helloworld::HelloReply response;
      *response.mutable_response() = std::move(*context->request.mutable_request());
      auto &writer = context->writer;
      agrpc::finish(writer, response, grpc::Status::OK,
                    boost::asio::bind_executor(
                        grpc_context, [context = std::move(context)]() {}));
    }
  });
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
      auto guard = boost::asio::make_work_guard(grpc_context);
      spawn_accept_loop(grpc_context, service, guard);
      grpc_context.run();
    });
  }

  for (auto &thread : threads) {
    thread.join();
  }

  server->Shutdown();
}
