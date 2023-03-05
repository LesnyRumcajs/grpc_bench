// Copyright (c) 2018, the gRPC project authors. Please see the AUTHORS file
// for details. All rights reserved.
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

/// Dart implementation of the gRPC helloworld.Greeter server.
import 'dart:io';
import 'dart:isolate';

import 'package:grpc/grpc.dart';

import 'package:helloworld/src/generated/helloworld.pb.dart';
import 'package:helloworld/src/generated/helloworld.pbgrpc.dart';

class GreeterService extends GreeterServiceBase {
  @override
  Future<HelloReply> sayHello(ServiceCall call, HelloRequest request) async {
    return HelloReply()..response = request.request;
  }
}

Future<void> main(List<String> args) async {
  Map<String, String> env = Platform.environment;
  final cpus = int.tryParse(env["GRPC_SERVER_CPUS"] ?? '1') ?? 1;

  if (cpus > 1) {
    for (var serve = 0; serve < cpus; serve++) {
      Isolate.spawn(_startServer, []);
    }
  }
  // Bind one server in current Isolate
  _startServer();

  print('Server listening on port 50051...');
  await ProcessSignal.sigterm.watch().first;
}

void _startServer([List? args]) async {
  final server = Server([GreeterService()]);
  await server.serve(
      address: InternetAddress.anyIPv4, port: 50051, shared: true);
}
