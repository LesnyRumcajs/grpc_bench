import Foundation
import GRPCCore
import GRPCNIOTransportHTTP2
import NIOPosix

let numberOfThreads = ProcessInfo.processInfo.environment["GRPC_SERVER_CPUS"].flatMap(Int.init) ?? 1
let group = MultiThreadedEventLoopGroup(numberOfThreads: numberOfThreads)

let server = GRPCServer(
  transport: .http2NIOPosix(
    address: .ipv4(host: "0.0.0.0", port: 50051),
    transportSecurity: .plaintext,
    eventLoopGroup: group
  ),
  services: [GreeterService()]
)

print("server started on port 50051")
try await server.serve()
