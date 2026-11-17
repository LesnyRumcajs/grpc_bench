import GRPCCore
import HelloWorldModel

struct GreeterService: Helloworld_Greeter.SimpleServiceProtocol {
  func sayHello(
    request: Helloworld_HelloRequest,
    context: ServerContext
  ) async throws -> Helloworld_HelloReply {
    return Helloworld_HelloReply.with {
      $0.response = request.request
    }
  }
}
