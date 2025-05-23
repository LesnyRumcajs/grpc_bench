// The usage of pajamax is very simplar to tonic.
// You may compare this to tonic's [hellowold server]
// (https://github.com/hyperium/tonic/blob/master/examples/src/helloworld/server.rs)
// example.

use pajamax::status::Status;

use helloworld::{Greeter, GreeterServer};
use helloworld::{HelloReply, HelloRequest};

// import the generated code from .proto
mod helloworld {
    pajamax::include_proto!("helloworld");
}

// define your business context
struct MyGreeter();

// `Greeter` trait defines all methods in gRPC server
impl Greeter for MyGreeter {
    // there are 3 difference compared to tonic's method handler:
    // - `fn` but not `async fn`
    // - `HelloRequest` but not `Request<HelloRequest>`
    // - `HelloReply` but not `Response<HelloReply>`
    fn say_hello(&self, req: HelloRequest) -> Result<HelloReply, Status> {
        let reply = HelloReply {
            response: req.request,
        };
        Ok(reply)
    }
}

fn main() {
    let addr = "0.0.0.0:50051";
    let greeter = MyGreeter();

    println!("GreeterServer listening on {}", addr);

    // start the server
    // By now we have not support configurations and multiple service,
    // so this API is simpler than tonic's.
    pajamax::serve(GreeterServer::new(greeter), addr).unwrap();
}
