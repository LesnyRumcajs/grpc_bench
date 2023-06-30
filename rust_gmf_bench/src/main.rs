use gmf::server::gmf_server::GmfServer;
use tonic::{Request, Response, Status};

use hello_world::greeter_server::{Greeter, GreeterServer};
use hello_world::{HelloReply, HelloRequest};
use tower::Service;

pub mod hello_world {
    tonic::include_proto!("helloworld");
}

#[derive(Default)]
pub struct MyGreeter {}

#[tonic::async_trait]
impl Greeter for MyGreeter {
    async fn say_hello(
        &self,
        request: Request<HelloRequest>,
    ) -> Result<Response<HelloReply>, Status> {
        let reply = hello_world::HelloReply {
            response: request.into_inner().request,
        };
        Ok(Response::new(reply))
    }
}

#[cfg(target_os = "linux")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "0.0.0.0:50051".parse().unwrap();
    let greeter = MyGreeter::default();

    let tonic: GreeterServer<MyGreeter> = GreeterServer::new(greeter);
    use hyper::service::service_fn;
    let gmf = GmfServer::new(
        service_fn(move |req| {
            let mut tonic = tonic.clone();
            tonic.call(req)
        }),
        // The maximum number of concurrent connections per each physical core.
        //
        // This number is not a hard requirement, but more of an initial estimate.
        // The actual maximum may need to be tuned based on your server's
        // resource availability and expected load. The choice of `10240` here
        // was largely arbitrary, chosen based on basic benchmarks to keep the
        // memory usage below 512MB.
        10240,
    );

    // Run the gRPC server on the provided address
    gmf.serve(addr).unwrap_or_else(|e| panic!("failed {e}"));

    Ok(())
}
