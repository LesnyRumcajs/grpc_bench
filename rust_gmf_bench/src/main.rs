use gmf::server::gmf_server::GmfServer;
use std::sync::Arc;
use tonic::{Request, Response, Status};

use glommio::Placement;
use hello_world::greeter_server::{Greeter, GreeterServer};
use hello_world::{HelloReply, HelloRequest};

#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

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
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "0.0.0.0:50051".parse().unwrap();
    let greeter = MyGreeter::default();

    let tonic: GreeterServer<MyGreeter> = GreeterServer::new(greeter);
    use hyper::service::service_fn;
    let gmf = GmfServer::new(
        service_fn(move |req| {
            let mut tonic = tonic.clone();
            tonic.call(req)
        }),
        10240,
        // Specifies a policy by which Executor selects CPUs.
        Placement::Fixed(0),
    );

    let sender = Arc::clone(&gmf.signal_tx);

    ctrlc_async::set_async_handler(async move {
        info!("Received Ctrl-C, shutting down");
        sender.try_send(()).unwrap_or_else(|_| {
            error!("Failed to send termination signal.");
        });
    })
    .expect("Error setting Ctrl-C handler");

    // Run the gRPC server on the provided address
    gmf.serve(addr).unwrap_or_else(|e| panic!("failed {}", e));

    Ok(())
}
