use tonic::{transport::Server, Request, Response, Status};

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cpus = std::env::var("GRPC_SERVER_CPUS")
        .map(|v| v.parse().unwrap())
        .unwrap_or(1);

    println!("Running with {} threads", cpus);

    // Esentially the same as tokio::main, but with number of threads set to
    // avoid thrashing when cggroup limits are applied by Docker.
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(cpus)
        .enable_all()
        .build()
        .unwrap()
        .block_on(serve())
}

async fn serve() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "0.0.0.0:50051".parse().unwrap();
    let greeter = MyGreeter::default();

    println!("GreeterServer listening on {}", addr);

    Server::builder()
        .add_service(GreeterServer::new(greeter))
        .serve(addr)
        .await?;

    Ok(())
}
