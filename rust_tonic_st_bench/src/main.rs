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

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "0.0.0.0:50051".parse().unwrap();
    let greeter = MyGreeter::default();

    println!("GreeterServer listening on {}", addr);

    Server::builder()
        .add_service(GreeterServer::new(greeter))
        .serve(addr)
        .await?;

    Ok(())
}
