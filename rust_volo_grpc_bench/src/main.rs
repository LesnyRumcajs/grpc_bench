use std::net::SocketAddr;

use volo_grpc::{
    server::{Server, ServiceBuilder},
    Request, Response, Status,
};

use volo_gen::helloworld::{Greeter, GreeterServer, HelloReply, HelloRequest};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

include!(concat!(env!("OUT_DIR"), "/volo_gen.rs"));

pub struct MyGreeter;

impl Greeter for MyGreeter {
    async fn say_hello(
        &self,
        request: Request<HelloRequest>,
    ) -> Result<Response<HelloReply>, Status> {
        let reply = HelloReply {
            response: request.into_inner().request,
        };
        Ok(Response::new(reply))
    }
}

fn main() {
    let cpus = std::env::var("GRPC_SERVER_CPUS")
        .map(|v| v.parse().unwrap())
        .unwrap_or(1);

    println!("Running with {} threads", cpus);

    // Same as #[volo::main], but with the number of worker threads set to
    // avoid thrashing when cgroup limits are applied by Docker.
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(cpus)
        .enable_all()
        .build()
        .unwrap()
        .block_on(serve())
}

async fn serve() {
    let addr: SocketAddr = "0.0.0.0:50051".parse().unwrap();
    let addr = volo::net::Address::from(addr);

    println!("GreeterServer listening on {}", addr);

    Server::new()
        .add_service(ServiceBuilder::new(GreeterServer::new(MyGreeter)).build())
        .run(addr)
        .await
        .unwrap();
}
