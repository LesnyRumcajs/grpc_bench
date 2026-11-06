use std::convert::Infallible;

use ntex::io::IoConfig;
use ntex::util::BytePageSize;
use ntex::{SharedCfg, server::Server, service::ServiceFactory};
use ntex_grpc::server;

mod helloworld;
use helloworld::{HelloReply, HelloRequest};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(Clone)]
pub struct GreeterServer;

#[server(crate::helloworld::Greeter)]
impl GreeterServer {
    #[method(SayHello)]
    async fn say_hello(&self, req: HelloRequest) -> HelloReply {
        HelloReply {
            response: req.request,
        }
    }
}

impl ServiceFactory<(), server::ServerRequest> for GreeterServer {
    type Res = server::ServerResponse;
    type Error = server::ServerError;
    type Service = GreeterServer;
    type InitError = Infallible;

    async fn create(&self, (): &()) -> Result<Self::Service, Self::InitError> {
        Ok(GreeterServer)
    }
}

#[ntex::main]
async fn main() -> std::io::Result<()> {
    let workers = std::env::var("GRPC_SERVER_CPUS")
        .map(|v| v.parse().unwrap())
        .unwrap_or(1);
    let addr = "0.0.0.0:50051";

    println!("Running with {workers} workers");
    println!("GreeterServer listening on {addr}");

    let cfg = SharedCfg::new("GRPC").add(
        IoConfig::new()
            .set_read_size(BytePageSize::Size8, BytePageSize::Size64)
            .set_write_size(BytePageSize::Size32)
            .set_write_backpressure(256 * 1024),
    );

    Server::builder()
        .bind("grpc", addr, cfg, async |_| {
            server::GrpcServer::new(GreeterServer)
        })?
        .workers(workers)
        .run()
        .await
}
