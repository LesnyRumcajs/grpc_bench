pub mod grpc_bindings;

use wtx::{
    data_transformation::dnsn::QuickProtobuf,
    grpc::{GrpcStatusCode, Server, ServerData},
    http::{
        server_framework::{post, Router},
        ReqResBuffer, Request, StatusCode,
    },
};

#[tokio::main]
async fn main() -> wtx::Result<()> {
    let router = Router::paths(wtx::paths!((
        "/helloworld.Greeter/SayHello",
        post(say_hello)
    ),));
    Server::new(router).listen("0.0.0.0:50051", |_| {}).await
}

async fn say_hello(
    _: &mut Request<ReqResBuffer>,
    _: ServerData<QuickProtobuf>,
) -> wtx::Result<(StatusCode, GrpcStatusCode)> {
    Ok((StatusCode::Ok, GrpcStatusCode::Ok))
}
