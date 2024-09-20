pub mod grpc_bindings;

use wtx::{
    data_transformation::dnsn::QuickProtobuf,
    grpc::{GrpcManager, GrpcStatusCode},
    http::{
        server_framework::{post, Router, ServerFrameworkBuilder, State},
        ReqResBuffer, StatusCode,
    },
};

fn main() -> wtx::Result<()> {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(std::env::var("GRPC_SERVER_CPUS")?.parse()?)
        .enable_all()
        .build()?
        .block_on(async move {
            let router = Router::paths(wtx::paths!((
                "/helloworld.Greeter/SayHello",
                post(say_hello)
            )))?;
            ServerFrameworkBuilder::new(router)
                .with_req_aux(|| QuickProtobuf::default())
                .listen("0.0.0.0:50051", |_| {})
                .await
        })
}

async fn say_hello(
    state: State<'_, (), GrpcManager<QuickProtobuf>, ReqResBuffer>,
) -> wtx::Result<StatusCode> {
    *state.ra.status_code_mut() = GrpcStatusCode::Ok;
    Ok(StatusCode::Ok)
}
