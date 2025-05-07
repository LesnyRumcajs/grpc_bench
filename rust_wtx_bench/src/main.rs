pub mod grpc_bindings;

use wtx::{
    data_transformation::dnsn::QuickProtobuf,
    grpc::{GrpcManager, GrpcMiddleware},
    http::{
        server_framework::{post, Router, ServerFrameworkBuilder, State},
        ReqResBuffer, StatusCode,
    },
    misc::{simple_seed, Xorshift64},
};

fn main() -> wtx::Result<()> {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(std::env::var("GRPC_SERVER_CPUS")?.parse()?)
        .enable_all()
        .build()?
        .block_on(async move {
            let router = Router::new(
                wtx::paths!(("/helloworld.Greeter/SayHello", post(say_hello))),
                GrpcMiddleware,
            )?;
            ServerFrameworkBuilder::new(router)
                .with_req_aux(|| QuickProtobuf::default())
                .tokio(
                    "0.0.0.0:50051",
                    Xorshift64::from(simple_seed()),
                    |_| {},
                    |_| Ok(()),
                )
                .await
        })
}

async fn say_hello(
    _: State<'_, (), GrpcManager<QuickProtobuf>, ReqResBuffer>,
) -> wtx::Result<StatusCode> {
    Ok(StatusCode::Ok)
}
