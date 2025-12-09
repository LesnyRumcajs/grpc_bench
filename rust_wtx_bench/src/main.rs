pub mod grpc_bindings;

use wtx::{
    de::format::QuickProtobuf,
    grpc::{GrpcManager, GrpcMiddleware},
    http::{
        server_framework::{post, Router, ServerFrameworkBuilder, State},
        ReqResBuffer, StatusCode,
    },
    rng::{simple_seed, Xorshift64},
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
            ServerFrameworkBuilder::new(Xorshift64::from(simple_seed()), router)
                .with_stream_aux(|_| Ok(QuickProtobuf))
                .tokio("0.0.0.0:50051", |_| {}, |_| Ok(()), |_| {})
                .await
        })
}

async fn say_hello(
    _: State<'_, (), GrpcManager<QuickProtobuf>, ReqResBuffer>,
) -> wtx::Result<StatusCode> {
    Ok(StatusCode::Ok)
}
