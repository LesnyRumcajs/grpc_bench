pub mod grpc_bindings;

use wtx::{
    codec::format::QuickProtobuf,
    grpc::{GrpcManager, GrpcMiddleware},
    http::{
        server_framework::{post, Router, ServerFrameworkBuilder, State},
        HttpRecvParams, MsgBufferString, StatusCode,
    },
    misc::var
};

fn main() -> wtx::Result<()> {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(var("GRPC_SERVER_CPUS")?.parse()?)
        .enable_all()
        .build()?
        .block_on(async move {
            let router = Router::new(
                wtx::paths!(("/helloworld.Greeter/SayHello", post(say_hello))),
                GrpcMiddleware,
            )?;
            ServerFrameworkBuilder::new(HttpRecvParams::with_permissive_params(), router)
                .with_stream_aux(|_| Ok(QuickProtobuf))
                .tokio("0.0.0.0:50051", |_| {}, |_| Ok(()), |_| Ok(()), |_| {})
                .await
        })
}

async fn say_hello(
    _: State<'_, (), GrpcManager<QuickProtobuf>, MsgBufferString>,
) -> wtx::Result<StatusCode> {
    Ok(StatusCode::Ok)
}
