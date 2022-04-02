use dotenv::dotenv;
use log::info;
use std::env;
use thruster::{
    async_middleware, context::hyper_request::HyperRequest, middleware_fn, App, MiddlewareNext,
    MiddlewareResult, ThrusterServer,
};
use thruster_grpc::{
    context::{generate_context, ProtoContext as Ctx},
    server::ProtoServer,
    util::{context_to_message, message_to_context},
};

#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

mod hello_world {
    include!(concat!(env!("OUT_DIR"), "/helloworld.rs"));
}

#[middleware_fn]
pub async fn say_hello(mut context: Ctx, _next: MiddlewareNext<Ctx>) -> MiddlewareResult<Ctx> {
    let hello_world_request = context_to_message::<hello_world::HelloRequest>(&mut context)
        .await
        .unwrap();

    Ok(message_to_context(
        context,
        hello_world::HelloReply {
            response: hello_world_request.request,
        },
    )
    .await)
}

fn main() {
    let _ = dotenv();

    env_logger::init();

    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "50051".to_string());

    info!("Starting server at {}:{}!", host, port);

    let app = App::<HyperRequest, Ctx, ()>::create(generate_context, ()).post(
        "/helloworld.Greeter/SayHello",
        async_middleware!(Ctx, [say_hello]),
    );

    let cpus = std::env::var("GRPC_SERVER_CPUS")
        .map(|v| v.parse().unwrap())
        .unwrap_or(1);

    info!("Running with {} threads", cpus);

    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(cpus)
        .enable_all()
        .build()
        .unwrap()
        .block_on(ProtoServer::new(app).build(&host, port.parse::<u16>().unwrap()))
}
