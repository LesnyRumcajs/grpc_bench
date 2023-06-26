use dotenv::dotenv;
use log::info;
use std::env;
use thruster::{
    context::hyper_request::HyperRequest, m, middleware_fn, App, MiddlewareNext, MiddlewareResult,
    ThrusterServer,
};
use thruster_grpc::{
    context::{generate_context, ProtoContext, ProtoContextExt},
    server::ProtoServer,
};

#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

mod hello_world {
    include!(concat!(env!("OUT_DIR"), "/helloworld.rs"));
}

type Ctx = ProtoContext<()>;

#[middleware_fn]
pub async fn say_hello(mut context: Ctx, _next: MiddlewareNext<Ctx>) -> MiddlewareResult<Ctx> {
    let hello_world_request = context
        .get_proto::<hello_world::HelloRequest>()
        .await
        .unwrap();

    context
        .proto(hello_world::HelloReply {
            response: hello_world_request.request,
        })
        .await;

    Ok(context)
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let _ = dotenv();

    env_logger::init();

    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let port = env::var("PORT").unwrap_or_else(|_| "50051".to_string());

    info!("Starting server at {}:{}!", host, port);

    let app = App::<HyperRequest, Ctx, ()>::create(generate_context, ())
        .post("/helloworld.Greeter/SayHello", m![say_hello]);

    ProtoServer::new(app)
        .build(&host, port.parse::<u16>().unwrap())
        .await;
}
