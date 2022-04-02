mod proto;

use std::io::Read;
use std::sync::Arc;
use std::{io, thread};

use futures::channel::oneshot;
use futures::executor::block_on;
use futures::prelude::*;
use grpcio::{ChannelBuilder, Environment, RpcContext, ServerBuilder, UnarySink};

use crate::proto::gen::helloworld::{HelloReply, HelloRequest};
use crate::proto::gen::helloworld_grpc::{create_greeter, Greeter};

#[global_allocator]
static GLOBAL: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Clone)]
struct GreeterService;

impl Greeter for GreeterService {
    fn say_hello(&mut self, ctx: RpcContext<'_>, mut req: HelloRequest, sink: UnarySink<HelloReply>) {
        let mut resp = HelloReply::default();
        resp.set_response(req.take_request());
        let f = sink
            .success(resp)
            .map_err(move |e| println!("failed to reply {:?}: {:?}", req, e))
            .map(|_| ());
        ctx.spawn(f)
    }
}

fn main() {
    let cpus = std::env::var("GRPC_SERVER_CPUS")
        .unwrap_or("1".to_string())
        .parse()
        .unwrap();
    let env = Arc::new(Environment::new(cpus));
    let service = create_greeter(GreeterService);
    let ch_builder = ChannelBuilder::new(env.clone());

    let mut server = ServerBuilder::new(env)
        .register_service(service)
        .bind("0.0.0.0", 50_051)
        .channel_args(ch_builder.build_args())
        .build()
        .unwrap();
    server.start();
    for (host, port) in server.bind_addrs() {
        println!("listening on {}:{} ({} cpus)", host, port, cpus);
    }
    let (tx, rx) = oneshot::channel();
    thread::spawn(move || {
        println!("Press ENTER to exit...");
        let _ = io::stdin().read(&mut [0]).unwrap();
        tx.send(())
    });
    let _ = block_on(rx);
    let _ = block_on(server.shutdown());
}
