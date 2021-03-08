fn main() {
    let proto_root = "src/proto/helloworld";
    let proto_out = "src/proto/gen";
    println!("cargo:rerun-if-changed={}", proto_root);
    protoc_grpcio::compile_grpc_protos(&["helloworld.proto"], &[proto_root], &proto_out, None)
        .expect("Failed to compile gRPC definitions!");
}
