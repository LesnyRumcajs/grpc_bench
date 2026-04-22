fn main() {
    tonic_prost_build::compile_protos("proto/helloworld/helloworld.proto").unwrap();
}
