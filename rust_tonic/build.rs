fn main() {
    tonic_build::compile_protos("proto/helloworld/helloworld.proto").unwrap();
}
