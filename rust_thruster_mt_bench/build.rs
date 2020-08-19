fn main() {
    prost_build::compile_protos(&["proto/helloworld/helloworld.proto"], &["proto/"]).unwrap();
}
