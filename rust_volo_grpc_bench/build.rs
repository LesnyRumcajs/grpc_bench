fn main() {
    volo_build::Builder::protobuf()
        .include_dirs(vec!["proto/helloworld".into()])
        .add_service("proto/helloworld/helloworld.proto")
        .write()
        .unwrap();
}
