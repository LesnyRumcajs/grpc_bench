use pb_rs::{types::FileDescriptor, ConfigBuilder};
use std::{
    fs::{remove_dir_all, DirBuilder},
    path::Path,
};

fn main() {
    let cmd = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let in_dir = Path::new(&cmd).join("protos");
    let out_dir = Path::new(&std::env::var("OUT_DIR").unwrap()).join("protos");
    if out_dir.exists() {
        remove_dir_all(&out_dir).unwrap();
    }
    DirBuilder::new().create(&out_dir).unwrap();
    FileDescriptor::run(
        &ConfigBuilder::new(
            &[Path::new(&cmd)
                .join("protos/helloworld/helloworld.proto")
                .as_path()],
            None,
            Some(&out_dir.as_path()),
            &[in_dir.as_path()],
        )
        .unwrap()
        .build(),
    )
    .unwrap();
}
