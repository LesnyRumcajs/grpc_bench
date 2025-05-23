fn main() -> Result<(), Box<dyn std::error::Error>> {
    pajamax_build::compile_protos(&["proto/helloworld/helloworld.proto"], &["."])?;
    Ok(())
}
