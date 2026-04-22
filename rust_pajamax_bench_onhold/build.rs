fn main() -> Result<(), Box<dyn std::error::Error>> {
    pajamax_build::compile_protos_in_local(&["proto/helloworld/helloworld.proto"], &["."])?;
    Ok(())
}
