//! Homebrew is not normally on the system linker's search path.
fn main() {
    println!("cargo:rerun-if-env-changed=GC_LIB_DIR");
    if let Ok(path) = std::env::var("GC_LIB_DIR") {
        println!("cargo:rustc-link-search=native={path}");
    } else if std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos")
    {
        for path in ["/opt/homebrew/lib", "/usr/local/lib"] {
            if std::path::Path::new(path).join("libgc.dylib").exists() {
                println!("cargo:rustc-link-search=native={path}");
                break;
            }
        }
    }
    println!("cargo:rustc-link-lib=gc");
}
