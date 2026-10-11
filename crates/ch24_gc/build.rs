fn main() {
    let copying = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../build/native-static-libs-24-copying.txt");
    println!("cargo:rerun-if-changed={}", copying.display());
    let flags = std::fs::read_to_string(copying)
        .expect("run python3 scripts/build_runtime.py 24 first");
    println!("cargo:rustc-env=FUN_COPYING_STATIC_LIBS={}", flags.trim());
    lalrpop::process_root().expect("Fun grammar should be valid");
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../build/native-static-libs-24.txt");
    println!("cargo:rerun-if-changed={}", path.display());
    let flags = std::fs::read_to_string(&path).expect(
        "build the runtime first: python3 scripts/build_runtime.py 24 (or just build)",
    );
    println!("cargo:rustc-env=FUN_NATIVE_STATIC_LIBS={}", flags.trim());
}
