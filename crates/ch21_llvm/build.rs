fn main() {
    lalrpop::process_root().expect("Fun grammar should be valid");
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../build/native-static-libs-21.txt");
    println!("cargo:rerun-if-changed={}", path.display());
    let flags = std::fs::read_to_string(&path).expect(
        "build the runtime first: python3 scripts/build_runtime.py 21 (or just build)",
    );
    println!("cargo:rustc-env=FUN_NATIVE_STATIC_LIBS={}", flags.trim());
}
