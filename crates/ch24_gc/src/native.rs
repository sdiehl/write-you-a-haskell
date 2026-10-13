//! Host linking is an explicit process boundary: no shell expansion or guessed flags.
use crate::{flat, llvm::Backend};
use inkwell::context::Context;
use std::{
    path::{Path, PathBuf},
    process::Command,
};

/// Runtime archive beside the compiler, or an explicit FUN_RT installation override.
pub fn runtime() -> Result<PathBuf, String> {
    let path = if let Some(path) = std::env::var_os("FUN_RT") {
        PathBuf::from(path)
    } else {
        std::env::current_exe()
            .map_err(|e| e.to_string())?
            .parent()
            .unwrap()
            .join("libfun_rt_24.a")
    };
    if !path.is_file() {
        return Err(format!(
            "runtime archive {} is missing; run just build or set FUN_RT",
            path.display()
        ));
    }
    Ok(path)
}

/// Emit verified host code to a temporary object, then link against fun-rt and Boehm.
pub fn build(
    program: &flat::Program,
    output: &Path,
    opt: u8,
) -> Result<(), String> {
    build_with_gc(program, output, opt, false)
}

/// Select precise roots when linking a copying-collector executable.
pub fn build_with_gc(
    program: &flat::Program,
    output: &Path,
    opt: u8,
    copying: bool,
) -> Result<(), String> {
    let context = Context::create();
    let backend = if copying {
        Backend::with_copying(&context, program, opt)
    } else {
        Backend::new(&context, program, opt)
    }
    .map_err(|e| e.to_string())?;
    backend.optimize(opt).map_err(|e| e.to_string())?;
    let archive = if copying {
        let path = match std::env::var_os("FUN_RT_COPYING") {
            Some(path) => PathBuf::from(path),
            None => runtime()?.with_file_name("libfun_rt_24_copying.a"),
        };
        if !path.is_file() {
            return Err(format!(
                "copying runtime {} is missing; run just build",
                path.display()
            ));
        }
        path
    } else {
        runtime()?
    };
    let parent = output
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    let mut scratch = None;
    for n in 0..1000 {
        let dir =
            parent.join(format!(".fun-link-{}-{n}", std::process::id()));
        match std::fs::create_dir(&dir) {
            Ok(()) => {
                scratch = Some(dir);
                break;
            }
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(e) => return Err(e.to_string()),
        }
    }
    let scratch =
        Scratch(scratch.ok_or("cannot create temporary object directory")?);
    let object = scratch.0.join("program.o");
    backend.object(&object).map_err(|e| e.to_string())?;
    let mut command =
        Command::new(std::env::var_os("CC").unwrap_or_else(|| "cc".into()));
    command.arg(&object).arg(archive);
    if let Some(dir) = std::env::var_os("GC_LIB_DIR") {
        command.arg("-L").arg(dir);
    } else if cfg!(target_os = "macos") {
        for dir in ["/opt/homebrew/lib", "/usr/local/lib"] {
            if Path::new(dir).join("libgc.dylib").exists() {
                command.arg("-L").arg(dir);
                break;
            }
        }
    }
    let executable = scratch.0.join("program");
    command
        .args(
            if copying {
                env!("FUN_COPYING_STATIC_LIBS")
            } else {
                env!("FUN_NATIVE_STATIC_LIBS")
            }
            .split_whitespace(),
        )
        .arg("-o")
        .arg(&executable);
    let result = command
        .output()
        .map_err(|e| format!("could not run system linker: {e}"))?;
    if !result.status.success() {
        return Err(format!(
            "native link failed: {}",
            String::from_utf8_lossy(&result.stderr)
        ));
    }
    std::fs::rename(executable, output)
        .map_err(|e| format!("cannot install executable: {e}"))
}

struct Scratch(PathBuf);

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
