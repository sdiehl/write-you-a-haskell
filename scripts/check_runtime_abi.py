#!/usr/bin/env python3
"""Link a real C consumer against the static runtime using rustc's reported flags."""
import json
import os
import pathlib
import platform
import shlex
import subprocess
import sys
import tempfile

root = pathlib.Path(__file__).resolve().parents[1]
search = []
if os.environ.get("GC_LIB_DIR"):
    search = ["-L" + os.environ["GC_LIB_DIR"]]
elif platform.system() == "Darwin":
    for directory in ["/opt/homebrew/lib", "/usr/local/lib"]:
        if pathlib.Path(directory, "libgc.dylib").exists():
            search = ["-L" + directory]
            break
metadata = subprocess.check_output(["cargo", "metadata", "--no-deps", "--format-version=1"], cwd=root, text=True)
for chapter in (20, 21, 22, 23, 24):
    subprocess.run([sys.executable, "scripts/build_runtime.py", str(chapter)], cwd=root, check=True)
    flags = shlex.split((root / f"build/native-static-libs-{chapter}.txt").read_text())
    archive = pathlib.Path(json.loads(metadata)["target_directory"]) / f"debug/libfun_rt_{chapter}.a"
    runtime = next((root / "crates").glob(f"ch{chapter}_*")) / "rt"
    with tempfile.TemporaryDirectory(prefix="fun-abi-") as directory:
        binary = pathlib.Path(directory) / "smoke"
        subprocess.run([
            os.environ.get("CC", "cc"), "-Wall", "-Wextra", "-Werror",
            "-I" + str(runtime / "include"), str(runtime / "tests/smoke.c"),
            str(archive), *search, *flags, "-o", str(binary),
        ], cwd=root, check=True)
        result = subprocess.run([str(binary)], capture_output=True, check=True)
        expected_stderr = b"42\n" if chapter >= 23 else b""
        assert result.stdout == b"42" and result.stderr == expected_stderr, result
    print(f"C ABI smoke: runtime {chapter} linked and printed 42")
