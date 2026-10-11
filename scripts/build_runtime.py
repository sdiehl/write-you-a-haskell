#!/usr/bin/env python3
"""Build a chapter runtime first and retain rustc's host-specific static link flags."""
import pathlib
import json
import re
import subprocess
import sys
import shutil

root = pathlib.Path(__file__).resolve().parents[1]
chapter = int(sys.argv[1]) if len(sys.argv) > 1 else 21
package = f"fun-rt-{chapter:02}"
profile_args = ["--release"] if "--release" in sys.argv[2:] else []
profile = "release" if profile_args else "debug"
if chapter == 24:
    metadata = json.loads(subprocess.check_output(["cargo", "metadata", "--no-deps", "--format-version=1"], cwd=root, text=True))
    target_root = pathlib.Path(metadata["target_directory"])
    target = target_root / "copying-runtime"
    args = ["cargo", "rustc", "-p", package, "--features", "copying-only",
            "--target-dir", str(target), *profile_args, "--", "--print", "native-static-libs"]
    precise = subprocess.run(args, cwd=root, capture_output=True, text=True, check=True)
    flags = re.findall(r"native-static-libs:\s*([^\n]+)", precise.stdout + precise.stderr)
    if not flags:
        sys.exit("missing native link flags for copying runtime")
    (root / "build").mkdir(exist_ok=True)
    (root / "build/native-static-libs-24-copying.txt").write_text(flags[-1].strip() + "\n")
    (target_root / profile).mkdir(parents=True, exist_ok=True)
    shutil.copyfile(target / profile / "libfun_rt_24.a", target_root / profile / "libfun_rt_24_copying.a")
subprocess.run(["cargo", "build", "-p", package, *profile_args], cwd=root, check=True)
result = subprocess.run(
    ["cargo", "rustc", "-p", package, *profile_args, "--", "--print", "native-static-libs"],
    cwd=root, capture_output=True, text=True, check=True,
)
report = result.stdout + result.stderr
matches = re.findall(r"native-static-libs:\s*([^\n]+)", report)
if not matches:
    sys.exit("rustc did not report native static libraries:\n" + report)
output = root / "build" / f"native-static-libs-{chapter:02}.txt"
output.parent.mkdir(exist_ok=True)
output.write_text(matches[-1].strip() + "\n")
print(f"{package}: link flags recorded in {output.relative_to(root)}")
