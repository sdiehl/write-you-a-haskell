#!/usr/bin/env python3
"""Exercise source depth with the unoptimized compiler, outside Cargo's test profile."""
import json
import os
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[1]
env = dict(os.environ, CARGO_PROFILE_DEV_OPT_LEVEL="0")
subprocess.run(["cargo", "build", "--locked", "-p", "ch24_gc", "--bin", "fun"],
               cwd=root, env=env, check=True)
metadata = json.loads(subprocess.check_output(
    ["cargo", "metadata", "--no-deps", "--format-version=1"], cwd=root, env=env))
binary = Path(metadata["target_directory"]) / "debug/fun"
with tempfile.TemporaryDirectory(prefix="fun-depth-") as directory:
    directory = Path(directory)
    source = directory / "deep.fun"
    source.write_text("main = print (length [" + ",".join(["1"] * 5000) + "])\n")
    commands = [["run", "--engine=" + engine] for engine in ["core", "anf", "flat"]]
    commands += [["dump", "--pass=" + name] for name in ["erase", "anf", "flat", "llvm"]]
    for command in commands:
        output = directory / "output"
        with output.open("wb") as stream:
            result = subprocess.run([str(binary), *command, "--lint", str(source)],
                                    cwd=root, stdout=stream, stderr=subprocess.PIPE, timeout=60)
        if result.returncode or result.stderr:
            raise RuntimeError(f"{command}: exit {result.returncode}: {result.stderr.decode(errors='replace')}")
        if command[0] == "run" and output.read_bytes() != b"5000\n":
            raise RuntimeError(f"{command}: wrong result")
        if not 0 < output.stat().st_size < 12_000_000:
            raise RuntimeError(f"{command}: empty or unexpectedly large output")
        print("Unoptimized depth check:", " ".join(command))
