set shell := ["bash", "-eu", "-o", "pipefail", "-c"]
export PATH := justfile_directory() / ".tools/bin" + ":" + env_var("PATH")

check: runtime
    cargo fmt --all -- --check
    cargo clippy --workspace --all-targets -- -D warnings
    cargo test --workspace
    python3 scripts/check_runtime_abi.py

bootstrap:
    python3 scripts/bootstrap.py

book:
    cargo xtask syntax --check
    python3 scripts/check_book.py
    mkdir -p build
    mdbook build book 2> build/mdbook.log
    cat build/mdbook.log
    ! rg -i "warn|error" build/mdbook.log

pdf: book
    cargo xtask pdf-collect
    mkdir -p build/pdf
    pandoc build/pdf/book.md -f markdown+tex_math_dollars+raw_attribute -t typst --lua-filter pdf/filters/links.lua --lua-filter pdf/filters/callouts.lua --lua-filter pdf/filters/parts.lua --lua-filter pdf/filters/code.lua -o build/pdf/body.typ 2> build/pdf/pandoc.log
    cat build/pdf/pandoc.log
    ! rg 'Could not convert TeX math|WARNING|ERROR' build/pdf/pandoc.log
    typst compile --root . --font-path pdf/fonts --ignore-system-fonts pdf/book.typ build/write-you-a-haskell.pdf

runtime:
    python3 scripts/build_runtime.py 20
    python3 scripts/build_runtime.py 21
    python3 scripts/build_runtime.py 22
    python3 scripts/build_runtime.py 23
    python3 scripts/build_runtime.py 24

build: runtime
    cargo build --workspace
