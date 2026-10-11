#!/usr/bin/env python3
"""Install the pinned book toolchain locally, using Python's standard library."""
from pathlib import Path
import os
import platform
import shutil
import subprocess
import tarfile
import urllib.request
import zipfile

ROOT = Path(__file__).resolve().parents[1]
TOOLS = ROOT / '.tools'
BIN = TOOLS / 'bin'
BIN.mkdir(parents=True, exist_ok=True)
os.chdir(ROOT)

def run(*args):
    subprocess.run(args, check=True)

def download(url, name):
    path = TOOLS / name
    if not path.exists():
        print('Downloading', url, flush=True)
        with urllib.request.urlopen(url, timeout=90) as source, path.open('wb') as dest:
            shutil.copyfileobj(source, dest)
    return path

def unpack(path, dest):
    if path.suffix == '.zip':
        with zipfile.ZipFile(path) as archive:
            archive.extractall(dest)
    else:
        with tarfile.open(path) as archive:
            archive.extractall(dest, filter='data')

def installed(tool, version):
    path = BIN / tool
    if not path.exists():
        return False
    result = subprocess.run([str(path), '--version'], text=True, capture_output=True)
    return result.returncode == 0 and version in result.stdout

for tool, version in [('mdbook', '0.5.3'), ('mdbook-katex', '0.10.0')]:
    if not installed(tool, version):
        run('cargo', 'install', tool, '--version', version, '--locked', '--root', str(TOOLS))

revision = '32e5190a5418367059851a2ce5055dceab79b510'
if not (TOOLS / 'include-rs-revision').exists() or (TOOLS / 'include-rs-revision').read_text() != revision:
    run('cargo', 'install', '--git', 'https://github.com/sdiehl/mdbook-include-rs',
        '--rev', revision, '--locked', '--force', '--root', str(TOOLS))
    (TOOLS / 'include-rs-revision').write_text(revision)

# Upstream 0.5.3 locks mdbook-core to 0.5.1 and omits editor fallback.
# Preserve the exact patched dependency graph in the repository.
if not (TOOLS / 'bib-patched-v1').exists() or not installed('mdbook-bib', '0.5.3'):
    archive = download('https://static.crates.io/crates/mdbook-bib/mdbook-bib-0.5.3.crate', 'mdbook-bib.crate')
    unpack(archive, TOOLS)
    source = TOOLS / 'mdbook-bib-0.5.3'
    with (source / 'Cargo.toml').open('a') as out:
        out.write('\n[workspace]\n')
    shutil.copy(ROOT / 'scripts/locks/mdbook-bib.lock', source / 'Cargo.lock')
    parser = source / 'src/parser/hayagriva_parser.rs'
    parser.write_text(parser.read_text().replace('let persons = entry.authors();',
        'let persons = entry.authors().or_else(|| entry.editors());'))
    run('cargo', 'install', '--path', str(source), '--locked', '--force', '--root', str(TOOLS))
    (TOOLS / 'bib-patched-v1').touch()

system, machine = platform.system(), platform.machine()
if system not in ['Darwin', 'Linux'] or machine not in ['arm64', 'aarch64', 'x86_64']:
    raise SystemExit('Only Linux/macOS on x86-64/AArch64 are supported.')
arm = machine in ['arm64', 'aarch64']
if not installed('pandoc', '3.12.1'):
    suffix = ('arm64' if arm else 'x86_64') + '-macOS.zip' if system == 'Darwin' else 'linux-' + ('arm64' if arm else 'amd64') + '.tar.gz'
    name = 'pandoc-3.12.1-' + suffix
    unpack(download('https://github.com/jgm/pandoc/releases/download/3.12.1/' + name, name), TOOLS / 'pandoc-dist')
    executable = next((TOOLS / 'pandoc-dist').rglob('bin/pandoc'))
    shutil.copy(executable, BIN / 'pandoc')
    (BIN / 'pandoc').chmod(0o755)
if not installed('typst', '0.15.1'):
    target = ('aarch64' if arm else 'x86_64') + ('-apple-darwin' if system == 'Darwin' else '-unknown-linux-musl')
    name = 'typst-' + target + '.tar.xz'
    unpack(download('https://github.com/typst/typst/releases/download/v0.15.1/' + name, name), TOOLS)
    shutil.copy(TOOLS / ('typst-' + target) / 'typst', BIN / 'typst')
    (BIN / 'typst').chmod(0o755)
print('Book tools are installed in .tools/bin. Use just book or just pdf.')
