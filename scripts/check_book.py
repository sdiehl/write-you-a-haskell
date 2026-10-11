#!/usr/bin/env python3
"""Fail on unresolved book inputs and unverified citation keys before rendering."""
from pathlib import Path
import re
import subprocess
import sys
root = Path(__file__).resolve().parents[1]
keys = set(re.findall(r'@\w+\{([^,]+),', (root / 'book/refs.bib').read_text()))
for chapter in (root / 'book/src').glob('*.md'):
    if chapter.name == "SUMMARY.md":
        continue
    text = chapter.read_text()
    for key in re.findall(r'\[@([A-Za-z0-9_-]+)\]', text):
        if not (key in keys):
            raise SystemExit(f'{chapter}: unknown citation {key}')
    for include in re.findall(r'\{\{#include ([^}]+)\}\}', text):
        path, *anchor = include.split(':')
        source = (chapter.parent / path).read_text()
        if anchor:
            if not (len(anchor) == 1 and not anchor[0].isdigit()):
                raise SystemExit('Use named anchors, never line ranges')
            if not (f'// ANCHOR: {anchor[0]}' in source):
                raise SystemExit(f'{chapter}: missing anchor {include}')
            if not (f'// ANCHOR_END: {anchor[0]}' in source):
                raise SystemExit(f'{chapter}: missing end anchor {include}')
print('Chapter citations and source anchors checked.')

subprocess.run([sys.executable, str(root / "scripts/fun-spacing.py"), "--check"], check=True)

subprocess.run([sys.executable, str(root / "scripts/error-index.py"), "--check"], check=True)
