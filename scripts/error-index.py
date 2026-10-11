#!/usr/bin/env python3
"""Generate the final diagnostic appendix from the compiler's stable registry."""
from pathlib import Path
import re
import sys
root = Path(__file__).resolve().parents[1]
source = (root / 'crates/ch24_gc/src/diagnostics/codes.rs').read_text()
rows = re.findall(r'\("([EW]\d{4})", "([^"]+)"\)', source)
if not rows or len({code for code, _ in rows}) != len(rows):
    raise SystemExit("invalid or duplicate error registry")
text = '''# Appendix E: Diagnostic Index

This table is generated from the final compiler's error registry. Codes are
stable identifiers. Messages and secondary labels add context about the source
program. The primary label points to the location that triggered the diagnostic.

| Code | Meaning |
|:---|:---|
'''
text += ''.join(f'| `{code}` | {description[0].upper() + description[1:]} |\n' for code, description in rows)
text += '''
Lexing, layout, and parsing use `E00xx`. Renaming uses `E01xx`, typing uses
`E02xx`, and kind checking uses `E03xx`. A pass may report several diagnostics,
but the driver does not run later passes after an error.

`W0001` reports a possibly missing match and includes witness patterns when
available. `W0002` identifies an unreachable clause. Warnings do not reject a
program. A missing match that execution actually reaches raises a runtime error.

Runtime errors have messages rather than compiler codes. They begin with `fun:`
and exit with status 1. `exitWith` uses the requested status. Compiler dumps and
internal lint failures are developer tools, not an alternative diagnostic API.

For an ambiguous record field, annotate the receiver's type. For an ambiguous
class constraint, make the constrained type visible in the result or select it
explicitly. Fun does not default unresolved numeric constraints.
'''
path = root / 'book/src/appendix-e-diagnostics.md'
if '--check' in sys.argv:
    def canonical(s):
        lines = []
        for line in s.splitlines():
            if line.startswith('|'):
                cells = [re.sub(r'-{3,}', '---', c.strip()) for c in line.split('|')]
                line = '|'.join(cells)
            lines.append(line.rstrip())
        return '\n'.join(lines)
    if not (canonical(path.read_text()) == canonical(text)):
        raise SystemExit('Run python3 scripts/error-index.py')
else:
    path.write_text(text)
