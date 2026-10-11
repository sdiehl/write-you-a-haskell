#!/usr/bin/env python3
"""Add presentation-only blank lines between top-level Fun declarations."""
import json
import re
import sys

SYMBOLS = set('!#$%&*+./<=>?@\\^|~:-')


def space_code(source):
    lines = source.splitlines(keepends=True)
    result = []
    previous = None
    depth = 0
    comment_depth = 0
    quote = None
    escaped = False
    for number, line in enumerate(lines):
        top = depth == 0 and comment_depth == 0 and quote is None
        key = None
        if top and line and not line[0].isspace():
            if re.match(r'(data|type|class|instance)\b', line):
                key = ('declaration', number)
            elif re.match(r'infix[lr]?\b', line):
                key = ('fixities',)
            else:
                binding = re.match(r"([a-z_][A-Za-z0-9_']*|\([^()\s]+\))(?=[\s,:=|])", line)
                if binding and binding[1] not in {'let', 'if', 'case', 'do', 'in', 'then', 'else'}:
                    rest = line[binding.end():]
                    following = next((s.lstrip() for s in lines[number + 1:] if s.strip()), '')
                    if re.search(r'::|(?<![<>=/])=(?!=)|\|', rest) or following.startswith('|'):
                        key = ('binding', binding[1])
            if key is not None:
                if previous is not None and key != previous and result and result[-1].strip():
                    # Leave immediately preceding line comments attached to the declaration.
                    start = len(result)
                    while start and result[start - 1].startswith('--'):
                        start -= 1
                    if start and result[start - 1].strip():
                        result.insert(start, '\n')
                previous = key
        result.append(line)
        # Track delimiters and literals so an explicit block or multiline string
        # cannot be mistaken for a fresh top-level declaration.
        i = 0
        while i < len(line):
            pair = line[i:i + 2]
            c = line[i]
            if quote:
                if escaped:
                    escaped = False
                elif c == '\\':
                    escaped = True
                elif c == quote:
                    quote = None
            elif comment_depth:
                if pair == '{-':
                    comment_depth += 1
                    i += 1
                elif pair == '-}':
                    comment_depth -= 1
                    i += 1
            elif pair == '{-':
                comment_depth = 1
                i += 1
            elif pair == '--' and (i + 2 == len(line) or line[i + 2] not in SYMBOLS):
                break
            elif c == '"' or (c == "'" and (i == 0 or not (line[i - 1].isalnum() or line[i - 1] in "_'"))):
                quote = c
            elif c in '([{':
                depth += 1
            elif c in ')]}':
                depth -= 1
            i += 1
    return ''.join(result)


def format_markdown(text):
    def replace(match):
        return match[1] + space_code(match[2]) + match[3]
    return re.sub(r'(^```fun[^\n]*\n)(.*?)(^```[ \t]*$)', replace, text, flags=re.M | re.S)


def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == 'supports':
            return
        if sys.argv[1] == '--check':
            # Signatures and equations stay together. Strings and nested blocks
            # keep their exact bytes, and formatting twice changes nothing.
            source = 'f :: Int -> Int\nf 0 = 0\nf n = n\n-- entry\nmain = do\n  let x = 1\n  print x\n'
            expected = source.replace('f n = n\n-- entry', 'f n = n\n\n-- entry')
            assert space_code(source) == expected
            assert space_code(expected) == expected
            assert space_code("f=1\ng=2\n") == "f=1\n\ng=2\n"
            for source in ['{ f = 1;\ng = 2 }\n', 'main = "first\nf = 3\nlast"\n', "f' x = x\nf' 0 = 0\n"]:
                assert space_code(source) == source
            print('Fun example spacing checked.')
            return
        raise SystemExit('usage: fun-spacing.py [supports RENDERER | --check]')
    _, book = json.load(sys.stdin)
    pending = list(book.get('items', book.get('sections', [])))
    while pending:
        item = pending.pop()
        if 'Chapter' in item:
            chapter = item['Chapter']
            chapter['content'] = format_markdown(chapter['content'])
            pending.extend(chapter['sub_items'])
    json.dump(book, sys.stdout)


if __name__ == '__main__':
    main()
