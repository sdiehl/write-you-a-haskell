#!/usr/bin/env python3
"""Dedent rendered Rust fragments after both mdBook include mechanisms run.

Source files stay untouched. Multiline string literals and fences marked
`rust,keep-indent` retain their exact indentation.
"""
import json
import re
import sys
import textwrap


def has_multiline_string(source):
    """Conservatively skip fragments with multiline or incomplete strings."""
    i = 0
    while i < len(source):
        if source.startswith('//', i):
            end = source.find('\n', i)
            i = len(source) if end < 0 else end + 1
        elif source.startswith('/*', i):
            depth = 1
            i += 2
            while i < len(source) and depth:
                if source.startswith('/*', i):
                    depth += 1
                    i += 2
                elif source.startswith('*/', i):
                    depth -= 1
                    i += 2
                else:
                    i += 1
        else:
            raw = re.match(r'(?:br|cr|r)(#*)"', source[i:])
            if raw:
                start = i + raw.end()
                end = source.find('"' + raw[1], start)
                if end < 0 or '\n' in source[start:end]:
                    return True
                i = end + 1 + len(raw[1])
            elif source[i] == '"':
                start = i
                i += 1
                while i < len(source):
                    if source[i] == '\\':
                        i += 2
                    elif source[i] == '"':
                        break
                    else:
                        i += 1
                if i >= len(source) or '\n' in source[start:i]:
                    return True
                i += 1
            elif source[i] == "'":
                # Consume character literals, but leave lifetimes alone.
                char = re.match(r"'(?:\\(?:u\{[0-9a-fA-F_]+\}|x[0-9a-fA-F]{2}|.)|[^'\\\n])'", source[i:])
                i += char.end() if char else 1
            else:
                i += 1
    return False


def format_markdown(source):
    def replace(match):
        opening, fence, info, code, closing = match.groups()
        flags = re.split(r'[,\s]+', info.strip())
        if flags[0] != 'rust' or 'keep-indent' in flags:
            return match[0]
        if has_multiline_string(code):
            return match[0]
        return opening + textwrap.dedent(code) + closing

    return re.sub(
        r'(^(`{3,}|~{3,})([^\n]*)\n)(.*?)(^\2[ \t]*$)',
        replace, source, flags=re.M | re.S,
    )


def check():
    fragment = '```rust\n    fn visit() {\n        step();\n    }\n```'
    expected = '```rust\nfn visit() {\n    step();\n}\n```'
    assert format_markdown(fragment) == expected
    assert format_markdown(expected) == expected
    assert format_markdown(fragment.replace('```', '~~~~')) == expected.replace('```', '~~~~')
    for source in [fragment.replace('rust', 'text'), fragment.replace('rust', 'rust,keep-indent')]:
        assert format_markdown(source) == source
    for literal in ['"first\n    second"', 'r##"first\n    second"##', 'b"first\n    second"', '"unfinished']:
        source = '```rust\n    let s = ' + literal + ';\n```'
        assert format_markdown(source) == source
    assert not has_multiline_string('/* nested /* " */ comment */\n// "\nlet c = \'"\';\nlet s = "ok";')
    assert not has_multiline_string('let s = r#"a "quoted" word"#;')
    assert has_multiline_string('let s = "first\\\n    second";')
    print('Rust fragment dedenting checked.')


def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == 'supports':
            return
        if sys.argv[1] == '--check':
            check()
            return
        raise SystemExit('usage: rust-dedent.py [supports RENDERER | --check]')
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
