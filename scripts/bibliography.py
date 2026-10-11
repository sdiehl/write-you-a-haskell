#!/usr/bin/env python3
"""Render the HTML reference list with Pandoc's pinned Chicago citeproc.

mdbook-bib retains citation parsing and links. Its 0.5.3 CSL reference renderer
flattens emphasis and duplicates proceedings metadata; replace only that page.
"""
import html
import json
from pathlib import Path
import re
import subprocess
import sys
from urllib.parse import urlsplit


def render(bibliography):
    result = subprocess.run(
        ["pandoc", "--citeproc", "--bibliography", str(bibliography),
         "--from=markdown", "--to=html5", "--wrap=none"],
        input='---\nnocite: "@*"\n---\n',
        text=True, capture_output=True, check=True,
    )
    if result.stderr:
        sys.stderr.write(result.stderr)
    # Pandoc prefixes keys with ref-; keep existing mdbook-bib fragment links.
    content = result.stdout.replace('id="ref-', 'id="')

    def source_link(match):
        url = html.unescape(match.group(1))
        parsed = urlsplit(url)
        label = "DOI: " + parsed.path.lstrip('/') if parsed.hostname == "doi.org" else "Online"
        return (f'<a class="bib-source" href="{html.escape(url, quote=True)}" '
                f'title="{html.escape(url, quote=True)}">{html.escape(label)}</a>')

    content = re.sub(r'<a\s+href="([^"]+)"[^>]*>.*?</a>', source_link, content, flags=re.S)
    return '# Bibliography\n\n' + content


def main():
    if len(sys.argv) > 1:
        sys.exit(0 if sys.argv[1:] == ["supports", "html"] else 1)
    context, book = json.load(sys.stdin)
    chapters = []
    pending = list(book["items"])
    while pending:
        item = pending.pop()
        if "Chapter" in item:
            chapter = item["Chapter"]
            chapters.append(chapter)
            pending.extend(chapter["sub_items"])
    references = [c for c in chapters if c["path"] == "bibliography.md"]
    if len(references) != 1:
        raise RuntimeError("expected one mdbook-bib bibliography chapter")
    root = Path(context["root"])
    bibliography = root / context["config"]["book"].get("src", "src") / context["config"]["preprocessor"]["bib"]["bibliography"]
    content = render(bibliography)
    targets = set(re.findall(r'id="([^"]+)"', content))
    for chapter in chapters:
        for key in re.findall(r'bibliography\.html#([A-Za-z0-9_-]+)', chapter["content"]):
            if key not in targets:
                raise RuntimeError(f"missing bibliography target: {key}")
    references[0]["content"] = content
    json.dump(book, sys.stdout)


if __name__ == "__main__":
    main()
