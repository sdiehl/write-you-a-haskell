#!/usr/bin/env python3
"""Bridge mdBook 0.5 optional config fields to the author's mdBook 0.4 plugin.

Null-valued config keys are omitted, and the book items/sections envelope is
translated. Chapter contents pass through the upstream extractor unchanged. This can be removed when upstream migrates to mdbook-preprocessor.
"""
import json
import re
import subprocess
import sys

if len(sys.argv) > 1:
    sys.exit(subprocess.call(["mdbook-include-rs", *sys.argv[1:]]))

context, book = json.load(sys.stdin)
def without_nulls(value):
    if isinstance(value, dict):
        return {k: without_nulls(v) for k, v in value.items() if v is not None}
    if isinstance(value, list):
        return [without_nulls(v) for v in value]
    return value
context["config"] = without_nulls(context["config"])
book["sections"] = book.pop("items")
book["__non_exhaustive"] = None
result = subprocess.run(["mdbook-include-rs"], input=json.dumps([context, book]), text=True, capture_output=True)
sys.stderr.write(result.stderr)
# Upstream currently reports extraction errors as source diagnostics but exits
# successfully. Recognize that diagnostic format, never words in an error message.
include_error = re.search(r"(?m)^(?:.+:\d+:\d+: |Error processing chapter )", result.stderr)
if result.returncode or include_error:
    sys.exit(result.returncode or 1)
processed = json.loads(result.stdout)
processed["items"] = processed.pop("sections")
processed.pop("__non_exhaustive", None)
json.dump(processed, sys.stdout)
