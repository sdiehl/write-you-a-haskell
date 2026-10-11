# Appendix E: Diagnostic Index

This table is generated from the final compiler's error registry. Codes are
stable identifiers. Messages and secondary labels add context about the source
program. The primary label points to the location that triggered the diagnostic.

| Code    | Meaning                                        |
| :------ | :--------------------------------------------- |
| `E0001` | Invalid token                                  |
| `E0002` | Invalid syntax                                 |
| `E0003` | Invalid layout                                 |
| `E0004` | Invalid projection dot                         |
| `E0101` | Unbound variable                               |
| `E0102` | Duplicate definition                           |
| `E0103` | Unbound constructor or type                    |
| `E0104` | Duplicate field                                |
| `E0110` | Reserved operator redefined                    |
| `E0111` | Primitive outside Prelude                      |
| `E0120` | Recursive non-function binding                 |
| `E0130` | Fixity without definition                      |
| `E0201` | Type mismatch                                  |
| `E0202` | Infinite type                                  |
| `E0203` | Signature too general                          |
| `E0210` | No instance                                    |
| `E0211` | Invalid instance methods or duplicate instance |
| `E0230` | Ambiguous type                                 |
| `E0240` | Record must have one constructor               |
| `E0241` | Missing field                                  |
| `E0242` | Duplicate field binding                        |
| `E0243` | Unknown field                                  |
| `E0244` | Type does not declare field                    |
| `E0245` | Ambiguous field access                         |
| `E0250` | Main must have type IO ()                      |
| `E0301` | Kind mismatch                                  |
| `E0302` | Partially applied synonym                      |
| `E0303` | Cyclic synonym                                 |
| `W0001` | Non-exhaustive match                           |
| `W0002` | Redundant clause                               |

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
