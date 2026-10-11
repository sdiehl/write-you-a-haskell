# Appendix A: Surface Syntax

This grammar collects the final Fun language in one place. It describes source
syntax after layout has inserted braces and semicolons. Name resolution, kinds,
and typing impose the restrictions listed below. See [Appendix B](appendix-b-typing.md)
for the corresponding judgments.

## Reading the Grammar

Quoted strings are terminals. `|` separates alternatives, `[ X ]` means optional,
and `{ X }` means zero or more repetitions. Parentheses group grammar expressions.
Quoted brackets and braces are literal tokens. `varid`, `conid`, and literal tokens
are defined in the lexical section. An unbraced source file receives an outer
layout block, so both styles have the same `program` production.

## Declarations

```text
program     ::= '{' [ topdecl { ';' topdecl } ] [ ';' ] '}'
topdecl     ::= datadecl | typedecl | classdecl | instdecl
              | fixity | sig | bind

datadecl    ::= 'data' tycon { tyvar } '=' constr
                { '|' constr } [ deriving ]
constr      ::= con { atype }
              | con '{' fielddecl { ',' fielddecl } '}'
fielddecl   ::= varid { ',' varid } '::' type
deriving    ::= 'deriving' conid
              | 'deriving' '(' conid { ',' conid } ')'
typedecl    ::= 'type' conid { tyvar } '=' type

classdecl   ::= 'class' [ context '=>' ] conid tyvar
                [ 'where' '{' [ member { ';' member } ] [ ';' ] '}' ]
member      ::= sig | bind
instdecl    ::= 'instance' [ context '=>' ] conid insthead
                [ 'where' '{' [ bind { ';' bind } ] [ ';' ] '}' ]
insthead    ::= conid | '(' conid { tyvar } ')'
              | '[' tyvar ']' | '(' ')'
              | '(' tyvar ',' tyvar [ ',' tyvar ] ')'
fixity      ::= ('infixl' | 'infixr' | 'infix') digit op { ',' op }
sig         ::= var { ',' var } '::' qualtype

bind        ::= funlhs rhs | pat rhs
funlhs      ::= var { apat } | apat varop apat
rhs         ::= '=' expr [ 'where' decls ]
              | guard '=' expr { guard '=' expr } [ 'where' decls ]
guard       ::= '|' expr
decls       ::= '{' [ localdecl { ';' localdecl } ] [ ';' ] '}'
localdecl   ::= sig | bind
```

A record declaration has exactly one constructor. Its labels are distinct, but
other record types may reuse them. Instance-head variables are distinct and the
context mentions only those variables. Fixity declarations occur only at the top
level, with precedence `0` through `9`. The only deriving classes are `Eq`, `Ord`,
and `Show`. Pattern bindings are nonrecursive. A recursive binding must become a
function after desugaring.

## Expressions

```text
expr        ::= infixexp [ '::' qualtype ]
infixexp    ::= lexp { op lexp } | '-' infixexp
lexp        ::= '\' apat { apat } '->' expr
              | 'let' decls 'in' expr
              | 'if' expr 'then' expr 'else' expr
              | 'case' expr 'of' '{' alt { ';' alt } [ ';' ] '}'
              | 'do' '{' stmt { ';' stmt } [ ';' ] '}'
              | fexp
fexp        ::= aexp { aexp }
aexp        ::= atom { projection | update }
atom        ::= var | con | literal
              | '(' expr ')'
              | '(' expr ',' expr [ ',' expr ] ')'
              | '[' [ expr { ',' expr } ] ']'
              | '[' expr '..' expr ']'
              | '(' infixexp op ')' | '(' op infixexp ')'
              | '(' PROJ_SECTION varid ')'
              | conid '{' fbind { ',' fbind } '}'
projection  ::= PROJ varid
update      ::= '{' fbind { ',' fbind } '}'
fbind       ::= varid '=' expr
alt         ::= pat ( '->' expr | guard '->' expr
                { guard '->' expr } ) [ 'where' decls ]
stmt        ::= expr | pat '<-' expr | 'let' decls

var         ::= varid | '(' varsym ')'
con         ::= conid | '(' ':' ')' | '[' ']' | '(' ')'
              | '(' ',' ')' | '(' ',' ',' ')'
op          ::= varsym | '`' varid '`' | ':'
varop       ::= varsym | '`' varid '`'
```

A block's last `do` statement must be an expression. A bare constructor followed
by a record brace is construction, not update. Construction requires every field
exactly once. Updates preserve the record's type. Tuples have arity two or three.
`(- e)` is negation rather than a right section. `(&&)` and `(||)` are accepted
as values, but these operators cannot be redefined.

Projection and update bind tighter than application. Thus `f r.x` means
`f (r.x)`. Application associates left. Arrow types associate right. An operator
chain is resolved using the declarations in scope, with `infixl 9` as the default.
Unary minus follows Haskell's precedence-six rule. Conflicting associativities
at equal precedence require parentheses.

The Prelude supplies these fixities:

| Associativity  | Precedence | Operators                                                     |
| -------------- | ---------- | ------------------------------------------------------------- |
| Right          | 9          | `.`                                                           |
| Left           | 7          | `*`, `/`, `` `div` ``, `` `mod` ``, `` `quot` ``, `` `rem` `` |
| Left           | 6          | `+`, `-`                                                      |
| Right          | 6          | `<>`                                                          |
| Right          | 5          | `:`, `++`                                                     |
| Nonassociative | 4          | `==`, `/=`, `<`, `<=`, `>`, `>=`                              |
| Left           | 4          | `<$>`, `<*>`                                                  |
| Right          | 3          | `&&`                                                          |
| Right          | 2          | `\|\|`                                                        |
| Left           | 1          | `>>=`, `>>`                                                   |
| Right          | 0          | `$`                                                           |

## Patterns and Types

```text
pat         ::= lpat [ ':' pat ]
lpat        ::= conid apat { apat } | '-' intlit | apat
apat        ::= varid [ '@' apat ] | '_' | con
              | conid '{' [ fpat { ',' fpat } ] '}'
              | literal | '(' pat ')'
              | '(' pat ',' pat [ ',' pat ] ')'
              | '[' [ pat { ',' pat } ] ']'
fpat        ::= varid '=' pat

qualtype    ::= [ context '=>' ] type
context     ::= pred | '(' pred { ',' pred } ')'
pred        ::= conid tyvar
type        ::= btype [ '->' type ]
btype       ::= atype { atype }
atype       ::= tycon | tyvar | '(' type ')'
              | '(' type ',' type [ ',' type ] ')'
              | '[' type ']'
tycon       ::= conid | '[' ']' | '(' ')'
              | '(' ',' ')' | '(' ',' ',' ')' | '(' '->' ')'
tyvar       ::= varid
```

Pattern variables are distinct within a pattern vector. Omitted record fields
mean wildcards. Constructors in patterns must be saturated. Type synonyms must
be fully applied and acyclic. There is no explicit `forall`, kind annotation,
lazy pattern, qualified name, or user-defined infix constructor.

## Lexical Structure and Layout

Identifiers are ASCII: `varid` matches `[a-z_][A-Za-z0-9_']*` and `conid` matches
`[A-Z][A-Za-z0-9_']*`. `_` alone is a wildcard. Reserved words are:

```text
case class data deriving do else if in infix infixl infixr
instance let of then type where
```

An operator is a maximal run of the following symbol characters, excluding the
reserved operators below. User-defined operators cannot begin with `:`.

```text
symbol characters: ! # $ % & * + . / < = > ? @ \ ^ | ~ : -
reserved operators: .. : :: = \ | <- -> @ =>
```

Integer literals are decimal or `0x` hexadecimal, with underscore separators.
Double literals contain a decimal point or an exponent, as in `1.5`, `1e10`, and
`2.5e-3`. Signs are syntax outside the literal token. Character literals contain
one Unicode scalar in single quotes. Strings contain Unicode text in double
quotes. Both support `\n`, `\t`, `\\`, `\'`, `\"`, `\0`, and `\u{…}` escapes.
`literal` ranges over integer, double, character, and string tokens.

Line comments start with `--` followed by a nonsymbol character. `-->` is an
operator. Block comments use `{-` and `-}` and may nest.

Before layout, adjacency distinguishes projection dots. A dot immediately after
a variable, closing delimiter, or literal, and immediately before a `varid`, is
`PROJ`. Immediately after `(` and before a `varid`, it is `PROJ_SECTION`.
Otherwise it is an ordinary operator. Consequently `f.g` projects a field, while
`f . g` composes functions.

Layout inserts braces after `let`, `where`, `of`, and `do`, and around an unbraced
file. Equal indentation separates declarations or statements. Dedentation closes
blocks. Explicit braces take precedence, and `in` closes an implicit `let` block
even on the same line. [Chapter 8](ch08-layout.md#see-the-invisible-delimiters)
shows the inserted tokens directly. The Haskell Report gives the underlying
lexical and layout conventions [@haskell2010].
