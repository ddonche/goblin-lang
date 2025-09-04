# Goblin Grammar (EBNF), Precedence, AST Mapping, and Diagnostics — v1.18

> Source of truth: **Lexer Notes v1.18** (this draft derives only from the latest notes you uploaded). It intentionally **omits any pipeline operator** and uses **`?>>`** for optional chaining and **`&`** for definedness.

---

## 0) Lexical/terminal summary (for reference)
Identifiers, literals, tokens, and keywords come from the lexer notes. Highlights used by the grammar below:
- Postfix specials: `++`, `--`, postfix factorial `!`, postfix ceil `^`, postfix floor `_`, postfix square `**` (after number), postfix sqrt `//` (after number).
- Arithmetic: `+ - * / % // ** ><` (divmod).
- Logical & nullish: `and or not`, aliases `&& <> !` (parser normalizes); `??` nullish.
- Member/call/index/safe-chain: `. () [] ? >>` (safe chain attaches to a following member/call/index step).
- Comparison & membership sugar: `== != === !== < <= > >= between`.
- Assignment and compound assign: `= += -= *= /= %= **=` (assignment is a **statement**, not an expression).
- Joins: `|` (no-space), `||` (single-space).
- Percent family: numeric percent literal (`12.5%`), `%s` (percent-of-self), `%o` (percent-of-other / `% of <expr>` sugar).
- Duration units: `s m h d w mo y` (greedy; `mo` ≠ `m`).
- Money literals (single token): `$12.34`, `12.34 USD`, `US$12.34`.
- Blob literal: `blob` / `blob "..."`.
- Dates/times: `date "…"`, `time "…"`, `datetime "…" tz:"…"`.
- Use/version: `use Name[@VERSION] [as Alias]`; also `via`, `prefer` for provider binding.
- Keywords (hard subset used here): `if elif else for in while repeat unless attempt rescue ensure raise return skip jump until stop assert class fn op enum use import export via test true false nil contract prefer judge morph set end blob`.

---

## 1) EBNF — program, decls, statements, expressions

Notation: `[...]` optional, `{...}` zero-or-more, `(...)` grouping, `|` alternation. Terminals are quoted.

### 1.1 Program
```
Program        := { TopLevel }
TopLevel       := UseDecl | ImportDecl | ExportDecl | ContractDecl
                | EnumDecl | ClassDecl | OpDecl | TestDecl | SetPolicy
                | Stmt
```

### 1.2 Declarations
```
// use / import / export
UseDecl        := 'use' Ident [ '@' Version ] [ 'as' Ident ] NL
ImportDecl     := 'import' ImportWhat 'from' String [ 'as' Ident ] NL
ImportWhat     := Ident { ',' Ident } | String
ExportDecl     := 'export' ( '*' | Ident { ',' Ident } ) NL

// contracts (capability interfaces)
ContractDecl   := 'contract' Ident '(' ParamList? ')' RetAnn NL Block 'end'
RetAnn         := [ '->' TypeRef ]

// enums
EnumDecl       := 'enum' Ident [ 'as' ('int'|'string') ] NL { EnumVariant NL } 'end'
EnumVariant    := Ident [ '=' (Int|String) ]

// classes and members
ClassDecl      := 'class' Ident NL { ClassMember } 'end'
ClassMember    := FieldDecl | OpDecl | ActionDecl | Stmt
FieldDecl      := Ident [ ':' TypeRef ] [ '=' Expr ] NL

// actions / functions (notes list `fn`; allow both spellings if needed)
ActionDecl     := ('fn'|'action'|'act') Ident '(' ParamList? ')' RetAnn NL Block 'end'

// operations
OpDecl         := 'op' Ident OpBody
OpBody         := '(' ParamList? ')' ( NL Block 'end' | '=' Expr NL )

// tests
TestDecl       := 'test' String NL Block 'end'

// policy configuration blocks
SetPolicy      := 'set' '@policy' (Ident|String) NL Block 'end'

ParamList      := Param { ',' Param }
Param          := Ident [ ':' TypeRef ] [ '=' Expr ] | '...' Ident
TypeRef        := Ident [ '[' TypeRef { ',' TypeRef } ']' ]
Version        := /* semver/range token per lexer */
NL             := NEWLINE
```

### 1.3 Statements & blocks
```
Stmt           := SimpleStmt NL
               | IfStmt | UnlessStmt | WhileStmt | ForStmt | RepeatUntil
               | AttemptStmt | JudgeStmt | MorphStmt | Block

SimpleStmt     := AssignStmt | ExprStmt | AssertStmt | ReturnStmt | JumpStmt | RaiseStmt
AssignStmt     := LValue '=' Expr
LValue         := Ident | MemberExpr | IndexExpr
ExprStmt       := Expr
AssertStmt     := 'assert' Expr [ ',' String ]
ReturnStmt     := 'return' [ Expr ]
JumpStmt       := 'skip' | 'jump' | 'stop'
RaiseStmt      := 'raise' [ Expr ]            // bare rethrow allowed inside rescue

IfStmt         := 'if' Expr NL Block { 'elif' Expr NL Block } [ 'else' NL Block ] 'end'
UnlessStmt     := 'unless' Expr NL Block [ 'else' NL Block ] 'end'
WhileStmt      := 'while' Expr NL Block 'end'
ForStmt        := 'for' IterVars 'in' Expr [ WhereClause ] NL Block 'end'
IterVars       := Ident [ ',' Ident ]
WhereClause    := 'where' Expr
RepeatUntil    := 'repeat' NL Block 'until' Expr NL 'end'

AttemptStmt    := 'attempt' NL Block [ 'rescue' [ Ident [ 'as' Ident ] ] NL Block ]
                                  [ 'ensure' NL Block ] 'end'

Block          := { Stmt }
```

### 1.4 Patterned control: `judge` and `morph`
```
JudgeStmt      := JudgeExpr
JudgeExpr      := 'judge' [ Expr ] NL { GuardArm } [ ElseArm ] 'end'
GuardArm       := Expr ':' ArmBody NL
ElseArm        := 'else' ':' ArmBody NL
ArmBody        := ( Expr [NL] ) | ( NL Block )

MorphStmt      := MorphExpr
MorphExpr      := 'morph' Expr NL { PatternArm } [ ElseArm ] 'end'
PatternArm     := Pattern ':' ArmBody NL
Pattern        := Literal | Ident | TypeRef | RangePattern
RangePattern   := Expr '..' Expr    // inclusive
```

### 1.5 Expressions (precedence tiers)
```
Expr           := OrExpr
OrExpr         := AndExpr   { OrOp AndExpr }
OrOp           := 'or' | '<>'
AndExpr        := Nullish   { AndOp Nullish }
AndOp          := 'and' | '&&'
Nullish        := Compare   { '??' Compare }

Compare        := Additive  { CmpOp Additive }
CmpOp          := '==' | '!=' | '===' | '!==' | '<' | '<=' | '>' | '>=' | 'between'

Additive       := Multiplicative { ('+' | '-') Multiplicative }
Multiplicative := Power { ( '*' | '/' | '%' | '//' | '><' ) Power }

Power          := Prefix { '**' Prefix }            // right-assoc

Prefix         := { ('+'|'-'|'!'|'not'|'&') } Postfix

Postfix        := Primary { PostfixOp }
PostfixOp      := CallSuffix | IndexSuffix | MemberSuffix | SafeChainSuffix
                | PostfixFact | PostfixCeil | PostfixFloor | PostfixInc | PostfixDec
                | PercentSelf | PercentOfOther | PostfixSquare | PostfixSqrt
CallSuffix     := '(' ArgList? ')'
ArgList        := Expr { ',' Expr }
IndexSuffix    := '[' Expr { ',' Expr } ']'
MemberSuffix   := '.' Ident
SafeChainSuffix:= '?>>' ( MemberSuffix | CallSuffix | IndexSuffix )
PostfixFact    := '!'
PostfixCeil    := '^'
PostfixFloor   := '_'
PostfixInc     := '++'
PostfixDec     := '--'
PercentSelf    := '%s'
PercentOfOther := '%o' Expr         // `% of <expr>` sugar form desugars in lowering
PostfixSquare  := '**'              // when directly after NUMBER
PostfixSqrt    := '//'              // when directly after NUMBER

Primary        := Literal
               | ArrayLit | MapLit
               | '(' Expr ')'
               | Ident
               | JudgeExpr | MorphExpr

ArrayLit       := '[' [ Expr { ',' Expr } [ ',' ] ] ']'
MapLit         := '{' [ MapEntry { ',' MapEntry } [ ',' ] ] '}'
MapEntry       := (String | Ident) ':' Expr

Literal        := Number | Money | String | Blob | DateLit | TimeLit | DateTimeLit
                | 'true' | 'false' | 'nil'
```

---

## 2) Operator precedence & associativity (low → high)
1. `or` (`<>` alias)
2. `and` (`&&` alias)
3. `??` (nullish coalescing)
4. Comparisons: `== != === !== < <= > >= between` (non-assoc when mixed)
5. `+ -`
6. `* / % // ><`
7. `**` (right-associative)
8. Prefix: `+ - ! not &`
9. Postfix application: member `.`, call `()`, index `[]`, safe-chain `?>> step`
10. Postfix specials: `! ^ _ ++ --` and percent family `%s` / `%o <expr>`; numeric postfix square/sqrt after NUMBER
11. Joins: `| ||` (string joining)

> Assignment `=` (and compound assigns) are **statement-level**.

---

## 3) AST node mapping (skeleton)
_Struct/enum names are illustrative; adapt to your crate names._

```rust
// ===== Module =====
pub struct Module { pub items: Vec<Item> }

pub enum Item {
    Use(UseDecl), Import(ImportDecl), Export(ExportDecl),
    Contract(ContractDecl), Enum(EnumDecl), Class(ClassDecl),
    Op(OpDecl), Test(TestDecl), SetPolicy(SetPolicy),
    Stmt(Stmt),
}

// ===== Declarations =====
pub struct UseDecl { pub name: Ident, pub version: Option<Version>, pub alias: Option<Ident> }
pub struct ImportDecl { pub what: Vec<Ident>, pub from: String, pub alias: Option<Ident> }
pub struct ExportDecl { pub names: ExportList }
pub enum ExportList { All, Some(Vec<Ident>) }

pub struct ContractDecl { pub name: Ident, pub params: Vec<Param>, pub ret: Option<TypeRef>, pub body: Block }

pub struct EnumDecl { pub name: Ident, pub backing: Option<EnumBacking>, pub variants: Vec<EnumVariant> }
pub enum EnumBacking { Int, Str }
pub struct EnumVariant { pub name: Ident, pub value: Option<Literal> }

pub struct ClassDecl { pub name: Ident, pub members: Vec<ClassMember> }
pub enum ClassMember { Field(FieldDecl), Op(OpDecl), Action(ActionDecl), Stmt(Stmt) }

pub struct FieldDecl { pub name: Ident, pub ty: Option<TypeRef>, pub init: Option<Expr> }

pub struct ActionDecl { pub name: Ident, pub params: Vec<Param>, pub ret: Option<TypeRef>, pub body: Block }

pub struct OpDecl { pub name: Ident, pub params: Vec<Param>, pub ret: Option<TypeRef>, pub body: OpBody }
pub enum OpBody { Block(Block), Expr(Expr) }

pub struct TestDecl { pub name: String, pub body: Block }

pub struct SetPolicy { pub name: StringOrIdent, pub body: Block }

pub struct Param { pub name: Ident, pub ty: Option<TypeRef>, pub default_: Option<Expr>, pub variadic: bool }
pub struct TypeRef { pub head: Ident, pub args: Vec<TypeRef> }

// ===== Statements =====
pub enum Stmt {
    Assign(Assign), Expr(Expr), Assert{cond: Expr, msg: Option<String>},
    Return(Option<Expr>), Jump(JumpKind), Raise(Option<Expr>),
    If(IfStmt), Unless{cond: Expr, then_b: Block, else_b: Option<Block>},
    While{cond: Expr, body: Block}, For{vars: Vec<Ident>, iter: Expr, where_: Option<Expr>, body: Block},
    RepeatUntil{body: Block, cond: Expr}, Attempt(Attempt),
    Judge(Judge), Morph(Morph), Block(Block),
}

pub struct Assign { pub target: LValue, pub value: Expr }
pub enum LValue { Name(Ident), Member(Box<Expr>, Ident), Index(Box<Expr>, Vec<Expr>) }

pub struct IfStmt { pub cond: Expr, pub then_b: Block, pub elifs: Vec<(Expr, Block)>, pub else_b: Option<Block> }

pub enum JumpKind { Skip, Jump, Stop }

pub struct Attempt { pub try_b: Block, pub rescue: Option<Rescue>, pub ensure_b: Option<Block> }
pub struct Rescue { pub name: Option<Ident>, pub as_name: Option<Ident>, pub body: Block }

pub struct Judge { pub scrutinee: Option<Expr>, pub arms: Vec<(Expr, ArmBody)>, pub else_arm: Option<ArmBody> }
pub struct Morph { pub scrutinee: Expr, pub arms: Vec<(Pattern, ArmBody)>, pub else_arm: Option<ArmBody> }

pub enum ArmBody { Expr(Expr), Block(Block) }

pub enum Pattern { Lit(Literal), Bind(Ident), Type(TypeRef), Range(Expr, Expr) }

pub struct Block { pub stmts: Vec<Stmt> }

// ===== Expressions =====
pub enum Expr {
    Lit(Literal), Name(Ident), Array(Vec<Expr>), Map(Vec<(Key, Expr)>),
    Unary{op: UnaryOp, rhs: Box<Expr>}, Binary{lhs: Box<Expr>, op: BinOp, rhs: Box<Expr>},
    Call{callee: Box<Expr>, args: Vec<Expr>}, Member{base: Box<Expr>, name: Ident},
    Index{base: Box<Expr>, args: Vec<Expr>}, SafeChain(Vec<SafeStep>),
    Postfix{base: Box<Expr>, op: PostfixOp},
    JudgeExpr(Judge), MorphExpr(Morph), Join{parts: Vec<(Expr, JoinKind)>},
    Nullish{lhs: Box<Expr>, rhs: Box<Expr>},
}

pub enum SafeStep { Member(Ident), Call(Vec<Expr>), Index(Vec<Expr>) }

pub enum JoinKind { NoSpace, Space }

pub enum UnaryOp { Pos, Neg, Not, Definedness }

pub enum BinOp {
    Or, And, CmpEq, CmpNe, CmpStrictEq, CmpStrictNe, Lt, Le, Gt, Ge,
    Between, Add, Sub, Mul, Div, Mod, IntDiv, Pow, DivMod,
}

pub enum PostfixOp { Fact, Ceil, Floor, Inc, Dec, Square, Sqrt, PctSelf, PctOf(Box<Expr>) }

pub enum Key { Ident(Ident), Str(String) }

pub enum Literal { Int(i64), Float(f64), Big(String), Money(MoneyLit), Percent(f64),
                   String(String, StringFlags), Blob(Vec<u8>), Date(String), Time(String), DateTime{val:String,tz:Option<String>},
                   True, False, Nil }
```

Lowering rules (selected):
- `%s` in a left-hand context desugars to `% of <lhs-expression>`.
- `%o E` becomes `% of E` with explicit base.
- `<>`, `&&`, `!` normalize to `or`, `and`, `not`.

---

## 4) Error handling & recovery (expectations)
- **Lex errors already defined** (bad escape, unterminated string/block comment, tabs/mixed indent, unknown unit, invalid numeric separator, etc.).
- **Syntax errors to raise** at parse:
  - `&` requires lvalue path (ident / member / index).
  - Postfix square/sqrt only valid when directly following a **number literal**.
  - `between` only valid in comparison position; expands to chained comparison during lowering.
  - `step` is not a keyword; if present in loop headers → syntax error.
  - `in` is reserved for loops; reject as boolean/membership op.
  - Safe chain `?>>` must be followed by a step (member/call/index).
- **Recovery** suggestions:
  - Synchronize on `NL` at statement boundaries and on `end` at block boundaries.
  - Inside arg/elem lists, synchronize on `,` and closing token `)`/`]`/`}`.

Diagnostics to surface (non-exhaustive):
- `IndentationError` (tabs or mixed width), `UnexpectedDedent`.
- `DurationUnitError` (e.g., `10min` → suggest `10m` or `10mo`).
- `PercentAmbiguityError` (space around `%` vs percent literal).
- `MoneyLiteralError` (invalid currency form).
- `DefinednessTargetError` (`&` on non-lvalue).

---

## 5) Notes on remaining features from lexer scope
- **Glams / provider binding:** `use Name[@ver] as Alias`; `expr via Alias` and `prefer cap.name via Alias` remain declarative; no special precedence.
- **Joins:** `|` and `||` parse as a dedicated join expression that concatenates stringy operands (space vs no-space).
- **Dice & pick shorthand:** keep as literal token kinds flowing into `Literal` variants or a dedicated `RandomLit` node if you prefer.
- **Blob, date/time/datetime, money:** modeled as distinct `Literal` kinds with payloads for downstream type-checking.

---

### Ready for Codex
This file contains: full EBNF, precedence/assoc, AST node skeleton, and error expectations — aligned to v1.18 and free of legacy constructs.

