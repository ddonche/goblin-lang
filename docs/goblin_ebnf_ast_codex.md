# Goblin v1.18 — Codex‑ready Grammar Pack

This document is generated from the latest lexer notes and design updates. It includes:

* Full EBNF for program/decls/stmts/exprs
* Final operator precedence & associativity
* AST node skeletons (Rust‑style)
* Error handling & lowering rules

Design anchors reflected here:

* **No pipeline operator**
* **Optional chaining `?>>`**
* **`act` / `action` only** (no `fn`/`op` — `op` is reserved in the lexer for operator tokens)
* **`@` usages separated by context**: `class @Item` (capitalized), `use Glam@VERSION`, `set @policy ... end`
* **`=`/`!=` as comparison operators in expressions** (assignment is statement‑only)
* **Divmod `><`**, int div `//`
* **Percent family binds tight** (`12.5%`, `%s`, `%o <expr>` / `% of <expr>`)
* **Definedness operator `&`** is a prefix unary
* `between` is parser sugar for chained comparisons

---

## 0) Terminals (summary for grammar)

* Identifiers: `Ident = [A-Za-z_][A-Za-z0-9_]*[!?]?`
* At‑identifiers:

  * `AT_CLASS` → `@` + capitalized ident (e.g., `@Item`, `@User_2`) — used only in `class` declarations
  * `AT_IDENT` → `@` + ident (e.g., `@policy`) — used in `set @policy ...` blocks
  * `AT_VERSION` → semver/range token after `use Name@...`
* Literals: `Int`, `Float`, `Big` (suffix `b`), `Money`, `String` (raw/trim\_lead flags), `Blob`, `Date`, `Time`, `DateTime`, `PercentLiteral` (`12.5%`), `Duration` (`NUMBER + s|m|h|d|w|mo|y`), `Dice`, `PickDigit`.
* Postfix specials after primary or number literal: `!` (factorial), `^` (ceil), `_` (floor), `++`, `--`, postfix square `**` (only after number literal), postfix sqrt `//` (only after number literal).
* Operators/punct used by grammar: `.  ( )  [ ]  { }  ,  :  ;  ::  =>  ->  |  ||  ??  ?>>  +  -  *  /  %  //  **  ><  =  !=  ==  !==  ===  !===  <  <=  >  >=  is  is not  and  or  not`.
* Keywords (hard subset used here): `if elif else for in while repeat unless attempt rescue ensure raise return skip jump until stop assert class enum use import export via test true false nil judge morph contract prefer set end blob`.

---

## 1) EBNF — program, declarations, statements, expressions

Notation: `[...]` optional, `{...}` zero‑or‑more, `|` alternation, terminals quoted.

### 1.1 Program

```
Program        := { TopLevel }
TopLevel       := UseDecl | ImportDecl | ExportDecl | ContractDecl
                | EnumDecl | ClassDecl | ActionDecl | TestDecl | SetPolicy
                | Stmt
```

### 1.2 Declarations

```
// use / import / export
UseDecl        := 'use' Ident [ '@' AT_VERSION ] [ 'as' Ident ] NL
ImportDecl     := 'import' ImportWhat 'from' String [ 'as' Ident ] NL
ImportWhat     := Ident { ',' Ident } | String
ExportDecl     := 'export' ( '*' | Ident { ',' Ident } ) NL

// contracts
ContractDecl   := 'contract' Ident '(' ParamList? ')' RetAnn NL Block 'end'
RetAnn         := [ '->' TypeRef ]

// enums
EnumDecl       := 'enum' Ident [ 'as' ('int'|'string') ] NL { EnumVariant NL } 'end'
EnumVariant    := Ident [ '=' (Int|String) ]

// classes and members
ClassDecl      := 'class' AT_CLASS NL { ClassMember } 'end'
ClassMember    := FieldDecl | ActionDecl | Stmt
FieldDecl      := Ident [ ':' TypeRef ] [ '=' Expr ] NL

// actions (one spelling or both)
ActionDecl     := ('act'|'action') Ident '(' ParamList? ')' RetAnn NL Block 'end'
                | ('act'|'action') Ident '(' ParamList? ')' RetAnn '=' Expr NL

// tests
TestDecl       := 'test' String NL Block 'end'

// policy blocks
SetPolicy      := 'set' '@policy' (Ident|String) NL Block 'end'

ParamList      := Param { ',' Param }
Param          := Ident [ ':' TypeRef ] [ '=' Expr ] | '...' Ident
TypeRef        := Ident [ '[' TypeRef { ',' TypeRef } ']' ]
NL             := NEWLINE
```

### 1.3 Statements & blocks

```
Stmt           := SimpleStmt NL
               | IfStmt | UnlessStmt | WhileStmt | ForStmt | RepeatUntil
               | AttemptStmt | JudgeStmt | MorphStmt | Block

SimpleStmt     := AssignStmt | ExprStmt | AssertStmt | ReturnStmt | JumpStmt | RaiseStmt
AssignStmt     := LValue '=' Expr                      // statement‑only (see disambiguation)
LValue         := Ident | MemberExpr | IndexExpr
ExprStmt       := Expr
AssertStmt     := 'assert' Expr [ ',' String ]
ReturnStmt     := 'return' [ Expr ]
JumpStmt       := 'skip' | 'jump' | 'stop'
RaiseStmt      := 'raise' [ Expr ]

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

### 1.4 Pattern control forms: `judge` and `morph`

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

### 1.5 Expressions (precedence via tiers)

```
Expr           := OrExpr
OrExpr         := AndExpr   { OrOp AndExpr }
OrOp           := 'or' | '<>'
AndExpr        := Nullish   { AndOp Nullish }
AndOp          := 'and' | '&&'
Nullish        := Compare   { '??' Compare }

Compare        := Additive  { CmpOp Additive }
CmpOp          := '=' | '!=' | '==' | '!==' | '===' | '!==='
                | '<' | '<=' | '>' | '>=' | 'is' | 'is not' | 'between'

Additive       := Multiplicative { ('+' | '-') Multiplicative }
Multiplicative := Power { ( '*' | '/' | '%' | '//' | '><' ) Power }

Power          := Prefix { '**' Prefix }            // right‑associative

Prefix         := { ('+'|'-'|'!'|'not'|'&') } Postfix

Postfix        := Primary { PostfixOp }
PostfixOp      := CallSuffix | IndexSuffix | MemberSuffix | SafeChainSuffix
                | PostFixSpecial | PercentSelf | PercentOfOther | PostfixSquare | PostfixSqrt
CallSuffix     := '(' ArgList? ')'
ArgList        := Expr { ',' Expr }
IndexSuffix    := '[' Expr { ',' Expr } ']'
MemberSuffix   := '.' Ident
SafeChainSuffix:= '?>>' ( MemberSuffix | CallSuffix | IndexSuffix )
PostFixSpecial := '!' | '^' | '_' | '++' | '--'      // postfix ops
PercentSelf    := '%s'
PercentOfOther := '%o' Expr                          // `% of <expr>` sugar
PostfixSquare  := '**'                               // only right after NUMBER literal
PostfixSqrt    := '//'                               // only right after NUMBER literal

Primary        := Literal | ArrayLit | MapLit | '(' Expr ')' | Ident | JudgeExpr | MorphExpr

ArrayLit       := '[' [ Expr { ',' Expr } [ ',' ] ] ']'
MapLit         := '{' [ MapEntry { ',' MapEntry } [ ',' ] ] '}'
MapEntry       := (String | Ident) ':' Expr

Literal        := Number | Money | String | Blob | DateLit | TimeLit | DateTimeLit
                | PercentLiteral | Duration | Dice | PickDigit
                | 'true' | 'false' | 'nil'
```

---

## 2) Operator precedence & associativity (low → high)

1. `or` (alias `<>`)
2. `and` (alias `&&`)
3. `??` (nullish)
4. **Comparisons** (non‑associative when mixed): `=`, `!=`, `==`, `!==`, `===`, `!===`, `<`, `<=`, `>`, `>=`, `is`, `is not`, `between`
5. `+`, `-`
6. `*`, `/`, `%`, `//` (int div), `><` (divmod)
7. `**` (right‑associative exponentiation)
8. Prefix unary: `+`, `-`, `!`, `not`, `&` (definedness)
9. Postfix application: `.`, `()`, `[]`, `?>> step` (left‑to‑right)
10. Postfix specials: `!`, `^`, `_`, `++`, `--`, numeric postfix square `**` / postfix sqrt `//` (only after **number literal**)
11. Percent family (tight binding): numeric percent literal `12.5%`, `%s`, `%o <expr>` / `% of <expr>`
12. Joins: `|` (no‑space), `||` (single‑space)

**Disambiguation rule:** `LValue '=' Expr` is parsed as an **assignment statement** only in statement position. Inside expressions, `=` is a comparison operator (assignment‑state family).

---

## 3) AST node skeletons (Rust‑style)

```rust
// ===== Module =====
pub struct Module { pub items: Vec<Item> }

pub enum Item {
    Use(UseDecl), Import(ImportDecl), Export(ExportDecl),
    Contract(ContractDecl), Enum(EnumDecl), Class(ClassDecl),
    Action(ActionDecl), Test(TestDecl), SetPolicy(SetPolicy),
    Stmt(Stmt),
}

// ===== Declarations =====
pub struct UseDecl { pub name: Ident, pub version: Option<VersionTok>, pub alias: Option<Ident> }
pub struct ImportDecl { pub what: Vec<Ident>, pub from: String, pub alias: Option<Ident> }
pub enum ExportList { All, Some(Vec<Ident>) }
pub struct ExportDecl { pub names: ExportList }

pub struct ContractDecl { pub name: Ident, pub params: Vec<Param>, pub ret: Option<TypeRef>, pub body: Block }

pub struct EnumDecl { pub name: Ident, pub backing: Option<EnumBacking>, pub variants: Vec<EnumVariant> }
pub enum EnumBacking { Int, Str }
pub struct EnumVariant { pub name: Ident, pub value: Option<Literal> }

pub struct ClassDecl { pub name: AtClass, pub members: Vec<ClassMember> }
pub enum ClassMember { Field(FieldDecl), Action(ActionDecl), Stmt(Stmt) }

pub struct FieldDecl { pub name: Ident, pub ty: Option<TypeRef>, pub init: Option<Expr> }

pub struct ActionDecl { pub name: Ident, pub params: Vec<Param>, pub ret: Option<TypeRef>, pub body: ActionBody }
pub enum ActionBody { Block(Block), Expr(Expr) }

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
    Or, And, Nullish,
    AssignStateEq, AssignStateNe,
    ValueEq, ValueNe,
    StrictEq, StrictNe,
    Lt, Le, Gt, Ge, Is, IsNot, Between,
    Add, Sub, Mul, Div, Mod, IntDiv, DivMod, Pow,
}

pub enum PostfixOp { Fact, Ceil, Floor, Inc, Dec, Square, Sqrt, PctSelf, PctOf(Box<Expr>) }

pub enum Key { Ident(Ident), Str(String) }

pub enum Literal {
    Int(i64), Float(f64), Big(String), Money(MoneyLit), Percent(f64),
    String(String, StringFlags), Blob(Vec<u8>),
    Date(String), Time(String), DateTime{val:String,tz:Option<String>},
    Duration(DurationLit), Dice(DiceLit), PickDigit{count:u32,digits:u32},
    True, False, Nil,
}

pub struct AtClass(pub String); // e.g., "@Item"; validate capitalization at parse
```

---

## 4) Lowering & semantic notes

* **Percent of self**: in an expression `E … N%s`, lower to `E … (N% of E …)` (self = the immediate left expression before `%s`).
* **Percent of other**: `%o X` → `% of X` during lowering.
* **Aliases**: `<>` → `or`, `&&` → `and`, `!` (prefix) → `not` for normalization.
* **`between`**: sugar for chained comparisons; lower to `(lhs >= a) and (lhs <= b)` (or appropriate variant).
* **Safe chain**: `A ?>> .b ?>> .c()` lowers to a short‑circuiting expansion; args to a skipped call are not evaluated.
* **Assignment disambiguation**: only `LValue '=' Expr` in statement position becomes an assignment; otherwise `=` is a comparison.

---

## 5) Error handling & diagnostics (expectations)

* **Lex**: Indentation errors (tabs/mixed), bad escapes/`\uXXXX`, unterminated string/block comment, invalid duration unit, invalid numeric separators.
* **Parse**:

  * `&` requires an lvalue path in prefix position → `DefinednessTargetError`.
  * Postfix square/sqrt only valid directly after a number literal.
  * Safe chain `?>>` must be followed by a member/call/index step.
  * `in` reserved for loops; reject as boolean/membership op.
  * Class names must be `AT_CLASS` (capitalized after `@`); error with fix‑it if not.
* **Recovery**: synchronize on `NL` for statements; on `end` for blocks; within lists on `,` and closing token.

---

## 6) Ready‑to‑export bundles

* **`goblin.ebnf`**: copy the EBNF blocks above into a file for Codex.
* **`ast.rs`**: copy the AST skeleton above into `goblin-ast` with your real span types.
* **`precedence.md`**: copy §2 to keep parser/diagnostics/tools in sync.
