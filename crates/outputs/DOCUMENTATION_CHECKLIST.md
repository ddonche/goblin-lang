# Goblin Documentation Checklist

Source-derived checklist for the language, interpreter, VM, DES, tooling, embedding APIs, and repository-specific data formats.

Total checklist entries: **1306**. Runtime parity-derived entries: **348**.

Every item includes exact repository source anchors. Existing prose documentation is indexed at the end, but source remains authoritative when prose and code disagree.

## Completion Standard

An item is complete only when its documentation covers the applicable parts of: purpose, syntax/API, inputs, outputs, aliases, errors, side effects, state/lifecycle, examples, edge cases, and cross-links. User-facing blocks must show both `xx` and `end` terminator forms.

## Coverage Summary

- Language Runtime Reference: 348
- AST and Language Data Model: 141
- Bytecode and Opcodes: 75
- CLI and Configuration: 25
- Compiler and Lowering: 6
- DES Architecture: 77
- DES Tick Runtime: 2
- Debugging and Introspection: 7
- Development Server: 2
- Diagnostics and Error Codes: 23
- GQL Query Language: 22
- Grammar and Syntax: 75
- Grid Runtime: 80
- Host Embedding: 30
- Interpreter Embedding API: 121
- Lexer and Parser APIs: 51
- Lexical Structure: 43
- Source Files and Spans: 1
- VM Embedding API: 1
- VM Errors: 18
- VM Execution Model: 9
- VM Session and State: 14
- Values, Memory, and Collection Backends: 57
- Workers and Swarm Execution: 30
- YALL Data Format: 48

## Language Runtime Reference

- [x] **abs**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12509`, `crates/goblin-vm/src/compiler.rs:1958`, `crates/goblin-vm/src/value.rs:343`, `crates/goblin-vm/src/builtins.rs:55`, `crates/goblin-vm/src/compiler.rs:1959`, `crates/goblin-vm/src/vm.rs:1827`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **after**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14769`, `crates/goblin-vm/src/compiler.rs:1991`, `crates/goblin-vm/src/value.rs:381`, `crates/goblin-vm/src/builtins.rs:483`, `crates/goblin-vm/src/compiler.rs:1992`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **after_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14823`, `crates/goblin-vm/src/compiler.rs:1993`, `crates/goblin-vm/src/value.rs:383`, `crates/goblin-vm/src/builtins.rs:503`, `crates/goblin-vm/src/compiler.rs:1994`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **append_file**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13976`, `crates/goblin-vm/src/compiler.rs:2176`, `crates/goblin-vm/src/value.rs:595`, `crates/goblin-vm/src/builtins.rs:1035`, `crates/goblin-vm/src/compiler.rs:2177`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **ask**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11159`, `crates/goblin-vm/src/compiler.rs:2231`, `crates/goblin-vm/src/value.rs:650`, `crates/goblin-vm/src/builtins.rs:3194`, `crates/goblin-vm/src/compiler.rs:2232`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **avg**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6399`, `crates/goblin-vm/src/compiler.rs:1961`, `crates/goblin-vm/src/value.rs:346`, `crates/goblin-vm/src/builtins.rs:130`, `crates/goblin-vm/src/compiler.rs:1962`, `crates/goblin-vm/src/vm.rs:1884`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **b**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12413`, `crates/goblin-vm/src/compiler.rs:2194`, `crates/goblin-vm/src/value.rs:614`, `crates/goblin-vm/src/builtins.rs:2847`, `crates/goblin-vm/src/compiler.rs:2195`, `crates/goblin-vm/src/vm.rs:1890`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **backend**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15304`, `crates/goblin-vm/src/compiler.rs:2240`, `crates/goblin-vm/src/value.rs:660`, `crates/goblin-vm/src/builtins.rs:3256`, `crates/goblin-vm/src/compiler.rs:2241`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **basename**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13985`, `crates/goblin-vm/src/compiler.rs:2216`, `crates/goblin-vm/src/value.rs:634`, `crates/goblin-vm/src/builtins.rs:3018`, `crates/goblin-vm/src/compiler.rs:2217`, `crates/goblin-vm/src/vm.rs:1914`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **before**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14742`, `crates/goblin-vm/src/compiler.rs:1990`, `crates/goblin-vm/src/value.rs:380`, `crates/goblin-vm/src/builtins.rs:473`, `crates/goblin-vm/src/compiler.rs:1991`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **before_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14796`, `crates/goblin-vm/src/compiler.rs:1992`, `crates/goblin-vm/src/value.rs:382`, `crates/goblin-vm/src/builtins.rs:493`, `crates/goblin-vm/src/compiler.rs:1993`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14850`, `crates/goblin-vm/src/compiler.rs:2170`, `crates/goblin-vm/src/value.rs:589`, `crates/goblin-vm/src/builtins.rs:849`, `crates/goblin-vm/src/compiler.rs:2171`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **big**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12413`, `crates/goblin-vm/src/compiler.rs:2194`, `crates/goblin-vm/src/value.rs:614`, `crates/goblin-vm/src/builtins.rs:2847`, `crates/goblin-vm/src/compiler.rs:2195`, `crates/goblin-vm/src/vm.rs:1890`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **bool**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12429`, `crates/goblin-vm/src/compiler.rs:2211`, `crates/goblin-vm/src/value.rs:527`, `crates/goblin-vm/src/builtins.rs:2122`, `crates/goblin-vm/src/compiler.rs:2153`, `crates/goblin-vm/src/compiler.rs:2212`, `crates/goblin-vm/src/vm.rs:1803`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **ceil**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12487`, `crates/goblin-vm/src/compiler.rs:1964`, `crates/goblin-vm/src/value.rs:349`, `crates/goblin-vm/src/builtins.rs:153`, `crates/goblin-vm/src/compiler.rs:1965`, `crates/goblin-vm/src/vm.rs:1830`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [x] **chars**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14926`, `crates/goblin-vm/src/compiler.rs:2163`, `crates/goblin-vm/src/value.rs:580`, `crates/goblin-vm/src/builtins.rs:2654`, `crates/goblin-vm/src/compiler.rs:2164`, `crates/goblin-vm/src/vm.rs:1826`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **clamp**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6596`, `crates/goblin-vm/src/compiler.rs:1967`, `crates/goblin-vm/src/value.rs:352`, `crates/goblin-vm/src/builtins.rs:177`, `crates/goblin-vm/src/compiler.rs:1968`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **clear_all_tokens**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6083`, `crates/goblin-vm/src/compiler.rs:2251`, `crates/goblin-vm/src/value.rs:671`, `crates/goblin-vm/src/builtins.rs:3381`, `crates/goblin-vm/src/compiler.rs:2252`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **clear_format**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12349`, `crates/goblin-vm/src/compiler.rs:2239`, `crates/goblin-vm/src/value.rs:659`, `crates/goblin-vm/src/builtins.rs:3233`, `crates/goblin-vm/src/compiler.rs:2240`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **clear_token**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6047`, `crates/goblin-vm/src/compiler.rs:2249`, `crates/goblin-vm/src/value.rs:669`, `crates/goblin-vm/src/builtins.rs:3364`, `crates/goblin-vm/src/compiler.rs:2250`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **clear_tokens**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6059`, `crates/goblin-vm/src/compiler.rs:2250`, `crates/goblin-vm/src/value.rs:670`, `crates/goblin-vm/src/builtins.rs:3373`, `crates/goblin-vm/src/compiler.rs:2251`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **clone_object**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13923`, `crates/goblin-vm/src/compiler.rs:2270`, `crates/goblin-vm/src/value.rs:681`, `crates/goblin-vm/src/builtins.rs:3419`, `crates/goblin-vm/src/compiler.rs:2271`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **cookie**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13965`, `crates/goblin-vm/src/compiler.rs:2035`, `crates/goblin-vm/src/value.rs:555`, `crates/goblin-vm/src/builtins.rs:2396`, `crates/goblin-vm/src/compiler.rs:2036`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **copy_file**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13980`, `crates/goblin-vm/src/compiler.rs:2190`, `crates/goblin-vm/src/value.rs:610`, `crates/goblin-vm/src/builtins.rs:2806`, `crates/goblin-vm/src/compiler.rs:2191`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **count**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13785`, `crates/goblin-vm/src/compiler.rs:1969`, `crates/goblin-vm/src/value.rs:370`, `crates/goblin-vm/src/builtins.rs:346`, `crates/goblin-vm/src/compiler.rs:1970`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **count_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15104`, `crates/goblin-vm/src/compiler.rs:2004`, `crates/goblin-vm/src/value.rs:394`, `crates/goblin-vm/src/builtins.rs:620`, `crates/goblin-vm/src/compiler.rs:2005`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **create_dir**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13974`, `crates/goblin-vm/src/compiler.rs:2189`, `crates/goblin-vm/src/value.rs:609`, `crates/goblin-vm/src/builtins.rs:2799`, `crates/goblin-vm/src/compiler.rs:2190`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **decision_debug**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6725`, `crates/goblin-vm/src/compiler.rs:2264`, `crates/goblin-vm/src/value.rs:675`, `crates/goblin-vm/src/builtins.rs:3447`, `crates/goblin-vm/src/compiler.rs:2265`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14135`, `crates/goblin-vm/src/compiler.rs:2062`, `crates/goblin-vm/src/value.rs:444`, `crates/goblin-vm/src/builtins.rs:1499`, `crates/goblin-vm/src/compiler.rs:2063`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_all**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14161`, `crates/goblin-vm/src/compiler.rs:2067`, `crates/goblin-vm/src/value.rs:449`, `crates/goblin-vm/src/builtins.rs:1554`, `crates/goblin-vm/src/compiler.rs:2068`, `crates/goblin-vm/src/vm.rs:1879`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_at**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14150`, `crates/goblin-vm/src/compiler.rs:2065`, `crates/goblin-vm/src/value.rs:447`, `crates/goblin-vm/src/builtins.rs:1512`, `crates/goblin-vm/src/compiler.rs:2066`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14172`, `crates/goblin-vm/src/compiler.rs:2095`, `crates/goblin-vm/src/value.rs:468`, `crates/goblin-vm/src/builtins.rs:1791`, `crates/goblin-vm/src/compiler.rs:2096`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_first**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14140`, `crates/goblin-vm/src/compiler.rs:2063`, `crates/goblin-vm/src/value.rs:445`, `crates/goblin-vm/src/builtins.rs:1504`, `crates/goblin-vm/src/compiler.rs:2064`, `crates/goblin-vm/src/vm.rs:1876`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14145`, `crates/goblin-vm/src/compiler.rs:2064`, `crates/goblin-vm/src/value.rs:446`, `crates/goblin-vm/src/builtins.rs:1508`, `crates/goblin-vm/src/compiler.rs:2065`, `crates/goblin-vm/src/vm.rs:1877`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14166`, `crates/goblin-vm/src/compiler.rs:2094`, `crates/goblin-vm/src/value.rs:468`, `crates/goblin-vm/src/builtins.rs:1785`, `crates/goblin-vm/src/compiler.rs:2095`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_path**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13977`, `crates/goblin-vm/src/compiler.rs:2191`, `crates/goblin-vm/src/value.rs:611`, `crates/goblin-vm/src/builtins.rs:2819`, `crates/goblin-vm/src/compiler.rs:2192`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_random**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14179`, `crates/goblin-vm/src/compiler.rs:2096`, `crates/goblin-vm/src/value.rs:468`, `crates/goblin-vm/src/builtins.rs:1798`, `crates/goblin-vm/src/compiler.rs:2097`, `crates/goblin-vm/src/vm.rs:1878`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **delete_where**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14155`, `crates/goblin-vm/src/compiler.rs:2066`, `crates/goblin-vm/src/value.rs:448`, `crates/goblin-vm/src/builtins.rs:1518`, `crates/goblin-vm/src/compiler.rs:2067`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **dirname**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13983`, `crates/goblin-vm/src/compiler.rs:2217`, `crates/goblin-vm/src/value.rs:635`, `crates/goblin-vm/src/builtins.rs:3024`, `crates/goblin-vm/src/compiler.rs:2218`, `crates/goblin-vm/src/vm.rs:1915`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **dups**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13793`, `crates/goblin-vm/src/compiler.rs:2017`, `crates/goblin-vm/src/value.rs:418`, `crates/goblin-vm/src/builtins.rs:1380`, `crates/goblin-vm/src/compiler.rs:2018`, `crates/goblin-vm/src/vm.rs:1818`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ends_with**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:16259`, `crates/goblin-vm/src/compiler.rs:1988`, `crates/goblin-vm/src/value.rs:378`, `crates/goblin-vm/src/builtins.rs:454`, `crates/goblin-vm/src/compiler.rs:1989`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **env**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13726`, `crates/goblin-vm/src/compiler.rs:2011`, `crates/goblin-vm/src/value.rs:401`, `crates/goblin-vm/src/builtins.rs:820`, `crates/goblin-vm/src/compiler.rs:2012`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **escape_html**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13994`, `crates/goblin-vm/src/compiler.rs:2226`, `crates/goblin-vm/src/value.rs:644`, `crates/goblin-vm/src/builtins.rs:3130`, `crates/goblin-vm/src/compiler.rs:2227`, `crates/goblin-vm/src/vm.rs:1923`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **Expr::Array**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18473`, `crates/goblin-vm/src/compiler.rs:820`, `crates/goblin-vm/src/compiler.rs:1785`, `crates/goblin-vm/src/compiler.rs:1786`, `crates/goblin-vm/src/compiler.rs:1829`, `crates/goblin-vm/src/compiler.rs:822`, `crates/goblin-vm/src/compiler.rs:1333`, `crates/goblin-vm/src/compiler.rs:1724`, `crates/goblin-vm/src/compiler.rs:1746`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:500`, `crates/goblin-vm/src/compiler.rs:831`, `crates/goblin-vm/src/compiler.rs:1053`, `crates/goblin-vm/src/debug.rs:62`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`, `crates/goblin-vm/src/compiler.rs:255`, `crates/goblin-vm/src/compiler.rs:288`, `crates/goblin-vm/src/compiler.rs:428`, `crates/goblin-vm/src/compiler.rs:466`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Binary**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17721`, `crates/goblin-vm/src/compiler.rs:963`, `crates/goblin-vm/src/compiler.rs:1148`, `crates/goblin-vm/src/compiler.rs:1151`, `crates/goblin-vm/src/compiler.rs:970`, `crates/goblin-vm/src/opcode.rs:255`, `crates/goblin-vm/src/vm.rs:810`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`, `crates/goblin-vm/src/compiler.rs:1165`, `crates/goblin-vm/src/opcode.rs:248`, `crates/goblin-vm/src/vm.rs:507`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Block**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18027`, `crates/goblin-vm/src/compiler.rs:181`, `crates/goblin-vm/src/compiler.rs:536`, `crates/goblin-vm/src/compiler.rs:994`, `crates/goblin-vm/src/compiler.rs:549`, `crates/goblin-vm/src/compiler.rs:915`, `crates/goblin-vm/src/compiler.rs:937`, `crates/goblin-vm/src/compiler.rs:1172`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Bool**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17889`, `crates/goblin-vm/src/compiler.rs:756`, `crates/goblin-vm/src/compiler.rs:757`, `crates/goblin-vm/src/vm.rs:1783`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/opcode.rs:204`, `crates/goblin-vm/src/vm.rs:166`, `crates/goblin-vm/src/opcode.rs:205`, `crates/goblin-vm/src/vm.rs:169`, `crates/goblin-vm/src/vm.rs:2008`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::BoxVar**
  Audience: language users. Parity status: `MISSING`.
  Source: `crates/goblin-interpreter/src/lib.rs:17973`, `crates/goblin-vm/src/compiler.rs:1070`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Call**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20327`, `crates/goblin-vm/src/compiler.rs:870`, `crates/goblin-vm/src/compiler.rs:1100`, `crates/goblin-vm/src/compiler.rs:866`, `crates/goblin-vm/src/compiler.rs:878`, `crates/goblin-vm/src/compiler.rs:907`, `crates/goblin-vm/src/compiler.rs:927`, `crates/goblin-vm/src/compiler.rs:884`, `crates/goblin-vm/src/opcode.rs:250`, `crates/goblin-vm/src/vm.rs:580`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Char**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17992`, `crates/goblin-vm/src/compiler.rs:808`, `crates/goblin-vm/src/vm.rs:1789`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:822`, `crates/goblin-vm/src/compiler.rs:1333`, `crates/goblin-vm/src/compiler.rs:1724`, `crates/goblin-vm/src/compiler.rs:1746`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::EnumVariant**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18303`, `crates/goblin-vm/src/compiler.rs:1041`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:500`, `crates/goblin-vm/src/compiler.rs:831`, `crates/goblin-vm/src/compiler.rs:1053`, `crates/goblin-vm/src/debug.rs:62`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::FreeCall**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:19112`, `crates/goblin-vm/src/compiler.rs:854`, `crates/goblin-vm/src/compiler.rs:1085`, `crates/goblin-vm/src/tick.rs:1057`, `crates/goblin-vm/src/tick.rs:1061`, `crates/goblin-vm/src/compiler.rs:866`, `crates/goblin-vm/src/compiler.rs:878`, `crates/goblin-vm/src/compiler.rs:907`, `crates/goblin-vm/src/compiler.rs:927`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Ident**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17993`, `crates/goblin-vm/src/compiler.rs:493`, `crates/goblin-vm/src/compiler.rs:611`, `crates/goblin-vm/src/compiler.rs:624`, `crates/goblin-vm/src/compiler.rs:814`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:500`, `crates/goblin-vm/src/compiler.rs:831`, `crates/goblin-vm/src/compiler.rs:1053`, `crates/goblin-vm/src/debug.rs:62`, `crates/goblin-vm/src/compiler.rs:263`, `crates/goblin-vm/src/compiler.rs:317`, `crates/goblin-vm/src/compiler.rs:322`, `crates/goblin-vm/src/compiler.rs:502`, `crates/goblin-vm/src/compiler.rs:511`, `crates/goblin-vm/src/opcode.rs:272`, `crates/goblin-vm/src/vm.rs:1081`, `crates/goblin-vm/src/compiler.rs:614`, `crates/goblin-vm/src/opcode.rs:264`, `crates/goblin-vm/src/vm.rs:980`, `crates/goblin-vm/src/compiler.rs:627`, `crates/goblin-vm/src/opcode.rs:265`, `crates/goblin-vm/src/vm.rs:1017`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Index**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18492`, `crates/goblin-vm/src/compiler.rs:835`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Index2**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17723`, `crates/goblin-vm/src/compiler.rs:1033`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::IndexMap**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18580`, `crates/goblin-vm/src/compiler.rs:841`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Judge**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18385`, `crates/goblin-vm/src/compiler.rs:182`, `crates/goblin-vm/src/compiler.rs:518`, `crates/goblin-vm/src/compiler.rs:989`, `crates/goblin-vm/src/compiler.rs:126`, `crates/goblin-vm/src/compiler.rs:135`, `crates/goblin-vm/src/compiler.rs:527`, `crates/goblin-vm/src/compiler.rs:681`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::LiteralToken**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17945`, `crates/goblin-vm/src/compiler.rs:1061`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Member**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18865`, `crates/goblin-vm/src/compiler.rs:847`, `crates/goblin-vm/src/compiler.rs:1148`, `crates/goblin-vm/src/compiler.rs:1150`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`, `crates/goblin-vm/src/compiler.rs:1165`, `crates/goblin-vm/src/opcode.rs:248`, `crates/goblin-vm/src/vm.rs:507`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Nil**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17888`, `crates/goblin-vm/src/compiler.rs:755`, `crates/goblin-vm/src/vm.rs:1782`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:756`, `crates/goblin-vm/src/opcode.rs:204`, `crates/goblin-vm/src/vm.rs:166`, `crates/goblin-vm/src/compiler.rs:757`, `crates/goblin-vm/src/opcode.rs:205`, `crates/goblin-vm/src/vm.rs:169`, `crates/goblin-vm/src/vm.rs:2008`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::NsCall**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18075`, `crates/goblin-vm/src/compiler.rs:888`, `crates/goblin-vm/src/compiler.rs:1299`, `crates/goblin-vm/src/compiler.rs:1355`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`, `crates/goblin-vm/src/compiler.rs:718`, `crates/goblin-vm/src/compiler.rs:918`, `crates/goblin-vm/src/compiler.rs:940`, `crates/goblin-vm/src/compiler.rs:1231`, `crates/goblin-vm/src/compiler.rs:126`, `crates/goblin-vm/src/compiler.rs:135`, `crates/goblin-vm/src/compiler.rs:527`, `crates/goblin-vm/src/compiler.rs:681`, `crates/goblin-vm/src/compiler.rs:684`, `crates/goblin-vm/src/compiler.rs:928`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Number**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17890`, `crates/goblin-vm/src/compiler.rs:759`, `crates/goblin-vm/src/vm.rs:1784`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Object**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18482`, `crates/goblin-vm/src/compiler.rs:825`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:500`, `crates/goblin-vm/src/compiler.rs:831`, `crates/goblin-vm/src/compiler.rs:1053`, `crates/goblin-vm/src/debug.rs:62`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::OptCall**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20389`, `crates/goblin-vm/src/compiler.rs:912`, `crates/goblin-vm/src/compiler.rs:549`, `crates/goblin-vm/src/compiler.rs:915`, `crates/goblin-vm/src/compiler.rs:937`, `crates/goblin-vm/src/compiler.rs:1172`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:718`, `crates/goblin-vm/src/compiler.rs:918`, `crates/goblin-vm/src/compiler.rs:940`, `crates/goblin-vm/src/compiler.rs:1231`, `crates/goblin-vm/src/compiler.rs:126`, `crates/goblin-vm/src/compiler.rs:135`, `crates/goblin-vm/src/compiler.rs:919`, `crates/goblin-vm/src/compiler.rs:941`, `crates/goblin-vm/src/compiler.rs:447`, `crates/goblin-vm/src/compiler.rs:665`, `crates/goblin-vm/src/compiler.rs:921`, `crates/goblin-vm/src/compiler.rs:931`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:866`, `crates/goblin-vm/src/compiler.rs:878`, `crates/goblin-vm/src/compiler.rs:907`, `crates/goblin-vm/src/compiler.rs:927`, `crates/goblin-vm/src/compiler.rs:684`, `crates/goblin-vm/src/compiler.rs:928`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::OptMember**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:19062`, `crates/goblin-vm/src/compiler.rs:935`, `crates/goblin-vm/src/compiler.rs:549`, `crates/goblin-vm/src/compiler.rs:915`, `crates/goblin-vm/src/compiler.rs:937`, `crates/goblin-vm/src/compiler.rs:1172`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:718`, `crates/goblin-vm/src/compiler.rs:918`, `crates/goblin-vm/src/compiler.rs:940`, `crates/goblin-vm/src/compiler.rs:1231`, `crates/goblin-vm/src/compiler.rs:126`, `crates/goblin-vm/src/compiler.rs:135`, `crates/goblin-vm/src/compiler.rs:919`, `crates/goblin-vm/src/compiler.rs:941`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`, `crates/goblin-vm/src/compiler.rs:684`, `crates/goblin-vm/src/compiler.rs:928`, `crates/goblin-vm/src/compiler.rs:447`, `crates/goblin-vm/src/compiler.rs:665`, `crates/goblin-vm/src/compiler.rs:921`, `crates/goblin-vm/src/compiler.rs:931`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Postfix**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20475`, `crates/goblin-vm/src/compiler.rs:967`, `crates/goblin-vm/src/compiler.rs:970`, `crates/goblin-vm/src/opcode.rs:255`, `crates/goblin-vm/src/vm.rs:810`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`, `crates/goblin-vm/src/compiler.rs:982`, `crates/goblin-vm/src/compiler.rs:1225`, `crates/goblin-vm/src/compiler.rs:1590`, `crates/goblin-vm/src/compiler.rs:1660`, `crates/goblin-vm/src/compiler.rs:1227`, `crates/goblin-vm/src/opcode.rs:216`, `crates/goblin-vm/src/vm.rs:291`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Prefix**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20411`, `crates/goblin-vm/src/compiler.rs:952`, `crates/goblin-vm/src/compiler.rs:955`, `crates/goblin-vm/src/opcode.rs:220`, `crates/goblin-vm/src/vm.rs:375`, `crates/goblin-vm/src/compiler.rs:956`, `crates/goblin-vm/src/compiler.rs:1456`, `crates/goblin-vm/src/opcode.rs:239`, `crates/goblin-vm/src/vm.rs:433`, `crates/goblin-vm/src/compiler.rs:970`, `crates/goblin-vm/src/opcode.rs:255`, `crates/goblin-vm/src/vm.rs:810`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Slice**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18721`, `crates/goblin-vm/src/compiler.rs:1016`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Slice3**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18776`, `crates/goblin-vm/src/compiler.rs:1024`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:898`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1020`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **Expr::Str**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:17926`, `crates/goblin-vm/src/compiler.rs:799`, `crates/goblin-vm/src/compiler.rs:1536`, `crates/goblin-vm/src/compiler.rs:1618`, `crates/goblin-vm/src/compiler.rs:1789`, `crates/goblin-vm/src/compiler.rs:802`, `crates/goblin-vm/src/opcode.rs:273`, `crates/goblin-vm/src/vm.rs:1099`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:255`, `crates/goblin-vm/src/compiler.rs:288`, `crates/goblin-vm/src/compiler.rs:428`, `crates/goblin-vm/src/compiler.rs:466`, `crates/goblin-vm/src/compiler.rs:372`, `crates/goblin-vm/src/compiler.rs:715`, `crates/goblin-vm/src/compiler.rs:729`, `crates/goblin-vm/src/compiler.rs:734`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: expression.
- [ ] **ext**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13982`, `crates/goblin-vm/src/compiler.rs:2219`, `crates/goblin-vm/src/value.rs:637`, `crates/goblin-vm/src/builtins.rs:3036`, `crates/goblin-vm/src/compiler.rs:2220`, `crates/goblin-vm/src/vm.rs:1917`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **f**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12409`, `crates/goblin-vm/src/compiler.rs:2210`, `crates/goblin-vm/src/value.rs:525`, `crates/goblin-vm/src/builtins.rs:2106`, `crates/goblin-vm/src/compiler.rs:2151`, `crates/goblin-vm/src/compiler.rs:2211`, `crates/goblin-vm/src/vm.rs:1802`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **f32**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12441`, `crates/goblin-vm/src/compiler.rs:2206`, `crates/goblin-vm/src/value.rs:627`, `crates/goblin-vm/src/builtins.rs:2961`, `crates/goblin-vm/src/compiler.rs:2207`, `crates/goblin-vm/src/vm.rs:1911`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **f64**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12442`, `crates/goblin-vm/src/compiler.rs:2207`, `crates/goblin-vm/src/value.rs:628`, `crates/goblin-vm/src/builtins.rs:2983`, `crates/goblin-vm/src/compiler.rs:2208`, `crates/goblin-vm/src/vm.rs:1912`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **file_exists**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13973`, `crates/goblin-vm/src/compiler.rs:2213`, `crates/goblin-vm/src/value.rs:631`, `crates/goblin-vm/src/builtins.rs:3003`, `crates/goblin-vm/src/compiler.rs:2214`, `crates/goblin-vm/src/vm.rs:1920`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **find**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13774`, `crates/goblin-vm/src/compiler.rs:2115`, `crates/goblin-vm/src/value.rs:365`, `crates/goblin-vm/src/builtins.rs:289`, `crates/goblin-vm/src/compiler.rs:2116`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **find_all**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13775`, `crates/goblin-vm/src/compiler.rs:1982`, `crates/goblin-vm/src/value.rs:366`, `crates/goblin-vm/src/builtins.rs:309`, `crates/goblin-vm/src/compiler.rs:1983`, `crates/goblin-vm/src/vm.rs:1893`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **float**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12409`, `crates/goblin-vm/src/compiler.rs:2210`, `crates/goblin-vm/src/value.rs:525`, `crates/goblin-vm/src/builtins.rs:2106`, `crates/goblin-vm/src/compiler.rs:2151`, `crates/goblin-vm/src/compiler.rs:2211`, `crates/goblin-vm/src/vm.rs:1802`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **floor**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12465`, `crates/goblin-vm/src/compiler.rs:1963`, `crates/goblin-vm/src/value.rs:348`, `crates/goblin-vm/src/builtins.rs:145`, `crates/goblin-vm/src/compiler.rs:1241`, `crates/goblin-vm/src/compiler.rs:1964`, `crates/goblin-vm/src/vm.rs:1829`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **format**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12150`, `crates/goblin-vm/src/compiler.rs:2164`, `crates/goblin-vm/src/value.rs:581`, `crates/goblin-vm/src/builtins.rs:2662`, `crates/goblin-vm/src/compiler.rs:2165`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **format_info**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12369`, `crates/goblin-vm/src/compiler.rs:2238`, `crates/goblin-vm/src/value.rs:658`, `crates/goblin-vm/src/builtins.rs:3240`, `crates/goblin-vm/src/compiler.rs:2239`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **freq**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13788`, `crates/goblin-vm/src/compiler.rs:2014`, `crates/goblin-vm/src/value.rs:413`, `crates/goblin-vm/src/builtins.rs:1237`, `crates/goblin-vm/src/compiler.rs:2015`, `crates/goblin-vm/src/vm.rs:1888`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get**
  Audience: language users. Parity status: `MISSING`.
  Source: `crates/goblin-interpreter/src/lib.rs:13999`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_all**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14025`, `crates/goblin-vm/src/compiler.rs:2080`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1685`, `crates/goblin-vm/src/compiler.rs:2081`, `crates/goblin-vm/src/vm.rs:1874`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_at**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14014`, `crates/goblin-vm/src/compiler.rs:2078`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1670`, `crates/goblin-vm/src/compiler.rs:2079`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14036`, `crates/goblin-vm/src/compiler.rs:2082`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1699`, `crates/goblin-vm/src/compiler.rs:2083`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_first**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14004`, `crates/goblin-vm/src/compiler.rs:2076`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1660`, `crates/goblin-vm/src/compiler.rs:2077`, `crates/goblin-vm/src/vm.rs:1872`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14009`, `crates/goblin-vm/src/compiler.rs:2077`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1665`, `crates/goblin-vm/src/compiler.rs:2078`, `crates/goblin-vm/src/vm.rs:1873`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14030`, `crates/goblin-vm/src/compiler.rs:2081`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1690`, `crates/goblin-vm/src/compiler.rs:2082`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_random**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14043`, `crates/goblin-vm/src/compiler.rs:2083`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1706`, `crates/goblin-vm/src/compiler.rs:2084`, `crates/goblin-vm/src/vm.rs:1875`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **get_where**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14019`, `crates/goblin-vm/src/compiler.rs:2079`, `crates/goblin-vm/src/value.rs:462`, `crates/goblin-vm/src/builtins.rs:1676`, `crates/goblin-vm/src/compiler.rs:2080`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13811`, `crates/goblin-vm/src/compiler.rs:2279`, `crates/goblin-vm/src/value.rs:701`, `crates/goblin-vm/src/builtins.rs:3478`, `crates/goblin-vm/src/compiler.rs:2280`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_count**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13822`, `crates/goblin-vm/src/compiler.rs:2294`, `crates/goblin-vm/src/value.rs:716`, `crates/goblin-vm/src/builtins.rs:3685`, `crates/goblin-vm/src/compiler.rs:2295`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_default_get**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13829`, `crates/goblin-vm/src/compiler.rs:2287`, `crates/goblin-vm/src/value.rs:709`, `crates/goblin-vm/src/builtins.rs:3618`, `crates/goblin-vm/src/compiler.rs:2288`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_default_set**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13830`, `crates/goblin-vm/src/compiler.rs:2288`, `crates/goblin-vm/src/value.rs:710`, `crates/goblin-vm/src/builtins.rs:3630`, `crates/goblin-vm/src/compiler.rs:2289`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_get**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13814`, `crates/goblin-vm/src/compiler.rs:2280`, `crates/goblin-vm/src/value.rs:702`, `crates/goblin-vm/src/builtins.rs:3520`, `crates/goblin-vm/src/compiler.rs:2281`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_has**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13824`, `crates/goblin-vm/src/compiler.rs:2296`, `crates/goblin-vm/src/value.rs:718`, `crates/goblin-vm/src/builtins.rs:3706`, `crates/goblin-vm/src/compiler.rs:2297`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_info**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13831`, `crates/goblin-vm/src/compiler.rs:2297`, `crates/goblin-vm/src/value.rs:719`, `crates/goblin-vm/src/builtins.rs:3715`, `crates/goblin-vm/src/compiler.rs:2298`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_neighbors**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13817`, `crates/goblin-vm/src/compiler.rs:2289`, `crates/goblin-vm/src/value.rs:711`, `crates/goblin-vm/src/builtins.rs:3641`, `crates/goblin-vm/src/compiler.rs:2290`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_occupied**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13818`, `crates/goblin-vm/src/compiler.rs:2290`, `crates/goblin-vm/src/value.rs:712`, `crates/goblin-vm/src/builtins.rs:3655`, `crates/goblin-vm/src/compiler.rs:2291`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_occupied_by**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13823`, `crates/goblin-vm/src/compiler.rs:2295`, `crates/goblin-vm/src/value.rs:717`, `crates/goblin-vm/src/builtins.rs:3694`, `crates/goblin-vm/src/compiler.rs:2296`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_occupied_count**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13820`, `crates/goblin-vm/src/compiler.rs:2292`, `crates/goblin-vm/src/value.rs:714`, `crates/goblin-vm/src/builtins.rs:3673`, `crates/goblin-vm/src/compiler.rs:2293`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_region_get**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13827`, `crates/goblin-vm/src/compiler.rs:2285`, `crates/goblin-vm/src/value.rs:707`, `crates/goblin-vm/src/builtins.rs:3591`, `crates/goblin-vm/src/compiler.rs:2286`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_region_info**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13833`, `crates/goblin-vm/src/compiler.rs:2299`, `crates/goblin-vm/src/value.rs:721`, `crates/goblin-vm/src/builtins.rs:3768`, `crates/goblin-vm/src/compiler.rs:2300`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_region_set**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13828`, `crates/goblin-vm/src/compiler.rs:2286`, `crates/goblin-vm/src/value.rs:708`, `crates/goblin-vm/src/builtins.rs:3605`, `crates/goblin-vm/src/compiler.rs:2287`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_set**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13815`, `crates/goblin-vm/src/compiler.rs:2281`, `crates/goblin-vm/src/value.rs:703`, `crates/goblin-vm/src/builtins.rs:3536`, `crates/goblin-vm/src/compiler.rs:2282`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_tile_get**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13825`, `crates/goblin-vm/src/compiler.rs:2283`, `crates/goblin-vm/src/value.rs:705`, `crates/goblin-vm/src/builtins.rs:3564`, `crates/goblin-vm/src/compiler.rs:2284`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_tile_info**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13832`, `crates/goblin-vm/src/compiler.rs:2298`, `crates/goblin-vm/src/value.rs:720`, `crates/goblin-vm/src/builtins.rs:3747`, `crates/goblin-vm/src/compiler.rs:2299`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_tile_set**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13826`, `crates/goblin-vm/src/compiler.rs:2284`, `crates/goblin-vm/src/value.rs:706`, `crates/goblin-vm/src/builtins.rs:3578`, `crates/goblin-vm/src/compiler.rs:2285`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_unoccupied**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13819`, `crates/goblin-vm/src/compiler.rs:2291`, `crates/goblin-vm/src/value.rs:713`, `crates/goblin-vm/src/builtins.rs:3664`, `crates/goblin-vm/src/compiler.rs:2292`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_unoccupied_count**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13821`, `crates/goblin-vm/src/compiler.rs:2293`, `crates/goblin-vm/src/value.rs:715`, `crates/goblin-vm/src/builtins.rs:3679`, `crates/goblin-vm/src/compiler.rs:2294`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **grid_void**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13816`, `crates/goblin-vm/src/compiler.rs:2282`, `crates/goblin-vm/src/value.rs:704`, `crates/goblin-vm/src/builtins.rs:3552`, `crates/goblin-vm/src/compiler.rs:2283`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **has**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13784`, `crates/goblin-vm/src/compiler.rs:2099`, `crates/goblin-vm/src/value.rs:409`, `crates/goblin-vm/src/builtins.rs:1096`, `crates/goblin-vm/src/compiler.rs:2100`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **highlight_code**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11233`, `crates/goblin-vm/src/compiler.rs:2193`, `crates/goblin-vm/src/value.rs:613`, `crates/goblin-vm/src/builtins.rs:2838`, `crates/goblin-vm/src/compiler.rs:2194`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **i**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12405`, `crates/goblin-vm/src/compiler.rs:2209`, `crates/goblin-vm/src/value.rs:524`, `crates/goblin-vm/src/builtins.rs:2087`, `crates/goblin-vm/src/compiler.rs:2150`, `crates/goblin-vm/src/compiler.rs:2210`, `crates/goblin-vm/src/vm.rs:1801`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **i16**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12434`, `crates/goblin-vm/src/compiler.rs:2199`, `crates/goblin-vm/src/value.rs:620`, `crates/goblin-vm/src/builtins.rs:2909`, `crates/goblin-vm/src/compiler.rs:2200`, `crates/goblin-vm/src/vm.rs:1904`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **i32**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12435`, `crates/goblin-vm/src/compiler.rs:2200`, `crates/goblin-vm/src/value.rs:621`, `crates/goblin-vm/src/builtins.rs:2917`, `crates/goblin-vm/src/compiler.rs:2201`, `crates/goblin-vm/src/vm.rs:1905`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **i64**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12436`, `crates/goblin-vm/src/compiler.rs:2201`, `crates/goblin-vm/src/value.rs:622`, `crates/goblin-vm/src/builtins.rs:2925`, `crates/goblin-vm/src/compiler.rs:2202`, `crates/goblin-vm/src/vm.rs:1906`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **i8**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12433`, `crates/goblin-vm/src/compiler.rs:2198`, `crates/goblin-vm/src/value.rs:619`, `crates/goblin-vm/src/builtins.rs:2901`, `crates/goblin-vm/src/compiler.rs:2199`, `crates/goblin-vm/src/vm.rs:1903`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15576`, `crates/goblin-vm/src/compiler.rs:2009`, `crates/goblin-vm/src/value.rs:399`, `crates/goblin-vm/src/builtins.rs:678`, `crates/goblin-vm/src/compiler.rs:2010`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_blocks**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15693`, `crates/goblin-vm/src/compiler.rs:2010`, `crates/goblin-vm/src/value.rs:400`, `crates/goblin-vm/src/builtins.rs:743`, `crates/goblin-vm/src/compiler.rs:2011`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_blocks_first**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15829`, `crates/goblin-vm/src/compiler.rs:2172`, `crates/goblin-vm/src/value.rs:591`, `crates/goblin-vm/src/builtins.rs:869`, `crates/goblin-vm/src/compiler.rs:2173`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_lines_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15505`, `crates/goblin-vm/src/compiler.rs:2002`, `crates/goblin-vm/src/value.rs:392`, `crates/goblin-vm/src/builtins.rs:635`, `crates/goblin-vm/src/compiler.rs:2003`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_lines_where**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15421`, `crates/goblin-vm/src/compiler.rs:2000`, `crates/goblin-vm/src/value.rs:390`, `crates/goblin-vm/src/builtins.rs:597`, `crates/goblin-vm/src/compiler.rs:2001`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15438`, `crates/goblin-vm/src/compiler.rs:2001`, `crates/goblin-vm/src/value.rs:391`, `crates/goblin-vm/src/builtins.rs:627`, `crates/goblin-vm/src/compiler.rs:2002`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **ignore_where**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15413`, `crates/goblin-vm/src/compiler.rs:1999`, `crates/goblin-vm/src/value.rs:389`, `crates/goblin-vm/src/builtins.rs:588`, `crates/goblin-vm/src/compiler.rs:2000`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **input**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11159`, `crates/goblin-vm/src/compiler.rs:2231`, `crates/goblin-vm/src/value.rs:650`, `crates/goblin-vm/src/builtins.rs:3194`, `crates/goblin-vm/src/compiler.rs:2232`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **int**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12405`, `crates/goblin-vm/src/compiler.rs:2209`, `crates/goblin-vm/src/value.rs:524`, `crates/goblin-vm/src/builtins.rs:2087`, `crates/goblin-vm/src/compiler.rs:2150`, `crates/goblin-vm/src/compiler.rs:2210`, `crates/goblin-vm/src/vm.rs:1801`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **invoke**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11242`, `crates/goblin-vm/src/compiler.rs:2180`, `crates/goblin-vm/src/value.rs:600`, `crates/goblin-vm/src/builtins.rs:2752`, `crates/goblin-vm/src/compiler.rs:2181`, `crates/goblin-vm/src/vm.rs:718`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_alnum**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11985`, `crates/goblin-vm/src/compiler.rs:2139`, `crates/goblin-vm/src/value.rs:512`, `crates/goblin-vm/src/builtins.rs:1995`, `crates/goblin-vm/src/compiler.rs:2140`, `crates/goblin-vm/src/vm.rs:1841`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_alpha**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11792`, `crates/goblin-vm/src/compiler.rs:2140`, `crates/goblin-vm/src/value.rs:513`, `crates/goblin-vm/src/builtins.rs:2003`, `crates/goblin-vm/src/compiler.rs:2141`, `crates/goblin-vm/src/vm.rs:1842`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_array**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11719`, `crates/goblin-vm/src/compiler.rs:2019`, `crates/goblin-vm/src/value.rs:501`, `crates/goblin-vm/src/builtins.rs:1984`, `crates/goblin-vm/src/compiler.rs:1525`, `crates/goblin-vm/src/compiler.rs:2020`, `crates/goblin-vm/src/vm.rs:1869`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_big**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11670`, `crates/goblin-vm/src/compiler.rs:2132`, `crates/goblin-vm/src/value.rs:505`, `crates/goblin-vm/src/builtins.rs:1988`, `crates/goblin-vm/src/compiler.rs:2133`, `crates/goblin-vm/src/vm.rs:1834`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_bool**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11634`, `crates/goblin-vm/src/compiler.rs:2126`, `crates/goblin-vm/src/value.rs:497`, `crates/goblin-vm/src/builtins.rs:1980`, `crates/goblin-vm/src/compiler.rs:2127`, `crates/goblin-vm/src/vm.rs:1865`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_bound_name**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11058`, `crates/goblin-vm/src/compiler.rs:2179`, `crates/goblin-vm/src/value.rs:599`, `crates/goblin-vm/src/builtins.rs:2746`, `crates/goblin-vm/src/compiler.rs:2180`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_char**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11710`, `crates/goblin-vm/src/compiler.rs:2135`, `crates/goblin-vm/src/value.rs:508`, `crates/goblin-vm/src/builtins.rs:1991`, `crates/goblin-vm/src/compiler.rs:2136`, `crates/goblin-vm/src/vm.rs:1837`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_control**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11764`, `crates/goblin-vm/src/compiler.rs:2171`, `crates/goblin-vm/src/value.rs:590`, `crates/goblin-vm/src/builtins.rs:863`, `crates/goblin-vm/src/compiler.rs:2172`, `crates/goblin-vm/src/vm.rs:1898`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_digit**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11777`, `crates/goblin-vm/src/compiler.rs:2141`, `crates/goblin-vm/src/value.rs:514`, `crates/goblin-vm/src/builtins.rs:2012`, `crates/goblin-vm/src/compiler.rs:2142`, `crates/goblin-vm/src/vm.rs:1843`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_dir**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13988`, `crates/goblin-vm/src/compiler.rs:2215`, `crates/goblin-vm/src/value.rs:633`, `crates/goblin-vm/src/builtins.rs:3013`, `crates/goblin-vm/src/compiler.rs:2216`, `crates/goblin-vm/src/vm.rs:1922`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_even**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11822`, `crates/goblin-vm/src/compiler.rs:2143`, `crates/goblin-vm/src/value.rs:516`, `crates/goblin-vm/src/builtins.rs:2028`, `crates/goblin-vm/src/compiler.rs:2144`, `crates/goblin-vm/src/vm.rs:1845`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_file**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13987`, `crates/goblin-vm/src/compiler.rs:2214`, `crates/goblin-vm/src/value.rs:632`, `crates/goblin-vm/src/builtins.rs:3008`, `crates/goblin-vm/src/compiler.rs:2215`, `crates/goblin-vm/src/vm.rs:1921`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_float**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11656`, `crates/goblin-vm/src/compiler.rs:2128`, `crates/goblin-vm/src/value.rs:499`, `crates/goblin-vm/src/builtins.rs:1982`, `crates/goblin-vm/src/compiler.rs:2129`, `crates/goblin-vm/src/vm.rs:1867`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_int**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11643`, `crates/goblin-vm/src/compiler.rs:2127`, `crates/goblin-vm/src/value.rs:498`, `crates/goblin-vm/src/builtins.rs:1981`, `crates/goblin-vm/src/compiler.rs:2128`, `crates/goblin-vm/src/vm.rs:1866`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_map**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11728`, `crates/goblin-vm/src/compiler.rs:2020`, `crates/goblin-vm/src/value.rs:502`, `crates/goblin-vm/src/builtins.rs:1985`, `crates/goblin-vm/src/compiler.rs:2021`, `crates/goblin-vm/src/vm.rs:1870`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15069`, `crates/goblin-vm/src/compiler.rs:2003`, `crates/goblin-vm/src/value.rs:393`, `crates/goblin-vm/src/builtins.rs:613`, `crates/goblin-vm/src/compiler.rs:2004`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_multiple_of**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11886`, `crates/goblin-vm/src/compiler.rs:2145`, `crates/goblin-vm/src/value.rs:518`, `crates/goblin-vm/src/builtins.rs:2044`, `crates/goblin-vm/src/compiler.rs:2146`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_negative**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11967`, `crates/goblin-vm/src/compiler.rs:2147`, `crates/goblin-vm/src/value.rs:520`, `crates/goblin-vm/src/builtins.rs:2065`, `crates/goblin-vm/src/compiler.rs:2148`, `crates/goblin-vm/src/vm.rs:1848`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_nil**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11625`, `crates/goblin-vm/src/compiler.rs:2125`, `crates/goblin-vm/src/value.rs:496`, `crates/goblin-vm/src/builtins.rs:1979`, `crates/goblin-vm/src/compiler.rs:2126`, `crates/goblin-vm/src/vm.rs:1864`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_nix**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12014`, `crates/goblin-vm/src/compiler.rs:2148`, `crates/goblin-vm/src/value.rs:521`, `crates/goblin-vm/src/builtins.rs:2074`, `crates/goblin-vm/src/compiler.rs:2149`, `crates/goblin-vm/src/vm.rs:1849`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_num**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11688`, `crates/goblin-vm/src/compiler.rs:2134`, `crates/goblin-vm/src/value.rs:507`, `crates/goblin-vm/src/builtins.rs:1990`, `crates/goblin-vm/src/compiler.rs:2135`, `crates/goblin-vm/src/vm.rs:1836`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_odd**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11855`, `crates/goblin-vm/src/compiler.rs:2144`, `crates/goblin-vm/src/value.rs:517`, `crates/goblin-vm/src/builtins.rs:2036`, `crates/goblin-vm/src/compiler.rs:2145`, `crates/goblin-vm/src/vm.rs:1846`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_pair**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11737`, `crates/goblin-vm/src/compiler.rs:2136`, `crates/goblin-vm/src/value.rs:509`, `crates/goblin-vm/src/builtins.rs:1992`, `crates/goblin-vm/src/compiler.rs:2137`, `crates/goblin-vm/src/vm.rs:1838`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_pct**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11679`, `crates/goblin-vm/src/compiler.rs:2133`, `crates/goblin-vm/src/value.rs:506`, `crates/goblin-vm/src/builtins.rs:1989`, `crates/goblin-vm/src/compiler.rs:2134`, `crates/goblin-vm/src/vm.rs:1835`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_positive**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11949`, `crates/goblin-vm/src/compiler.rs:2146`, `crates/goblin-vm/src/value.rs:519`, `crates/goblin-vm/src/builtins.rs:2056`, `crates/goblin-vm/src/compiler.rs:2147`, `crates/goblin-vm/src/vm.rs:1847`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_seq**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11746`, `crates/goblin-vm/src/compiler.rs:2137`, `crates/goblin-vm/src/value.rs:510`, `crates/goblin-vm/src/builtins.rs:1993`, `crates/goblin-vm/src/compiler.rs:2138`, `crates/goblin-vm/src/vm.rs:1839`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_str**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11701`, `crates/goblin-vm/src/compiler.rs:2129`, `crates/goblin-vm/src/value.rs:500`, `crates/goblin-vm/src/builtins.rs:1983`, `crates/goblin-vm/src/compiler.rs:2130`, `crates/goblin-vm/src/vm.rs:1868`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_type**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11096`, `crates/goblin-vm/src/compiler.rs:2178`, `crates/goblin-vm/src/value.rs:598`, `crates/goblin-vm/src/builtins.rs:2727`, `crates/goblin-vm/src/compiler.rs:2179`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_unit**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11755`, `crates/goblin-vm/src/compiler.rs:2138`, `crates/goblin-vm/src/value.rs:511`, `crates/goblin-vm/src/builtins.rs:1994`, `crates/goblin-vm/src/compiler.rs:2139`, `crates/goblin-vm/src/vm.rs:1840`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **is_whitespace**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11998`, `crates/goblin-vm/src/compiler.rs:2142`, `crates/goblin-vm/src/value.rs:515`, `crates/goblin-vm/src/builtins.rs:2020`, `crates/goblin-vm/src/compiler.rs:2143`, `crates/goblin-vm/src/vm.rs:1844`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **items**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13781`, `crates/goblin-vm/src/compiler.rs:2012`, `crates/goblin-vm/src/value.rs:406`, `crates/goblin-vm/src/builtins.rs:1076`, `crates/goblin-vm/src/compiler.rs:2013`, `crates/goblin-vm/src/vm.rs:1823`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **join**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14990`, `crates/goblin-vm/src/compiler.rs:1985`, `crates/goblin-vm/src/value.rs:375`, `crates/goblin-vm/src/builtins.rs:399`, `crates/goblin-vm/src/compiler.rs:1986`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **json_parse**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14392`, `crates/goblin-vm/src/compiler.rs:2006`, `crates/goblin-vm/src/value.rs:396`, `crates/goblin-vm/src/builtins.rs:659`, `crates/goblin-vm/src/compiler.rs:2007`, `crates/goblin-vm/src/vm.rs:1894`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **json_stringify**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14426`, `crates/goblin-vm/src/compiler.rs:2007`, `crates/goblin-vm/src/value.rs:397`, `crates/goblin-vm/src/builtins.rs:665`, `crates/goblin-vm/src/compiler.rs:2008`, `crates/goblin-vm/src/vm.rs:1860`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **json_stringify_pretty**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14457`, `crates/goblin-vm/src/compiler.rs:2008`, `crates/goblin-vm/src/value.rs:398`, `crates/goblin-vm/src/builtins.rs:671`, `crates/goblin-vm/src/compiler.rs:2009`, `crates/goblin-vm/src/vm.rs:1861`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **keep_after**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:16075`, `crates/goblin-vm/src/compiler.rs:1995`, `crates/goblin-vm/src/value.rs:385`, `crates/goblin-vm/src/builtins.rs:526`, `crates/goblin-vm/src/compiler.rs:1996`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **keep_before**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:16046`, `crates/goblin-vm/src/compiler.rs:1994`, `crates/goblin-vm/src/value.rs:384`, `crates/goblin-vm/src/builtins.rs:513`, `crates/goblin-vm/src/compiler.rs:1995`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **keep_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:16110`, `crates/goblin-vm/src/compiler.rs:1996`, `crates/goblin-vm/src/value.rs:386`, `crates/goblin-vm/src/builtins.rs:539`, `crates/goblin-vm/src/compiler.rs:1997`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **keep_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15979`, `crates/goblin-vm/src/compiler.rs:2005`, `crates/goblin-vm/src/value.rs:395`, `crates/goblin-vm/src/builtins.rs:648`, `crates/goblin-vm/src/compiler.rs:2006`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **keys**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13779`, `crates/goblin-vm/src/compiler.rs:2100`, `crates/goblin-vm/src/value.rs:404`, `crates/goblin-vm/src/builtins.rs:1058`, `crates/goblin-vm/src/compiler.rs:2101`, `crates/goblin-vm/src/vm.rs:1821`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **len**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15268`, `crates/goblin-vm/src/compiler.rs:1969`, `crates/goblin-vm/src/value.rs:370`, `crates/goblin-vm/src/builtins.rs:346`, `crates/goblin-vm/src/compiler.rs:1970`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **lines**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14886`, `crates/goblin-vm/src/compiler.rs:2161`, `crates/goblin-vm/src/value.rs:578`, `crates/goblin-vm/src/builtins.rs:2638`, `crates/goblin-vm/src/compiler.rs:2162`, `crates/goblin-vm/src/vm.rs:1824`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **link_score**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6768`, `crates/goblin-vm/src/compiler.rs:2267`, `crates/goblin-vm/src/value.rs:678`, `crates/goblin-vm/src/builtins.rs:3472`, `crates/goblin-vm/src/compiler.rs:2268`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **list_dirs**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13993`, `crates/goblin-vm/src/compiler.rs:2225`, `crates/goblin-vm/src/value.rs:643`, `crates/goblin-vm/src/builtins.rs:3112`, `crates/goblin-vm/src/compiler.rs:2226`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **list_tokens**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6092`, `crates/goblin-vm/src/compiler.rs:2252`, `crates/goblin-vm/src/value.rs:672`, `crates/goblin-vm/src/builtins.rs:3385`, `crates/goblin-vm/src/compiler.rs:2253`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **lower**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13766`, `crates/goblin-vm/src/compiler.rs:1976`, `crates/goblin-vm/src/value.rs:356`, `crates/goblin-vm/src/builtins.rs:202`, `crates/goblin-vm/src/compiler.rs:1977`, `crates/goblin-vm/src/vm.rs:1805`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **m**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12425`, `crates/goblin-vm/src/compiler.rs:2195`, `crates/goblin-vm/src/value.rs:615`, `crates/goblin-vm/src/builtins.rs:2870`, `crates/goblin-vm/src/compiler.rs:2196`, `crates/goblin-vm/src/vm.rs:1891`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **map**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13791`, `crates/goblin-vm/src/compiler.rs:2110`, `crates/goblin-vm/src/value.rs:416`, `crates/goblin-vm/src/builtins.rs:1326`, `crates/goblin-vm/src/compiler.rs:2111`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **max**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6514`, `crates/goblin-vm/src/compiler.rs:1960`, `crates/goblin-vm/src/value.rs:345`, `crates/goblin-vm/src/builtins.rs:97`, `crates/goblin-vm/src/compiler.rs:1961`, `crates/goblin-vm/src/vm.rs:1886`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **md_to_html**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11225`, `crates/goblin-vm/src/compiler.rs:2192`, `crates/goblin-vm/src/value.rs:612`, `crates/goblin-vm/src/builtins.rs:2832`, `crates/goblin-vm/src/compiler.rs:2193`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **mem_addr**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13801`, `crates/goblin-vm/src/compiler.rs:1956`, `crates/goblin-vm/src/value.rs:337`, `crates/goblin-vm/src/builtins.rs:39`, `crates/goblin-vm/src/compiler.rs:1957`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **mem_human**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13803`, `crates/goblin-vm/src/compiler.rs:2022`, `crates/goblin-vm/src/value.rs:339`, `crates/goblin-vm/src/builtins.rs:46`, `crates/goblin-vm/src/compiler.rs:2023`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **mem_total**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13802`, `crates/goblin-vm/src/compiler.rs:2021`, `crates/goblin-vm/src/value.rs:338`, `crates/goblin-vm/src/builtins.rs:43`, `crates/goblin-vm/src/compiler.rs:2022`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **member access**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:18865`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`, `crates/goblin-vm/src/debug.rs:63`, `crates/goblin-vm/src/debug.rs:67`, `crates/goblin-vm/src/opcode.rs:108`, `crates/goblin-vm/src/opcode.rs:247`, `crates/goblin-vm/src/vm.rs:623`, `crates/goblin-vm/src/vm.rs:630`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: member/object runtime.
- [ ] **metrics**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15338`, `crates/goblin-vm/src/compiler.rs:2241`, `crates/goblin-vm/src/value.rs:661`, `crates/goblin-vm/src/builtins.rs:3274`, `crates/goblin-vm/src/compiler.rs:2242`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **min**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6432`, `crates/goblin-vm/src/compiler.rs:1959`, `crates/goblin-vm/src/value.rs:344`, `crates/goblin-vm/src/builtins.rs:63`, `crates/goblin-vm/src/compiler.rs:1960`, `crates/goblin-vm/src/vm.rs:1885`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **minimize**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15217`, `crates/goblin-vm/src/compiler.rs:2106`, `crates/goblin-vm/src/value.rs:477`, `crates/goblin-vm/src/builtins.rs:1875`, `crates/goblin-vm/src/compiler.rs:2107`, `crates/goblin-vm/src/vm.rs:1857`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **mixed**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13770`, `crates/goblin-vm/src/compiler.rs:1980`, `crates/goblin-vm/src/value.rs:360`, `crates/goblin-vm/src/builtins.rs:246`, `crates/goblin-vm/src/compiler.rs:1981`, `crates/goblin-vm/src/vm.rs:1809`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **mode**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13789`, `crates/goblin-vm/src/compiler.rs:2015`, `crates/goblin-vm/src/value.rs:414`, `crates/goblin-vm/src/builtins.rs:1257`, `crates/goblin-vm/src/compiler.rs:2016`, `crates/goblin-vm/src/vm.rs:1889`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **normalize_newlines**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15399`, `crates/goblin-vm/src/compiler.rs:1998`, `crates/goblin-vm/src/value.rs:388`, `crates/goblin-vm/src/builtins.rs:583`, `crates/goblin-vm/src/compiler.rs:1999`, `crates/goblin-vm/src/vm.rs:1859`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **object instantiation**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:22098`, `crates/goblin-vm/src/builtins.rs:2387`, `crates/goblin-vm/src/builtins.rs:2406`, `crates/goblin-vm/src/builtins.rs:3337`, `crates/goblin-vm/src/builtins.rs:3338`, `crates/goblin-vm/src/builtins.rs:3404`, `crates/goblin-vm/src/builtins.rs:3419`, `crates/goblin-vm/src/builtins.rs:3424`, `crates/goblin-vm/src/builtins.rs:3425`, `crates/goblin-vm/src/builtins.rs:3435`, `crates/goblin-vm/src/builtins.rs:4204`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: member/object runtime.
- [ ] **object method dispatch**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21825`, `crates/goblin-vm/src/compiler.rs:873`, `crates/goblin-vm/src/compiler.rs:884`, `crates/goblin-vm/src/opcode.rs:115`, `crates/goblin-vm/src/opcode.rs:250`, `crates/goblin-vm/src/vm.rs:580`, `crates/goblin-vm/src/vm.rs:585`, `crates/goblin-vm/src/vm.rs:630`, `crates/goblin-vm/src/vm.rs:1794`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: member/object runtime.
- [ ] **object method dispatch with AST args**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21967`, `crates/goblin-vm/src/compiler.rs:873`, `crates/goblin-vm/src/compiler.rs:884`, `crates/goblin-vm/src/opcode.rs:115`, `crates/goblin-vm/src/opcode.rs:250`, `crates/goblin-vm/src/vm.rs:580`, `crates/goblin-vm/src/vm.rs:585`, `crates/goblin-vm/src/vm.rs:630`, `crates/goblin-vm/src/vm.rs:1794`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: member/object runtime.
- [ ] **operator !**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20442`, `crates/goblin-vm/src/compiler.rs:956`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator !=**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21500`, `crates/goblin-vm/src/compiler.rs:1232`, `crates/goblin-vm/src/opcode.rs:234`, `crates/goblin-vm/src/vm.rs:409`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator !==**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21491`, `crates/goblin-vm/src/compiler.rs:1232`, `crates/goblin-vm/src/opcode.rs:234`, `crates/goblin-vm/src/vm.rs:409`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator %**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20562`, `crates/goblin-vm/src/compiler.rs:970`, `crates/goblin-vm/src/compiler.rs:1230`, `crates/goblin-vm/src/opcode.rs:219`, `crates/goblin-vm/src/vm.rs:357`, `crates/goblin-vm/src/vm.rs:1266`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator %o**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21443`, `crates/goblin-vm/src/compiler.rs:1252`, `crates/goblin-vm/src/compiler.rs:1228`, `crates/goblin-vm/src/opcode.rs:217`, `crates/goblin-vm/src/vm.rs:305`, `crates/goblin-vm/src/vm.rs:1262`, `crates/goblin-vm/src/vm.rs:1263`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator &&**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21582`, `crates/goblin-vm/src/compiler.rs:1191`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ***
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20966`, `crates/goblin-vm/src/compiler.rs:1228`, `crates/goblin-vm/src/compiler.rs:1252`, `crates/goblin-vm/src/opcode.rs:217`, `crates/goblin-vm/src/vm.rs:305`, `crates/goblin-vm/src/vm.rs:1262`, `crates/goblin-vm/src/vm.rs:1263`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ****
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20566`, `crates/goblin-vm/src/compiler.rs:971`, `crates/goblin-vm/src/compiler.rs:1246`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator +**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20429`, `crates/goblin-vm/src/compiler.rs:1225`, `crates/goblin-vm/src/vm.rs:274`, `crates/goblin-vm/src/compiler.rs:982`, `crates/goblin-vm/src/compiler.rs:1590`, `crates/goblin-vm/src/compiler.rs:1660`, `crates/goblin-vm/src/compiler.rs:1765`, `crates/goblin-vm/src/opcode.rs:215`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ++**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20479`, `crates/goblin-vm/src/compiler.rs:978`, `crates/goblin-vm/src/compiler.rs:982`, `crates/goblin-vm/src/compiler.rs:1226`, `crates/goblin-vm/src/opcode.rs:232`, `crates/goblin-vm/src/vm.rs:394`, `crates/goblin-vm/src/vm.rs:1259`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator -**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20413`, `crates/goblin-vm/src/builtins.rs:4331`, `crates/goblin-vm/src/compiler.rs:955`, `crates/goblin-vm/src/compiler.rs:982`, `crates/goblin-vm/src/compiler.rs:1227`, `crates/goblin-vm/src/opcode.rs:216`, `crates/goblin-vm/src/vm.rs:291`, `crates/goblin-vm/src/vm.rs:1260`, `crates/goblin-vm/src/vm.rs:1261`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ..**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21778`, `crates/goblin-vm/src/builtins.rs:3068`, `crates/goblin-vm/src/builtins.rs:3161`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator /**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21006`, `crates/goblin-vm/src/builtins.rs:3027`, `crates/goblin-vm/src/builtins.rs:3049`, `crates/goblin-vm/src/compiler.rs:1229`, `crates/goblin-vm/src/compiler.rs:1240`, `crates/goblin-vm/src/opcode.rs:218`, `crates/goblin-vm/src/vm.rs:319`, `crates/goblin-vm/src/vm.rs:1264`, `crates/goblin-vm/src/vm.rs:1265`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator //**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:20567`, `crates/goblin-vm/src/builtins.rs:3184`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1229`, `crates/goblin-vm/src/compiler.rs:1240`, `crates/goblin-vm/src/opcode.rs:218`, `crates/goblin-vm/src/vm.rs:319`, `crates/goblin-vm/src/vm.rs:1264`, `crates/goblin-vm/src/vm.rs:1265`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator /=**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21674`, `crates/goblin-vm/src/compiler.rs:1232`, `crates/goblin-vm/src/opcode.rs:234`, `crates/goblin-vm/src/vm.rs:409`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator <**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21523`, `crates/goblin-vm/src/compiler.rs:1233`, `crates/goblin-vm/src/vm.rs:415`, `crates/goblin-vm/src/compiler.rs:1565`, `crates/goblin-vm/src/compiler.rs:1643`, `crates/goblin-vm/src/compiler.rs:1754`, `crates/goblin-vm/src/opcode.rs:235`, `crates/goblin-vm/src/vm.rs:413`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator <=**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21523`, `crates/goblin-vm/src/compiler.rs:1234`, `crates/goblin-vm/src/vm.rs:420`, `crates/goblin-vm/src/compiler.rs:737`, `crates/goblin-vm/src/opcode.rs:236`, `crates/goblin-vm/src/vm.rs:418`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ==**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21465`, `crates/goblin-vm/src/compiler.rs:1231`, `crates/goblin-vm/src/compiler.rs:718`, `crates/goblin-vm/src/compiler.rs:918`, `crates/goblin-vm/src/compiler.rs:940`, `crates/goblin-vm/src/compiler.rs:1307`, `crates/goblin-vm/src/compiler.rs:1359`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ===**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21456`, `crates/goblin-vm/src/compiler.rs:1231`, `crates/goblin-vm/src/compiler.rs:718`, `crates/goblin-vm/src/compiler.rs:918`, `crates/goblin-vm/src/compiler.rs:940`, `crates/goblin-vm/src/compiler.rs:1307`, `crates/goblin-vm/src/compiler.rs:1359`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator >**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21523`, `crates/goblin-vm/src/compiler.rs:1235`, `crates/goblin-vm/src/vm.rs:425`, `crates/goblin-vm/src/opcode.rs:237`, `crates/goblin-vm/src/vm.rs:423`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator >=**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21523`, `crates/goblin-vm/src/compiler.rs:1236`, `crates/goblin-vm/src/vm.rs:430`, `crates/goblin-vm/src/compiler.rs:732`, `crates/goblin-vm/src/opcode.rs:238`, `crates/goblin-vm/src/vm.rs:428`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator ??**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21658`, `crates/goblin-vm/src/compiler.rs:1211`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **operator of**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:21426`, `crates/goblin-vm/src/compiler.rs:1252`, `crates/goblin-vm/src/compiler.rs:1228`, `crates/goblin-vm/src/opcode.rs:217`, `crates/goblin-vm/src/vm.rs:305`, `crates/goblin-vm/src/vm.rs:1262`, `crates/goblin-vm/src/vm.rs:1263`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: operator/runtime.
- [ ] **optional member access**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:19062`, `crates/goblin-vm/src/compiler.rs:850`, `crates/goblin-vm/src/compiler.rs:935`, `crates/goblin-vm/src/compiler.rs:943`, `crates/goblin-vm/src/compiler.rs:1107`, `crates/goblin-vm/src/compiler.rs:1142`, `crates/goblin-vm/src/debug.rs:63`, `crates/goblin-vm/src/debug.rs:67`, `crates/goblin-vm/src/opcode.rs:108`, `crates/goblin-vm/src/opcode.rs:247`, `crates/goblin-vm/src/vm.rs:623`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: member/object runtime.
- [ ] **ord**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13776`, `crates/goblin-vm/src/compiler.rs:1983`, `crates/goblin-vm/src/value.rs:367`, `crates/goblin-vm/src/builtins.rs:330`, `crates/goblin-vm/src/compiler.rs:1984`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **overlay_strength**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6757`, `crates/goblin-vm/src/compiler.rs:2266`, `crates/goblin-vm/src/value.rs:677`, `crates/goblin-vm/src/builtins.rs:3461`, `crates/goblin-vm/src/compiler.rs:2267`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **overlays_of**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6746`, `crates/goblin-vm/src/compiler.rs:2265`, `crates/goblin-vm/src/value.rs:676`, `crates/goblin-vm/src/builtins.rs:3452`, `crates/goblin-vm/src/compiler.rs:2266`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **owned_by**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13837`, `crates/goblin-vm/src/compiler.rs:2268`, `crates/goblin-vm/src/value.rs:679`, `crates/goblin-vm/src/builtins.rs:3399`, `crates/goblin-vm/src/compiler.rs:2269`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **owns_tree**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13865`, `crates/goblin-vm/src/compiler.rs:2269`, `crates/goblin-vm/src/value.rs:680`, `crates/goblin-vm/src/builtins.rs:3414`, `crates/goblin-vm/src/compiler.rs:2270`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **pack**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12086`, `crates/goblin-vm/src/compiler.rs:2159`, `crates/goblin-vm/src/value.rs:568`, `crates/goblin-vm/src/builtins.rs:2497`, `crates/goblin-vm/src/compiler.rs:2160`, `crates/goblin-vm/src/vm.rs:1819`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **parse_bool**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15232`, `crates/goblin-vm/src/compiler.rs:2107`, `crates/goblin-vm/src/value.rs:478`, `crates/goblin-vm/src/builtins.rs:1888`, `crates/goblin-vm/src/compiler.rs:2108`, `crates/goblin-vm/src/vm.rs:1871`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **path_join**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13984`, `crates/goblin-vm/src/compiler.rs:2220`, `crates/goblin-vm/src/value.rs:638`, `crates/goblin-vm/src/builtins.rs:3043`, `crates/goblin-vm/src/compiler.rs:2221`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **path_normalize**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13986`, `crates/goblin-vm/src/compiler.rs:2222`, `crates/goblin-vm/src/value.rs:640`, `crates/goblin-vm/src/builtins.rs:3059`, `crates/goblin-vm/src/compiler.rs:2223`, `crates/goblin-vm/src/vm.rs:1919`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **path_relative_to**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13990`, `crates/goblin-vm/src/compiler.rs:2223`, `crates/goblin-vm/src/value.rs:641`, `crates/goblin-vm/src/builtins.rs:3074`, `crates/goblin-vm/src/compiler.rs:2224`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **path_split**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13989`, `crates/goblin-vm/src/compiler.rs:2221`, `crates/goblin-vm/src/value.rs:639`, `crates/goblin-vm/src/builtins.rs:3051`, `crates/goblin-vm/src/compiler.rs:2222`, `crates/goblin-vm/src/vm.rs:1918`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **pathfind**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13991`, `crates/goblin-vm/src/compiler.rs:2229`, `crates/goblin-vm/src/value.rs:647`, `crates/goblin-vm/src/builtins.rs:3152`, `crates/goblin-vm/src/compiler.rs:2230`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **pct**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12421`, `crates/goblin-vm/src/compiler.rs:2169`, `crates/goblin-vm/src/value.rs:588`, `crates/goblin-vm/src/builtins.rs:826`, `crates/goblin-vm/src/compiler.rs:2170`, `crates/goblin-vm/src/vm.rs:1833`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **percent**
  Audience: language users. Parity status: `MISSING`.
  Source: `crates/goblin-interpreter/src/lib.rs:12421`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **pick**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12671`, `crates/goblin-vm/src/compiler.rs:2173`, `crates/goblin-vm/src/value.rs:592`, `crates/goblin-vm/src/builtins.rs:929`, `crates/goblin-vm/src/compiler.rs:2174`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **pow**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12531`, `crates/goblin-vm/src/compiler.rs:1968`, `crates/goblin-vm/src/value.rs:353`, `crates/goblin-vm/src/builtins.rs:188`, `crates/goblin-vm/src/compiler.rs:975`, `crates/goblin-vm/src/compiler.rs:1247`, `crates/goblin-vm/src/compiler.rs:1969`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **provoke**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11543`, `crates/goblin-vm/src/compiler.rs:2182`, `crates/goblin-vm/src/value.rs:602`, `crates/goblin-vm/src/builtins.rs:2752`, `crates/goblin-vm/src/compiler.rs:2183`, `crates/goblin-vm/src/vm.rs:728`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14048`, `crates/goblin-vm/src/compiler.rs:2054`, `crates/goblin-vm/src/value.rs:432`, `crates/goblin-vm/src/builtins.rs:1447`, `crates/goblin-vm/src/compiler.rs:2055`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put_at**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14063`, `crates/goblin-vm/src/compiler.rs:2057`, `crates/goblin-vm/src/value.rs:435`, `crates/goblin-vm/src/builtins.rs:1469`, `crates/goblin-vm/src/compiler.rs:2058`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14074`, `crates/goblin-vm/src/compiler.rs:2086`, `crates/goblin-vm/src/value.rs:464`, `crates/goblin-vm/src/builtins.rs:1727`, `crates/goblin-vm/src/compiler.rs:2087`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put_first**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14053`, `crates/goblin-vm/src/compiler.rs:2055`, `crates/goblin-vm/src/value.rs:433`, `crates/goblin-vm/src/builtins.rs:1461`, `crates/goblin-vm/src/compiler.rs:2056`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14058`, `crates/goblin-vm/src/compiler.rs:2056`, `crates/goblin-vm/src/value.rs:434`, `crates/goblin-vm/src/builtins.rs:1465`, `crates/goblin-vm/src/compiler.rs:1367`, `crates/goblin-vm/src/compiler.rs:2057`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14068`, `crates/goblin-vm/src/compiler.rs:2085`, `crates/goblin-vm/src/value.rs:464`, `crates/goblin-vm/src/builtins.rs:1720`, `crates/goblin-vm/src/compiler.rs:2086`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **put_random**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14081`, `crates/goblin-vm/src/compiler.rs:2087`, `crates/goblin-vm/src/value.rs:464`, `crates/goblin-vm/src/builtins.rs:1735`, `crates/goblin-vm/src/compiler.rs:2088`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **rand_seed**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13208`, `crates/goblin-vm/src/compiler.rs:2044`, `crates/goblin-vm/src/value.rs:547`, `crates/goblin-vm/src/builtins.rs:2208`, `crates/goblin-vm/src/compiler.rs:2045`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **raw**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13769`, `crates/goblin-vm/src/compiler.rs:1981`, `crates/goblin-vm/src/value.rs:361`, `crates/goblin-vm/src/builtins.rs:252`, `crates/goblin-vm/src/compiler.rs:1982`, `crates/goblin-vm/src/vm.rs:1810`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **read_json**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14502`, `crates/goblin-vm/src/compiler.rs:2174`, `crates/goblin-vm/src/value.rs:593`, `crates/goblin-vm/src/builtins.rs:1019`, `crates/goblin-vm/src/compiler.rs:2175`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **read_text**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13979`, `crates/goblin-vm/src/compiler.rs:2196`, `crates/goblin-vm/src/value.rs:616`, `crates/goblin-vm/src/builtins.rs:1012`, `crates/goblin-vm/src/compiler.rs:2197`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14548`, `crates/goblin-vm/src/compiler.rs:2068`, `crates/goblin-vm/src/value.rs:597`, `crates/goblin-vm/src/builtins.rs:1632`, `crates/goblin-vm/src/compiler.rs:2069`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap_at**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14194`, `crates/goblin-vm/src/compiler.rs:2071`, `crates/goblin-vm/src/value.rs:455`, `crates/goblin-vm/src/builtins.rs:1581`, `crates/goblin-vm/src/compiler.rs:2072`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14211`, `crates/goblin-vm/src/compiler.rs:2098`, `crates/goblin-vm/src/value.rs:470`, `crates/goblin-vm/src/builtins.rs:1833`, `crates/goblin-vm/src/compiler.rs:2099`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap_first**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14184`, `crates/goblin-vm/src/compiler.rs:2069`, `crates/goblin-vm/src/value.rs:453`, `crates/goblin-vm/src/builtins.rs:1571`, `crates/goblin-vm/src/compiler.rs:2070`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14189`, `crates/goblin-vm/src/compiler.rs:2070`, `crates/goblin-vm/src/value.rs:454`, `crates/goblin-vm/src/builtins.rs:1576`, `crates/goblin-vm/src/compiler.rs:2071`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14205`, `crates/goblin-vm/src/compiler.rs:2097`, `crates/goblin-vm/src/value.rs:470`, `crates/goblin-vm/src/builtins.rs:1827`, `crates/goblin-vm/src/compiler.rs:2098`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reap_where**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14199`, `crates/goblin-vm/src/compiler.rs:2073`, `crates/goblin-vm/src/value.rs:457`, `crates/goblin-vm/src/builtins.rs:1601`, `crates/goblin-vm/src/compiler.rs:2074`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **register_token**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:5982`, `crates/goblin-vm/src/compiler.rs:2247`, `crates/goblin-vm/src/value.rs:667`, `crates/goblin-vm/src/builtins.rs:3343`, `crates/goblin-vm/src/compiler.rs:2248`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **req_body**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13963`, `crates/goblin-vm/src/compiler.rs:2033`, `crates/goblin-vm/src/value.rs:553`, `crates/goblin-vm/src/builtins.rs:2374`, `crates/goblin-vm/src/compiler.rs:2034`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **req_header**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13964`, `crates/goblin-vm/src/compiler.rs:2034`, `crates/goblin-vm/src/value.rs:554`, `crates/goblin-vm/src/builtins.rs:2377`, `crates/goblin-vm/src/compiler.rs:2035`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **req_method**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13960`, `crates/goblin-vm/src/compiler.rs:2030`, `crates/goblin-vm/src/value.rs:550`, `crates/goblin-vm/src/builtins.rs:2365`, `crates/goblin-vm/src/compiler.rs:2031`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **req_path**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13961`, `crates/goblin-vm/src/compiler.rs:2031`, `crates/goblin-vm/src/value.rs:551`, `crates/goblin-vm/src/builtins.rs:2368`, `crates/goblin-vm/src/compiler.rs:2032`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **req_query**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13962`, `crates/goblin-vm/src/compiler.rs:2032`, `crates/goblin-vm/src/value.rs:552`, `crates/goblin-vm/src/builtins.rs:2371`, `crates/goblin-vm/src/compiler.rs:2033`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **resolve_token**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:5991`, `crates/goblin-vm/src/compiler.rs:2248`, `crates/goblin-vm/src/value.rs:668`, `crates/goblin-vm/src/builtins.rs:3353`, `crates/goblin-vm/src/compiler.rs:2249`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reverse**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15190`, `crates/goblin-vm/src/compiler.rs:2104`, `crates/goblin-vm/src/value.rs:475`, `crates/goblin-vm/src/builtins.rs:1864`, `crates/goblin-vm/src/compiler.rs:2105`, `crates/goblin-vm/src/vm.rs:1814`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **reverse_chars**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15211`, `crates/goblin-vm/src/compiler.rs:2105`, `crates/goblin-vm/src/value.rs:476`, `crates/goblin-vm/src/builtins.rs:1871`, `crates/goblin-vm/src/compiler.rs:2106`, `crates/goblin-vm/src/vm.rs:1856`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **roll**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13218`, `crates/goblin-vm/src/compiler.rs:2041`, `crates/goblin-vm/src/value.rs:545`, `crates/goblin-vm/src/builtins.rs:2213`, `crates/goblin-vm/src/builtins.rs:3218`, `crates/goblin-vm/src/compiler.rs:2043`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **roll_detail**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13423`, `crates/goblin-vm/src/compiler.rs:2043`, `crates/goblin-vm/src/value.rs:546`, `crates/goblin-vm/src/builtins.rs:2265`, `crates/goblin-vm/src/builtins.rs:3225`, `crates/goblin-vm/src/compiler.rs:2044`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **roll_detail_str**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13672`, `crates/goblin-vm/src/compiler.rs:2234`, `crates/goblin-vm/src/value.rs:654`, `crates/goblin-vm/src/builtins.rs:3220`, `crates/goblin-vm/src/compiler.rs:2235`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **roll_str**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13672`, `crates/goblin-vm/src/compiler.rs:2233`, `crates/goblin-vm/src/value.rs:653`, `crates/goblin-vm/src/builtins.rs:3213`, `crates/goblin-vm/src/compiler.rs:2234`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **round**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12443`, `crates/goblin-vm/src/compiler.rs:1965`, `crates/goblin-vm/src/value.rs:350`, `crates/goblin-vm/src/builtins.rs:161`, `crates/goblin-vm/src/compiler.rs:1966`, `crates/goblin-vm/src/vm.rs:1831`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **run_cmd**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13796`, `crates/goblin-vm/src/compiler.rs:2027`, `crates/goblin-vm/src/value.rs:544`, `crates/goblin-vm/src/builtins.rs:2155`, `crates/goblin-vm/src/compiler.rs:2028`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **sample_weighted**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13790`, `crates/goblin-vm/src/compiler.rs:2016`, `crates/goblin-vm/src/value.rs:415`, `crates/goblin-vm/src/builtins.rs:1276`, `crates/goblin-vm/src/compiler.rs:2017`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **sanitize_bom**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:15379`, `crates/goblin-vm/src/compiler.rs:1997`, `crates/goblin-vm/src/value.rs:387`, `crates/goblin-vm/src/builtins.rs:570`, `crates/goblin-vm/src/compiler.rs:1998`, `crates/goblin-vm/src/vm.rs:1858`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **secure_pick**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13806`, `crates/goblin-vm/src/compiler.rs:2156`, `crates/goblin-vm/src/value.rs:563`, `crates/goblin-vm/src/builtins.rs:2521`, `crates/goblin-vm/src/compiler.rs:2157`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **secure_random**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13807`, `crates/goblin-vm/src/compiler.rs:2157`, `crates/goblin-vm/src/value.rs:564`, `crates/goblin-vm/src/builtins.rs:2605`, `crates/goblin-vm/src/compiler.rs:2158`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **secure_shuffle**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13808`, `crates/goblin-vm/src/compiler.rs:2158`, `crates/goblin-vm/src/value.rs:565`, `crates/goblin-vm/src/builtins.rs:2616`, `crates/goblin-vm/src/compiler.rs:2159`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **set_cookie**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13970`, `crates/goblin-vm/src/compiler.rs:2040`, `crates/goblin-vm/src/value.rs:560`, `crates/goblin-vm/src/builtins.rs:2450`, `crates/goblin-vm/src/compiler.rs:2041`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **set_header**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13969`, `crates/goblin-vm/src/compiler.rs:2039`, `crates/goblin-vm/src/value.rs:559`, `crates/goblin-vm/src/builtins.rs:2435`, `crates/goblin-vm/src/compiler.rs:2040`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **set_status**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13968`, `crates/goblin-vm/src/compiler.rs:2038`, `crates/goblin-vm/src/value.rs:558`, `crates/goblin-vm/src/builtins.rs:2426`, `crates/goblin-vm/src/compiler.rs:2039`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **shuffle**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13786`, `crates/goblin-vm/src/compiler.rs:2013`, `crates/goblin-vm/src/value.rs:411`, `crates/goblin-vm/src/builtins.rs:1153`, `crates/goblin-vm/src/compiler.rs:2014`, `crates/goblin-vm/src/vm.rs:1815`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **slug**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13768`, `crates/goblin-vm/src/compiler.rs:1979`, `crates/goblin-vm/src/value.rs:359`, `crates/goblin-vm/src/builtins.rs:229`, `crates/goblin-vm/src/compiler.rs:1980`, `crates/goblin-vm/src/vm.rs:1808`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **sort**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13787`, `crates/goblin-vm/src/compiler.rs:2108`, `crates/goblin-vm/src/value.rs:412`, `crates/goblin-vm/src/builtins.rs:1200`, `crates/goblin-vm/src/compiler.rs:2109`, `crates/goblin-vm/src/vm.rs:1816`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **split**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14945`, `crates/goblin-vm/src/compiler.rs:1984`, `crates/goblin-vm/src/value.rs:374`, `crates/goblin-vm/src/builtins.rs:376`, `crates/goblin-vm/src/compiler.rs:1985`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **sqrt**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12618`, `crates/goblin-vm/src/compiler.rs:1966`, `crates/goblin-vm/src/value.rs:351`, `crates/goblin-vm/src/builtins.rs:169`, `crates/goblin-vm/src/compiler.rs:977`, `crates/goblin-vm/src/compiler.rs:1967`, `crates/goblin-vm/src/vm.rs:1828`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **starts_with**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:16240`, `crates/goblin-vm/src/compiler.rs:1987`, `crates/goblin-vm/src/value.rs:377`, `crates/goblin-vm/src/builtins.rs:445`, `crates/goblin-vm/src/compiler.rs:1988`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **stem**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13981`, `crates/goblin-vm/src/compiler.rs:2218`, `crates/goblin-vm/src/value.rs:636`, `crates/goblin-vm/src/builtins.rs:3030`, `crates/goblin-vm/src/compiler.rs:2219`, `crates/goblin-vm/src/vm.rs:1916`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **Stmt::Action**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3435`, `crates/goblin-vm/src/compiler.rs:178`, `crates/goblin-vm/src/compiler.rs:505`, `crates/goblin-vm/src/vm.rs:1479`, `crates/goblin-vm/src/compiler.rs:511`, `crates/goblin-vm/src/opcode.rs:272`, `crates/goblin-vm/src/vm.rs:1081`, `crates/goblin-vm/src/compiler.rs:255`, `crates/goblin-vm/src/compiler.rs:288`, `crates/goblin-vm/src/compiler.rs:428`, `crates/goblin-vm/src/compiler.rs:466`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Bind**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:5656`, `crates/goblin-vm/src/compiler.rs:169`, `crates/goblin-vm/src/compiler.rs:299`, `crates/goblin-vm/src/compiler.rs:450`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:263`, `crates/goblin-vm/src/compiler.rs:317`, `crates/goblin-vm/src/compiler.rs:322`, `crates/goblin-vm/src/compiler.rs:502`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Block**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4214`, `crates/goblin-vm/src/compiler.rs:181`, `crates/goblin-vm/src/compiler.rs:536`, `crates/goblin-vm/src/compiler.rs:994`, `crates/goblin-vm/src/compiler.rs:549`, `crates/goblin-vm/src/compiler.rs:915`, `crates/goblin-vm/src/compiler.rs:937`, `crates/goblin-vm/src/compiler.rs:1172`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::BoxBind**
  Audience: language users. Parity status: `MISSING`.
  Source: `crates/goblin-interpreter/src/lib.rs:5510`, `crates/goblin-vm/src/compiler.rs:652`, `crates/goblin-vm/src/compiler.rs:447`, `crates/goblin-vm/src/compiler.rs:665`, `crates/goblin-vm/src/compiler.rs:921`, `crates/goblin-vm/src/compiler.rs:931`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Class**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3866`, `crates/goblin-vm/src/compiler.rs:562`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::ClearLink**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4194`, `crates/goblin-vm/src/compiler.rs:638`, `crates/goblin-vm/src/compiler.rs:639`, `crates/goblin-vm/src/opcode.rs:269`, `crates/goblin-vm/src/vm.rs:1067`, `crates/goblin-vm/src/compiler.rs:642`, `crates/goblin-vm/src/opcode.rs:270`, `crates/goblin-vm/src/vm.rs:1073`, `crates/goblin-vm/src/compiler.rs:648`, `crates/goblin-vm/src/opcode.rs:271`, `crates/goblin-vm/src/vm.rs:1077`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Enum**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3928`, `crates/goblin-vm/src/compiler.rs:565`, `crates/goblin-vm/src/compiler.rs:581`, `crates/goblin-vm/src/compiler.rs:587`, `crates/goblin-vm/src/compiler.rs:601`, `crates/goblin-vm/src/opcode.rs:262`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Expr**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3433`, `crates/goblin-vm/src/compiler.rs:295`, `crates/goblin-vm/src/compiler.rs:445`, `crates/goblin-vm/src/compiler.rs:1003`, `crates/goblin-vm/src/tick.rs:1061`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:447`, `crates/goblin-vm/src/compiler.rs:665`, `crates/goblin-vm/src/compiler.rs:921`, `crates/goblin-vm/src/compiler.rs:931`, `crates/goblin-vm/src/compiler.rs:456`, `crates/goblin-vm/src/opcode.rs:249`, `crates/goblin-vm/src/vm.rs:535`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Import**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4222`, `crates/goblin-vm/src/compiler.rs:570`, `crates/goblin-vm/src/compiler.rs:581`, `crates/goblin-vm/src/compiler.rs:587`, `crates/goblin-vm/src/compiler.rs:601`, `crates/goblin-vm/src/opcode.rs:262`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Judge**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:5396`, `crates/goblin-vm/src/compiler.rs:182`, `crates/goblin-vm/src/compiler.rs:518`, `crates/goblin-vm/src/compiler.rs:989`, `crates/goblin-vm/src/compiler.rs:126`, `crates/goblin-vm/src/compiler.rs:135`, `crates/goblin-vm/src/compiler.rs:527`, `crates/goblin-vm/src/compiler.rs:681`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::JudgeAll**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:5451`, `crates/goblin-vm/src/compiler.rs:189`, `crates/goblin-vm/src/compiler.rs:522`, `crates/goblin-vm/src/compiler.rs:126`, `crates/goblin-vm/src/compiler.rs:135`, `crates/goblin-vm/src/compiler.rs:527`, `crates/goblin-vm/src/compiler.rs:681`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::LinkDef**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4125`, `crates/goblin-vm/src/compiler.rs:629`, `crates/goblin-vm/src/compiler.rs:630`, `crates/goblin-vm/src/opcode.rs:266`, `crates/goblin-vm/src/vm.rs:1024`, `crates/goblin-vm/src/compiler.rs:633`, `crates/goblin-vm/src/opcode.rs:267`, `crates/goblin-vm/src/vm.rs:1037`, `crates/goblin-vm/src/compiler.rs:636`, `crates/goblin-vm/src/opcode.rs:268`, `crates/goblin-vm/src/vm.rs:1050`, `crates/goblin-vm/src/compiler.rs:639`, `crates/goblin-vm/src/opcode.rs:269`, `crates/goblin-vm/src/vm.rs:1067`, `crates/goblin-vm/src/compiler.rs:642`, `crates/goblin-vm/src/opcode.rs:270`, `crates/goblin-vm/src/vm.rs:1073`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::LinkOffset**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4163`, `crates/goblin-vm/src/compiler.rs:635`, `crates/goblin-vm/src/compiler.rs:636`, `crates/goblin-vm/src/opcode.rs:268`, `crates/goblin-vm/src/vm.rs:1050`, `crates/goblin-vm/src/compiler.rs:639`, `crates/goblin-vm/src/opcode.rs:269`, `crates/goblin-vm/src/vm.rs:1067`, `crates/goblin-vm/src/compiler.rs:642`, `crates/goblin-vm/src/opcode.rs:270`, `crates/goblin-vm/src/vm.rs:1073`, `crates/goblin-vm/src/compiler.rs:648`, `crates/goblin-vm/src/opcode.rs:271`, `crates/goblin-vm/src/vm.rs:1077`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::ObjectDecision**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4154`, `crates/goblin-vm/src/compiler.rs:641`, `crates/goblin-vm/src/compiler.rs:642`, `crates/goblin-vm/src/opcode.rs:270`, `crates/goblin-vm/src/vm.rs:1073`, `crates/goblin-vm/src/compiler.rs:648`, `crates/goblin-vm/src/opcode.rs:271`, `crates/goblin-vm/src/vm.rs:1077`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::ObjectLinkDef**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4140`, `crates/goblin-vm/src/compiler.rs:632`, `crates/goblin-vm/src/compiler.rs:633`, `crates/goblin-vm/src/opcode.rs:267`, `crates/goblin-vm/src/vm.rs:1037`, `crates/goblin-vm/src/compiler.rs:636`, `crates/goblin-vm/src/opcode.rs:268`, `crates/goblin-vm/src/vm.rs:1050`, `crates/goblin-vm/src/compiler.rs:639`, `crates/goblin-vm/src/opcode.rs:269`, `crates/goblin-vm/src/vm.rs:1067`, `crates/goblin-vm/src/compiler.rs:642`, `crates/goblin-vm/src/opcode.rs:270`, `crates/goblin-vm/src/vm.rs:1073`, `crates/goblin-vm/src/compiler.rs:648`, `crates/goblin-vm/src/opcode.rs:271`, `crates/goblin-vm/src/vm.rs:1077`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::OverlayApply**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3981`, `crates/goblin-vm/src/compiler.rs:608`, `crates/goblin-vm/src/compiler.rs:614`, `crates/goblin-vm/src/opcode.rs:264`, `crates/goblin-vm/src/vm.rs:980`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::OverlayDef**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3935`, `crates/goblin-vm/src/compiler.rs:605`, `crates/goblin-vm/src/compiler.rs:606`, `crates/goblin-vm/src/opcode.rs:263`, `crates/goblin-vm/src/vm.rs:958`, `crates/goblin-vm/src/compiler.rs:614`, `crates/goblin-vm/src/opcode.rs:264`, `crates/goblin-vm/src/vm.rs:980`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::OverlayDetach**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4087`, `crates/goblin-vm/src/compiler.rs:621`, `crates/goblin-vm/src/compiler.rs:627`, `crates/goblin-vm/src/opcode.rs:265`, `crates/goblin-vm/src/vm.rs:1017`, `crates/goblin-vm/src/compiler.rs:630`, `crates/goblin-vm/src/opcode.rs:266`, `crates/goblin-vm/src/vm.rs:1024`, `crates/goblin-vm/src/compiler.rs:633`, `crates/goblin-vm/src/opcode.rs:267`, `crates/goblin-vm/src/vm.rs:1037`, `crates/goblin-vm/src/compiler.rs:636`, `crates/goblin-vm/src/opcode.rs:268`, `crates/goblin-vm/src/vm.rs:1050`, `crates/goblin-vm/src/compiler.rs:639`, `crates/goblin-vm/src/opcode.rs:269`, `crates/goblin-vm/src/vm.rs:1067`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Return**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4825`, `crates/goblin-vm/src/compiler.rs:482`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:500`, `crates/goblin-vm/src/compiler.rs:831`, `crates/goblin-vm/src/compiler.rs:1053`, `crates/goblin-vm/src/debug.rs:62`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Sweep**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4876`, `crates/goblin-vm/src/compiler.rs:196`, `crates/goblin-vm/src/compiler.rs:540`, `crates/goblin-vm/src/compiler.rs:549`, `crates/goblin-vm/src/compiler.rs:915`, `crates/goblin-vm/src/compiler.rs:937`, `crates/goblin-vm/src/compiler.rs:1172`, `crates/goblin-vm/src/compiler.rs:397`, `crates/goblin-vm/src/compiler.rs:497`, `crates/goblin-vm/src/compiler.rs:553`, `crates/goblin-vm/src/compiler.rs:717`, `crates/goblin-vm/src/compiler.rs:554`, `crates/goblin-vm/src/compiler.rs:838`, `crates/goblin-vm/src/compiler.rs:844`, `crates/goblin-vm/src/compiler.rs:1573`, `crates/goblin-vm/src/compiler.rs:255`, `crates/goblin-vm/src/compiler.rs:288`, `crates/goblin-vm/src/compiler.rs:428`, `crates/goblin-vm/src/compiler.rs:466`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::TupleBind**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:3444`, `crates/goblin-vm/src/compiler.rs:173`, `crates/goblin-vm/src/compiler.rs:304`, `crates/goblin-vm/src/compiler.rs:544`, `crates/goblin-vm/src/compiler.rs:254`, `crates/goblin-vm/src/compiler.rs:262`, `crates/goblin-vm/src/compiler.rs:287`, `crates/goblin-vm/src/compiler.rs:310`, `crates/goblin-vm/src/compiler.rs:263`, `crates/goblin-vm/src/compiler.rs:317`, `crates/goblin-vm/src/compiler.rs:322`, `crates/goblin-vm/src/compiler.rs:502`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::UnitDecl**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4209`, `crates/goblin-vm/src/compiler.rs:647`, `crates/goblin-vm/src/compiler.rs:648`, `crates/goblin-vm/src/opcode.rs:271`, `crates/goblin-vm/src/vm.rs:1077`, `crates/goblin-vm/src/compiler.rs:447`, `crates/goblin-vm/src/compiler.rs:665`, `crates/goblin-vm/src/compiler.rs:921`, `crates/goblin-vm/src/compiler.rs:931`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **Stmt::Use**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:4340`, `crates/goblin-vm/src/compiler.rs:595`, `crates/goblin-vm/src/compiler.rs:581`, `crates/goblin-vm/src/compiler.rs:587`, `crates/goblin-vm/src/compiler.rs:601`, `crates/goblin-vm/src/opcode.rs:262`, `crates/goblin-vm/src/compiler.rs:606`, `crates/goblin-vm/src/opcode.rs:263`, `crates/goblin-vm/src/vm.rs:958`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: statement.
- [ ] **str**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12417`, `crates/goblin-vm/src/compiler.rs:1970`, `crates/goblin-vm/src/value.rs:526`, `crates/goblin-vm/src/builtins.rs:358`, `crates/goblin-vm/src/compiler.rs:1971`, `crates/goblin-vm/src/compiler.rs:2152`, `crates/goblin-vm/src/vm.rs:1800`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **string**
  Audience: language users. Parity status: `MISSING`.
  Source: `crates/goblin-interpreter/src/lib.rs:12417`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **sum**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6370`, `crates/goblin-vm/src/compiler.rs:1962`, `crates/goblin-vm/src/value.rs:347`, `crates/goblin-vm/src/builtins.rs:138`, `crates/goblin-vm/src/compiler.rs:1963`, `crates/goblin-vm/src/vm.rs:1883`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **summon**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11477`, `crates/goblin-vm/src/compiler.rs:2181`, `crates/goblin-vm/src/value.rs:601`, `crates/goblin-vm/src/builtins.rs:2752`, `crates/goblin-vm/src/compiler.rs:2182`, `crates/goblin-vm/src/vm.rs:723`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **tick**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6715`, `crates/goblin-vm/src/compiler.rs:2261`, `crates/goblin-vm/src/value.rs:698`, `crates/goblin-vm/src/builtins.rs:3332`, `crates/goblin-vm/src/compiler.rs:2262`, `crates/goblin-vm/src/vm.rs:733`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **tick_db**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:6715`, `crates/goblin-vm/src/compiler.rs:2261`, `crates/goblin-vm/src/value.rs:698`, `crates/goblin-vm/src/builtins.rs:3332`, `crates/goblin-vm/src/compiler.rs:2262`, `crates/goblin-vm/src/vm.rs:733`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **title**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13767`, `crates/goblin-vm/src/compiler.rs:1978`, `crates/goblin-vm/src/value.rs:358`, `crates/goblin-vm/src/builtins.rs:210`, `crates/goblin-vm/src/compiler.rs:1979`, `crates/goblin-vm/src/vm.rs:1807`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **to_map**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12425`, `crates/goblin-vm/src/compiler.rs:2195`, `crates/goblin-vm/src/value.rs:615`, `crates/goblin-vm/src/builtins.rs:2870`, `crates/goblin-vm/src/compiler.rs:2196`, `crates/goblin-vm/src/vm.rs:1891`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **tokenize**
  Audience: language users. Parity status: `MISSING`.
  Source: `crates/goblin-interpreter/src/lib.rs:15138`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **trim**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13771`, `crates/goblin-vm/src/compiler.rs:1973`, `crates/goblin-vm/src/value.rs:362`, `crates/goblin-vm/src/builtins.rs:262`, `crates/goblin-vm/src/compiler.rs:1974`, `crates/goblin-vm/src/vm.rs:1811`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **trim_lead**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13772`, `crates/goblin-vm/src/compiler.rs:1974`, `crates/goblin-vm/src/value.rs:363`, `crates/goblin-vm/src/builtins.rs:271`, `crates/goblin-vm/src/compiler.rs:1975`, `crates/goblin-vm/src/vm.rs:1812`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **trim_trail**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13773`, `crates/goblin-vm/src/compiler.rs:1975`, `crates/goblin-vm/src/value.rs:364`, `crates/goblin-vm/src/builtins.rs:280`, `crates/goblin-vm/src/compiler.rs:1976`, `crates/goblin-vm/src/vm.rs:1813`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **u16**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12438`, `crates/goblin-vm/src/compiler.rs:2203`, `crates/goblin-vm/src/value.rs:624`, `crates/goblin-vm/src/builtins.rs:2937`, `crates/goblin-vm/src/compiler.rs:2204`, `crates/goblin-vm/src/vm.rs:1908`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **u32**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12439`, `crates/goblin-vm/src/compiler.rs:2204`, `crates/goblin-vm/src/value.rs:625`, `crates/goblin-vm/src/builtins.rs:2945`, `crates/goblin-vm/src/compiler.rs:2205`, `crates/goblin-vm/src/vm.rs:1909`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **u64**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12440`, `crates/goblin-vm/src/compiler.rs:2205`, `crates/goblin-vm/src/value.rs:626`, `crates/goblin-vm/src/builtins.rs:2953`, `crates/goblin-vm/src/compiler.rs:2206`, `crates/goblin-vm/src/vm.rs:1910`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **u8**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12437`, `crates/goblin-vm/src/compiler.rs:2202`, `crates/goblin-vm/src/value.rs:623`, `crates/goblin-vm/src/builtins.rs:2929`, `crates/goblin-vm/src/compiler.rs:2203`, `crates/goblin-vm/src/vm.rs:1907`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **unique**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13792`, `crates/goblin-vm/src/compiler.rs:2119`, `crates/goblin-vm/src/value.rs:417`, `crates/goblin-vm/src/builtins.rs:1362`, `crates/goblin-vm/src/compiler.rs:2120`, `crates/goblin-vm/src/vm.rs:1817`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **unpack**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:12048`, `crates/goblin-vm/src/compiler.rs:2160`, `crates/goblin-vm/src/value.rs:569`, `crates/goblin-vm/src/builtins.rs:2502`, `crates/goblin-vm/src/compiler.rs:2161`, `crates/goblin-vm/src/vm.rs:1820`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14086`, `crates/goblin-vm/src/compiler.rs:2058`, `crates/goblin-vm/src/value.rs:438`, `crates/goblin-vm/src/builtins.rs:1477`, `crates/goblin-vm/src/compiler.rs:2059`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_all**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14112`, `crates/goblin-vm/src/compiler.rs:2089`, `crates/goblin-vm/src/value.rs:466`, `crates/goblin-vm/src/builtins.rs:1749`, `crates/goblin-vm/src/compiler.rs:2090`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_at**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14101`, `crates/goblin-vm/src/compiler.rs:2061`, `crates/goblin-vm/src/value.rs:441`, `crates/goblin-vm/src/builtins.rs:1491`, `crates/goblin-vm/src/compiler.rs:2062`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_between**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14123`, `crates/goblin-vm/src/compiler.rs:2092`, `crates/goblin-vm/src/value.rs:466`, `crates/goblin-vm/src/builtins.rs:1769`, `crates/goblin-vm/src/compiler.rs:2093`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_first**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14091`, `crates/goblin-vm/src/compiler.rs:2059`, `crates/goblin-vm/src/value.rs:439`, `crates/goblin-vm/src/builtins.rs:1483`, `crates/goblin-vm/src/compiler.rs:2060`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_last**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14096`, `crates/goblin-vm/src/compiler.rs:2060`, `crates/goblin-vm/src/value.rs:440`, `crates/goblin-vm/src/builtins.rs:1487`, `crates/goblin-vm/src/compiler.rs:2061`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_matching**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14117`, `crates/goblin-vm/src/compiler.rs:2091`, `crates/goblin-vm/src/value.rs:466`, `crates/goblin-vm/src/builtins.rs:1762`, `crates/goblin-vm/src/compiler.rs:2092`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_random**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14130`, `crates/goblin-vm/src/compiler.rs:2093`, `crates/goblin-vm/src/value.rs:466`, `crates/goblin-vm/src/builtins.rs:1777`, `crates/goblin-vm/src/compiler.rs:2094`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **update_where**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14106`, `crates/goblin-vm/src/compiler.rs:2090`, `crates/goblin-vm/src/value.rs:466`, `crates/goblin-vm/src/builtins.rs:1755`, `crates/goblin-vm/src/compiler.rs:2091`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **upper**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13765`, `crates/goblin-vm/src/compiler.rs:1977`, `crates/goblin-vm/src/value.rs:357`, `crates/goblin-vm/src/builtins.rs:206`, `crates/goblin-vm/src/compiler.rs:1978`, `crates/goblin-vm/src/vm.rs:1806`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **uuid_v4**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13995`, `crates/goblin-vm/src/compiler.rs:2227`, `crates/goblin-vm/src/value.rs:645`, `crates/goblin-vm/src/builtins.rs:3146`, `crates/goblin-vm/src/compiler.rs:2228`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **uuid_v7**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13996`, `crates/goblin-vm/src/compiler.rs:2228`, `crates/goblin-vm/src/value.rs:646`, `crates/goblin-vm/src/builtins.rs:3149`, `crates/goblin-vm/src/compiler.rs:2229`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **valtype**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11594`, `crates/goblin-vm/src/compiler.rs:2237`, `crates/goblin-vm/src/value.rs:657`, `crates/goblin-vm/src/builtins.rs:3229`, `crates/goblin-vm/src/compiler.rs:2238`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **values**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13780`, `crates/goblin-vm/src/compiler.rs:2101`, `crates/goblin-vm/src/value.rs:405`, `crates/goblin-vm/src/builtins.rs:1067`, `crates/goblin-vm/src/compiler.rs:2102`, `crates/goblin-vm/src/vm.rs:1822`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **vt**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:11594`, `crates/goblin-vm/src/compiler.rs:2237`, `crates/goblin-vm/src/value.rs:657`, `crates/goblin-vm/src/builtins.rs:3229`, `crates/goblin-vm/src/compiler.rs:2238`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **walk**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13992`, `crates/goblin-vm/src/compiler.rs:2224`, `crates/goblin-vm/src/value.rs:642`, `crates/goblin-vm/src/builtins.rs:3081`, `crates/goblin-vm/src/compiler.rs:2225`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **words**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14908`, `crates/goblin-vm/src/compiler.rs:2162`, `crates/goblin-vm/src/value.rs:579`, `crates/goblin-vm/src/builtins.rs:2646`, `crates/goblin-vm/src/compiler.rs:2163`, `crates/goblin-vm/src/vm.rs:1825`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **write_json**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14488`, `crates/goblin-vm/src/compiler.rs:2177`, `crates/goblin-vm/src/value.rs:596`, `crates/goblin-vm/src/builtins.rs:1045`, `crates/goblin-vm/src/compiler.rs:2178`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **write_text**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13978`, `crates/goblin-vm/src/compiler.rs:2175`, `crates/goblin-vm/src/value.rs:594`, `crates/goblin-vm/src/builtins.rs:1027`, `crates/goblin-vm/src/compiler.rs:2176`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **yall_minify**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14372`, `crates/goblin-vm/src/compiler.rs:2188`, `crates/goblin-vm/src/value.rs:608`, `crates/goblin-vm/src/builtins.rs:2792`, `crates/goblin-vm/src/compiler.rs:2189`, `crates/goblin-vm/src/vm.rs:1897`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **yall_parse**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14219`, `crates/goblin-vm/src/compiler.rs:2183`, `crates/goblin-vm/src/value.rs:603`, `crates/goblin-vm/src/builtins.rs:2756`, `crates/goblin-vm/src/compiler.rs:2184`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **yall_parse_file**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14258`, `crates/goblin-vm/src/compiler.rs:2184`, `crates/goblin-vm/src/value.rs:604`, `crates/goblin-vm/src/builtins.rs:2764`, `crates/goblin-vm/src/compiler.rs:2185`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **yall_pretty**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14353`, `crates/goblin-vm/src/compiler.rs:2187`, `crates/goblin-vm/src/value.rs:607`, `crates/goblin-vm/src/builtins.rs:2786`, `crates/goblin-vm/src/compiler.rs:2188`, `crates/goblin-vm/src/vm.rs:1896`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **yall_write**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14296`, `crates/goblin-vm/src/compiler.rs:2185`, `crates/goblin-vm/src/value.rs:605`, `crates/goblin-vm/src/builtins.rs:2771`, `crates/goblin-vm/src/compiler.rs:2186`, `crates/goblin-vm/src/vm.rs:1895`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **yall_write_file**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:14316`, `crates/goblin-vm/src/compiler.rs:2186`, `crates/goblin-vm/src/value.rs:606`, `crates/goblin-vm/src/builtins.rs:2777`, `crates/goblin-vm/src/compiler.rs:2187`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.
- [ ] **zip_dir**
  Audience: language users. Parity status: `PARTIAL`.
  Source: `crates/goblin-interpreter/src/lib.rs:13975`, `crates/goblin-vm/src/compiler.rs:2244`, `crates/goblin-vm/src/value.rs:664`, `crates/goblin-vm/src/builtins.rs:3300`, `crates/goblin-vm/src/compiler.rs:2245`
  Coverage: Document syntax/call forms, inputs, result, errors, mutation/side effects, examples, aliases, and interactions. Runtime kind: builtin/free call.

## AST and Language Data Model

- [ ] **ActionBody::Block**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:399`, `crates/goblin-ast/src/lib.rs:398`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **ActionBody::Expr**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:400`, `crates/goblin-ast/src/lib.rs:398`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BindMode::Retether**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:15`, `crates/goblin-ast/src/lib.rs:13`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BindMode::Shadow**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:16`, `crates/goblin-ast/src/lib.rs:13`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BindMode::Tether**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:14`, `crates/goblin-ast/src/lib.rs:13`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CapacityDecl::Count**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:85`, `crates/goblin-ast/src/lib.rs:83`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CapacityDecl::Field**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:88`, `crates/goblin-ast/src/lib.rs:83`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **enum: ActionBody**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:398`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: BindMode**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:13`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: CapacityDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:83`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: Expr**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: ImportItems**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:371`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: JudgeArmBody**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:419`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: LinkContinuity**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:283`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: OverlayApplyBehavior**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:166`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: OverlayContinuity**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:276`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: RelationDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:350`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: SpreadRule**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:151`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: Stmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: SweepArmKind**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:450`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: SweepArmRepeat**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:457`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: SweepMode**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:447`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: TransitionKind**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **Expr::Array**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:513`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Binary**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:532`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Block**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:548`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Bool**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:504`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::BoxVar**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:553`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Call**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:524`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Char**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:507`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::EnumVariant**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:535`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::FreeCall**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:526`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Ident**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:508`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Index**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:519`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Index2**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:521`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::IndexMap**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:520`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Judge**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:541`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::LiteralToken**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:552`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Member**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:517`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Nil**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:503`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::NsCall**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:527`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Number**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:505`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Object**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:514`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::OptCall**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:525`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::OptMember**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:518`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Postfix**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:531`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Prefix**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:530`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Slice**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:509`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Slice3**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:510`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Expr::Str**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:506`, `crates/goblin-ast/src/lib.rs:501`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **ImportItems::Expr**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:375`, `crates/goblin-ast/src/lib.rs:371`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **ImportItems::Named**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:377`, `crates/goblin-ast/src/lib.rs:371`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **ImportItems::Path**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:373`, `crates/goblin-ast/src/lib.rs:371`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **JudgeArmBody::Expr**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:420`, `crates/goblin-ast/src/lib.rs:419`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **JudgeArmBody::Stmts**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:421`, `crates/goblin-ast/src/lib.rs:419`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **LinkContinuity::Inherit**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:284`, `crates/goblin-ast/src/lib.rs:283`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **LinkContinuity::Reset**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:285`, `crates/goblin-ast/src/lib.rs:283`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **OverlayApplyBehavior::Caps**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:168`, `crates/goblin-ast/src/lib.rs:166`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **OverlayApplyBehavior::Replaces**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:170`, `crates/goblin-ast/src/lib.rs:166`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **OverlayApplyBehavior::Stacks**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:173`, `crates/goblin-ast/src/lib.rs:166`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **OverlayContinuity::Drop**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:279`, `crates/goblin-ast/src/lib.rs:276`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **OverlayContinuity::Split**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:277`, `crates/goblin-ast/src/lib.rs:276`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **OverlayContinuity::Transfer**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:278`, `crates/goblin-ast/src/lib.rs:276`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **RelationDef::Of**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:351`, `crates/goblin-ast/src/lib.rs:350`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **RelationDef::Re**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:353`, `crates/goblin-ast/src/lib.rs:350`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **RelationDef::With**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:352`, `crates/goblin-ast/src/lib.rs:350`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SpreadRule::All**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:155`, `crates/goblin-ast/src/lib.rs:151`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SpreadRule::Channel**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:153`, `crates/goblin-ast/src/lib.rs:151`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SpreadRule::Nearby**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:157`, `crates/goblin-ast/src/lib.rs:151`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SpreadRule::Ownership**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:159`, `crates/goblin-ast/src/lib.rs:151`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SpreadRule::Predicate**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:161`, `crates/goblin-ast/src/lib.rs:151`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Action**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:45`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Bind**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:46`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Block**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:66`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::BoxBind**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:70`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Class**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:44`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::ClearLink**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:61`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Enum**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:48`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Expr**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:43`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Import**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:49`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Judge**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:51`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::JudgeAll**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:52`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::LinkDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:58`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::LinkOffset**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:60`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::ObjectDecision**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:63`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::ObjectLinkDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:59`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::OverlayApply**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:56`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::OverlayDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:55`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::OverlayDetach**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:57`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Return**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:54`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Sweep**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:53`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::TupleBind**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:47`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::UnitDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:64`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Stmt::Use**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:50`, `crates/goblin-ast/src/lib.rs:42`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **struct: ActionDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:404`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: BindStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:20`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ClassDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:245`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ClearLinkStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:140`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: DecisionDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:325`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: EnumDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:480`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: EnumVariant**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:487`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: FieldDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:338`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ImportItem**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:384`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ImportStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:364`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: JudgeAllStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:441`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: JudgeArm**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:494`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: JudgeArmStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:427`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: JudgeStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:435`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: LinkDefStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:106`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: LinkOffsetStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:128`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Module**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:8`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ObjectLinkDefStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:117`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: OverlayApplyStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:225`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: OverlayConflictRule**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:210`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: OverlayDefStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:182`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: OverlayDetachStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:238`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: OverlaySpawnRule**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:217`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Param**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:390`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ReturnStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:413`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: SuccessorDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:290`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: SweepArm**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:464`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: SweepStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:472`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: TransitionDef**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:303`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: TupleBindStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:32`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: UnitDecl**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:93`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: UseStmt**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:357`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **SweepArmKind::AllBody**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:453`, `crates/goblin-ast/src/lib.rs:450`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SweepArmKind::Pattern**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:451`, `crates/goblin-ast/src/lib.rs:450`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SweepArmKind::Range**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:452`, `crates/goblin-ast/src/lib.rs:450`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SweepArmRepeat::All**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:458`, `crates/goblin-ast/src/lib.rs:457`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SweepArmRepeat::First**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:459`, `crates/goblin-ast/src/lib.rs:457`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **SweepArmRepeat::Last**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:460`, `crates/goblin-ast/src/lib.rs:457`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Absorb**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:270`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Erase**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:266`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Fracture**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:268`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Merge**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:269`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Mutate**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:272`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Spawn**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:265`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Split**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:267`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **TransitionKind::Subjugate**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:271`, `crates/goblin-ast/src/lib.rs:264`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **type: Ident**
  Audience: language and tooling authors.
  Source: `crates/goblin-ast/src/lib.rs:5`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.

## Bytecode and Opcodes

- [ ] **API: name**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:200`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **enum: Opcode**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **Opcode::Add**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:54`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::AddFloat**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:68`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::AddInt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:62`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Call**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:121`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::CallBuiltin**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:130`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::CallMethod**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:115`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::ClassInstantiate**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:112`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::ClearLink**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:179`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Concat**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:75`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Div**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:57`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::DivFloat**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:71`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::DivInt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:65`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Dup**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:46`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Eq**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:78`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Ge**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:83`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::GetIndex**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:103`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::GetMember**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:108`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Gt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:82`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::ImportFile**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:163`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Jump**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:90`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::JumpIfFalse**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:92`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::JumpIfTrue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:94`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Le**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:81`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LinkDef**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:173`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LinkOffset**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:177`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadConst**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:14`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadFalse**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:20`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadGlobal**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:32`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadLocal**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:24`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadNil**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:16`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadTrue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:18`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::LoadUpvalue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:38`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Lt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:80`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MakeArray**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:98`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MakeClosure**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:137`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MakeMap**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:100`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MakePair**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:142`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MakeRange**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:144`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MakeRangeInclusive**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:146`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Mul**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:56`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MulFloat**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:70`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::MulInt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:64`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Ne**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:79`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Neg**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:59`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::NegFloat**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:72`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::NegInt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:67`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Not**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:86`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::ObjectDecision**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:181`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::ObjectLinkDef**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:175`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::OverlayApply**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:169`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::OverlayDef**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:167`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::OverlayDetach**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:171`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Overwrite**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:51`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Pop**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:44`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Quick**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:151`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::RegisterAction**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:187`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Rem**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:58`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::RemInt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:66`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Return**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:126`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::SelfField**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:195`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::SetField**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:110`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::SetIndex**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:105`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::StoreGlobal**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:34`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::StoreLocal**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:28`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::StoreUpvalue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:40`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::StringInterp**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:191`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::Sub**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:55`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::SubFloat**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:69`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::SubInt**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:63`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::ToPct**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:140`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::TryBegin**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:156`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::TryEnd**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:158`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Opcode::UnitDecl**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/opcode.rs:183`, `crates/goblin-vm/src/opcode.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.

## CLI and Configuration

- [ ] **API: diff_paths**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1004`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: load**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/config.rs:15`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: resolve_module_path**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/config.rs:23`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **command/workflow: main**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:124`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run devserver**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:51`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run devserver with proxies**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1824`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run gql parse**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:646`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run lex check**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:438`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run parse**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:593`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run repl**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1229`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run repl vm**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1131`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run run**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1534`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run run vm**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1098`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **command/workflow: run run with args**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:1665`
  Coverage: Document command syntax, flags/environment, files used, exit status, and examples.
- [ ] **goblin --help**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:136`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin --version**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:130`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin gql-parse**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:242`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin lex --check**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:227`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin new**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:153`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin parse**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:237`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin repl**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:221`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin run**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:332`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **goblin start**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:163`
  Coverage: Document invocation, arguments/options, environment and files, output, exit codes, and examples.
- [ ] **struct: ProjectConfig**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/config.rs:6`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **VM engine selection (`--vm` / `GOBLIN_ENGINE=vm`)**
  Audience: CLI users.
  Source: `crates/goblin-cli/src/main.rs:209`, `crates/goblin-cli/src/main.rs:210`, `crates/goblin-cli/src/main.rs:211`
  Coverage: Document precedence, supported commands, behavioral differences, exit status, and examples.

## Compiler and Lowering

- [ ] **API: compile_action**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/compiler.rs:274`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: compile_action**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/compiler.rs:2314`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: compile_module**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/compiler.rs:246`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: compile_module**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/compiler.rs:2309`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/compiler.rs:239`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **struct: Compiler**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/compiler.rs:210`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.

## DES Architecture

- [ ] **API: add_link**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:138`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: add_overlay**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:107`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: apply_transition**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/tick.rs:121`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: as_float**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:17`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: begin_tick**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/tick.rs:73`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: candidates_for_class**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:197`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: clear_owner**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:88`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: create**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:71`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: drain_pending**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:109`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: end_tick**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/tick.rs:81`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: erase**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:165`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: get**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:121`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: get_by_name**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:135`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: get_by_name_mut**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:140`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: get_by_uuid**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:145`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: get_mut**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:128`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: grid_position**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:174`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: handle_for_interp_uuid**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:115`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: handle_for_name**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:150`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: handle_for_uuid**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:154`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: handles_at_cell**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:178`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: handles_for_class**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:70`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: has_pending**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:114`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: hosts_of**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:131`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: infer_trait_fields**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:121`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: insert_class**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:57`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: is_trait_value**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:25`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: live_count**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:208`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: live_entities**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:201`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: live_handles**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:193`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: mark_transition_candidate**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:187`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: name_for_handle**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:158`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: new**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:73`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: new**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:57`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: new**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/tick.rs:65`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: overlays_on**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:127`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: owned_by**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:96`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: owner_of**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:101`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: place_on_grid**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:154`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: remove_all**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:207`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: remove_class**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:61`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: remove_from_grid**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:166`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: remove_link**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:143`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: remove_overlay**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:112`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: rename**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/store.rs:184`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: set_owner**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:77`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: set_pending**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/entity.rs:94`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **API: unmark_transition_candidate**
  Audience: embedders and contributors.
  Source: `crates/goblin-des/src/index.rs:191`
  Coverage: Document operation, state mutation, complexity/ordering concerns, and failure behavior.
- [ ] **enum: FieldValue**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:8`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **enum: TickPhase**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:6`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **enum: TransitionResult**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:103`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **FieldValue::Bool**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:10`, `crates/goblin-des/src/entity.rs:8`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **FieldValue::Float**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:12`, `crates/goblin-des/src/entity.rs:8`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **FieldValue::Int**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:11`, `crates/goblin-des/src/entity.rs:8`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **FieldValue::Nil**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:9`, `crates/goblin-des/src/entity.rs:8`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **FieldValue::Str**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:13`, `crates/goblin-des/src/entity.rs:8`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **struct: Entity**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/entity.rs:42`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: EntityHandle**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/store.rs:10`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: EntityIndex**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/index.rs:10`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: EntityStore**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/store.rs:43`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: LinkId**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/index.rs:52`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: OverlayInstanceId**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/index.rs:48`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: TickRunner**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:59`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **struct: TickStats**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:37`
  Coverage: Document simulation role, identity/lifecycle, indexing, transition/tick behavior, and runtime integration.
- [ ] **TickPhase::ActionExec**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:14`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TickPhase::DecisionEval**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:12`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TickPhase::IndexSync**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:20`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TickPhase::OverlayApply**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:10`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TickPhase::OverlayDecay**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:8`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TickPhase::Swap**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:18`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TickPhase::TransitionEval**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:16`, `crates/goblin-des/src/tick.rs:6`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TransitionResult::Absorbed**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:113`, `crates/goblin-des/src/tick.rs:103`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TransitionResult::Erased**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:105`, `crates/goblin-des/src/tick.rs:103`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TransitionResult::Fragmented**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:111`, `crates/goblin-des/src/tick.rs:103`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TransitionResult::Mutated**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:107`, `crates/goblin-des/src/tick.rs:103`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TransitionResult::Replaced**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:109`, `crates/goblin-des/src/tick.rs:103`
  Coverage: Document semantic meaning and effect during DES processing.
- [ ] **TransitionResult::Subjugated**
  Audience: language users, simulation authors, and contributors.
  Source: `crates/goblin-des/src/tick.rs:115`, `crates/goblin-des/src/tick.rs:103`
  Coverage: Document semantic meaning and effect during DES processing.

## DES Tick Runtime

- [ ] **API: apply_overlay_modifiers**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/tick.rs:986`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: run_tick**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/tick.rs:18`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.

## Debugging and Introspection

- [ ] **API: describe_collection**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:117`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: disassemble**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:12`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: dump_session**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:76`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: dump_vm**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:90`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: format_op**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:41`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: format_stash_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:219`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: trace_op**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/debug.rs:144`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.

## Development Server

- [ ] **API: start**
  Audience: application developers.
  Source: `crates/goblin-devserver/src/lib.rs:21`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **struct: DevOptions**
  Audience: application developers.
  Source: `crates/goblin-devserver/src/lib.rs:7`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.

## Diagnostics and Error Codes

- [ ] **API: docs_link**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:193`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: error**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:135`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:39`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:67`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:100`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new_with_code**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:123`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: runtime**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:144`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: warning**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:139`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_code**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:174`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_help**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:163`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_label**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:70`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_link**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:168`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_note**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:158`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_secondary**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:153`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: with_snippet**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:180`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **enum: Severity**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:8`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **Severity::Error**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:9`, `crates/goblin-diagnostics/src/lib.rs:8`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Severity::Help**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:12`, `crates/goblin-diagnostics/src/lib.rs:8`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Severity::Note**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:11`, `crates/goblin-diagnostics/src/lib.rs:8`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Severity::Warning**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:10`, `crates/goblin-diagnostics/src/lib.rs:8`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **struct: Diagnostic**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:77`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: LabeledSpan**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:61`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Span**
  Audience: language users and tooling authors.
  Source: `crates/goblin-diagnostics/src/lib.rs:28`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.

## GQL Query Language

- [ ] **API: parse_query**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:74`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: pretty**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:339`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **enum: Action**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:6`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **enum: FieldSel**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:43`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **enum: LineKind**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:34`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **enum: Scope**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:9`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **FieldSel::All**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:45`, `crates/goblin-gql/src/lib.rs:43`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **FieldSel::Default**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:44`, `crates/goblin-gql/src/lib.rs:43`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **FieldSel::Some**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:46`, `crates/goblin-gql/src/lib.rs:43`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **LineKind::BlockCloseMarker**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:38`, `crates/goblin-gql/src/lib.rs:34`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **LineKind::Command**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:37`, `crates/goblin-gql/src/lib.rs:34`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **LineKind::Comment**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:39`, `crates/goblin-gql/src/lib.rs:34`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **LineKind::FieldSpec**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:35`, `crates/goblin-gql/src/lib.rs:34`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **LineKind::Relation**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:36`, `crates/goblin-gql/src/lib.rs:34`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Scope::All**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:12`, `crates/goblin-gql/src/lib.rs:9`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Scope::Ident**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:13`, `crates/goblin-gql/src/lib.rs:9`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Scope::IdentWithId**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:14`, `crates/goblin-gql/src/lib.rs:9`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Scope::Latest**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:10`, `crates/goblin-gql/src/lib.rs:9`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Scope::Oldest**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:11`, `crates/goblin-gql/src/lib.rs:9`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **struct: DslError**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:50`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Node**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:27`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Query**
  Audience: language users and embedders.
  Source: `crates/goblin-gql/src/lib.rs:18`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.

## Grammar and Syntax

- [ ] **block terminators: `xx` and `end`**
  Audience: language users.
  Source: `crates/goblin-lexer/src/lib.rs:1205`, `crates/goblin-parser/src/lib.rs:1928`, `crates/goblin-parser/src/lib.rs:1935`, `crates/goblin-parser/src/lib.rs:1941`
  Coverage: Document that every block form accepts both terminators, placement/layout rules, nested blocks, and missing-terminator diagnostics.
- [ ] **grammar production: action after keyword**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:2369`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: additive**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9094`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: args colon**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:11601`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: args paren**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:11578`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: assign**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:577`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: attempt stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5364`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: bind stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:2633`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: class decl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:3786`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: class decl keyword**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:4474`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: clear link**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7501`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: coalesce**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:655`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: coalesce impl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:8938`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: collect stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5300`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: compare**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:8994`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: decl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:4427`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: definedness lvalue**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:768`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: enum decl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:4298`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: expr**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:8033`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: field chain line class**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:2035`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: for stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:4986`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: format args pexpr after lparen**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:1007`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: free action**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:2437`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: if stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:4587`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: import**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7844`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: int literal to i128**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:188`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: interpolation lvalue**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:1230`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: judge all stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9529`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: judge condition**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9875`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: judge stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9378`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: kv bind list**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:3587`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: kv bind list judge**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9679`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: link def**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7268`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: link formula**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7623`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: literal token**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:327`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: local bind**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:471`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: matrix cell**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:3758`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: matrix decl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:3616`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: member**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:11158`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: module**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5537`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: multiplicative**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9140`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: name colon expr pair**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:3257`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: object link def**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7329`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: overlay apply**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7168`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: overlay def**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:6794`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: overlay detach**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7213`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: overlay stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:6761`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: postfix**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:10923`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: power**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9168`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: primary**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:545`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: primary impl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:8078`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: provoke stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9227`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: range**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9079`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: repeat stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5159`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: return stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:1964`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5691`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt block**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:3419`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt block until**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5336`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt block until dedent or close**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:10206`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt block until dedent or next arm**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:10178`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt block until dedent or next case**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9186`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: stmt sequence until dedent or next case**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9802`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: sweep arm header**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:10126`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: sweep stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:9905`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: sweep target exprs**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:10093`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: transition def**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7634`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: unary**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:10234`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: unit decl**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:7537`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: unless stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:4875`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: use**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:8001`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **grammar production: while stmt**
  Audience: language users and tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5077`
  Coverage: Document concrete syntax, block terminators (`xx` and `end` where applicable), nesting, precedence, and parser diagnostics.
- [ ] **guarded destructuring / bind guard (`?? =>`)**
  Audience: language users.
  Source: `crates/goblin-parser/src/lib.rs:2875`, `crates/goblin-parser/src/lib.rs:2963`, `crates/goblin-parser/src/lib.rs:3018`, `crates/goblin-interpreter/src/lib.rs:5656`, `crates/goblin-vm/src/compiler.rs:450`
  Coverage: Document syntax, nil/nix trigger semantics, parser lowering, bound-name visibility, guard statement execution, examples, and interactions with coalescing.
- [ ] **repeat map destructuring (`as key, value`)**
  Audience: language users.
  Source: `crates/goblin-parser/src/lib.rs:5197`, `crates/goblin-interpreter/src/lib.rs:19576`, `crates/goblin-interpreter/src/lib.rs:19605`
  Coverage: Document map iteration binding forms, ordering, key/value behavior, legacy form, and examples.
- [ ] **repeat object-field destructuring**
  Audience: language users.
  Source: `crates/goblin-interpreter/src/lib.rs:19849`, `crates/goblin-parser/src/lib.rs:5159`
  Coverage: Document field-name destructuring during repeat, missing fields, binding scope, and examples.
- [ ] **tuple destructuring bindings**
  Audience: language users.
  Source: `crates/goblin-parser/src/lib.rs:2633`, `crates/goblin-parser/src/lib.rs:2824`, `crates/goblin-interpreter/src/lib.rs:3444`, `crates/goblin-vm/src/compiler.rs:544`
  Coverage: Document tuple targets, arity, tether/retether/shadow modes, errors, object restrictions, and examples.

## Grid Runtime

- [ ] **API: cell_to_region**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:247`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: cell_to_tile**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:239`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: cells_with_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:96`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: cells_with_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:516`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: contains**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:560`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: count_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:90`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: count_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:512`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: from_int**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:19`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: from_str**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:27`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:65`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:119`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:149`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:430`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:556`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get_mut**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:558`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get_snapshot**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:529`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get_world_default**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:405`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: has_hierarchy**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:315`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: has_snapshot**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:537`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: in_bounds**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:311`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: insert**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:552`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: is_occupied**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:49`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: is_void**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:48`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: is_void**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:80`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: is_void**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:447`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: layer**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:416`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: layer_mut**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:420`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: layer_names**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:130`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: layer_names**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:160`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: layer_names**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:424`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: layer_or_create**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:409`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: names**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:562`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: neighbors**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:451`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:61`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:115`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:145`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:278`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:550`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: occupied**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:491`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: occupied_cells**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:84`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: region_at**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:365`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: region_at_cell**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:349`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: region_at_cell_mut**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:356`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: region_at_mut**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:371`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: region_index**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:256`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: resolve**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:379`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: set**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:69`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: set**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:123`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: set**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:153`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: set**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:437`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: set_world_default**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:398`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: snapshot**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:102`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tick_db_begin**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:520`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tick_db_commit**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:525`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tile_at**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:335`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tile_at_cell**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:319`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tile_at_cell_mut**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:326`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tile_at_mut**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:341`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tile_index**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:252`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: tile_to_region**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:243`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: try_default**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:235`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: try_new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:182`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: unoccupied_cells**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:498`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: void**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:76`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: void_cell**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:441`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **CellState::Occupied**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:42`, `crates/goblin-vm/src/grid.rs:41`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **CellState::Unoccupied**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:43`, `crates/goblin-vm/src/grid.rs:41`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **CellState::Void**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:44`, `crates/goblin-vm/src/grid.rs:41`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **enum: CellState**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:41`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **enum: NeighborMode**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:11`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **NeighborMode::Eight**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:13`, `crates/goblin-vm/src/grid.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **NeighborMode::Four**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:12`, `crates/goblin-vm/src/grid.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **NeighborMode::Hex**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:14`, `crates/goblin-vm/src/grid.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **NeighborMode::Wrapped**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:15`, `crates/goblin-vm/src/grid.rs:11`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **struct: GridLayer**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:55`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: GridRegion**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:140`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: GridStore**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:545`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: GridTile**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:110`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: GridWorld**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:264`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: HierarchyConfig**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/grid.rs:170`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.

## Host Embedding

- [ ] **API: add_app**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:213`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: add_proxy**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:212`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: add_static**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:211`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: bind**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:194`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: build**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:214`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: caps**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:210`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: limits**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:209`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:192`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: proxy**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:200`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: run**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:324`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **enum: HostError**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:916`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **enum: Step**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:151`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **HostError::Bind**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:918`, `crates/goblin-host/src/lib.rs:916`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **HostError::Internal**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:922`, `crates/goblin-host/src/lib.rs:916`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **HostError::Io**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:920`, `crates/goblin-host/src/lib.rs:916`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Step::App**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:154`, `crates/goblin-host/src/lib.rs:151`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Step::Proxy**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:153`, `crates/goblin-host/src/lib.rs:151`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **Step::Static**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:152`, `crates/goblin-host/src/lib.rs:151`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **struct: Caps**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:113`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Ctx**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:132`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Host**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:217`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: HostBuilder**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:187`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: HostConfig**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:165`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Limits**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:10`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: ProxyCfg**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:142`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: ProxyRule**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:158`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Request**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:128`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Response**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:130`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: StaticCfg**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:135`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **trait: WebApp**
  Audience: embedders.
  Source: `crates/goblin-host/src/lib.rs:147`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.

## Interpreter Embedding API

- [ ] **API: as_slice**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:147`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: backend_name**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:202`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: box_dump**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:811`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: des_erase**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1204`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: des_flush_pending**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1178`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: des_register**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1170`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: enter_file**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:913`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: eval_expr**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1313`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: eval_line**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1259`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: eval_module**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1249`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: eval_stmt**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1244`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: from_vec**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:136`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: gen_uuid**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:976`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:111`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1347`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get_export**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:193`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get_module_env**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:197`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get_or_compile**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:71`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get_var**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1035`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: get_var_mut**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:947`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: highlight_code**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:58`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: history_len**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1346`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: import_base_dir**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:900`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: import_failed_focus_inner**
  Audience: embedders and tooling authors.
  Source: `crates/goblin-interpreter/src/diagnostics.rs:30`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: insert**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:170`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: iter**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:228`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: load_box_toml**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:615`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: load_glam_box_toml**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:676`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: load_module**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:118`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: maybe_rebucket**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:198`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: md_to_html**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:8`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: metrics_map**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:208`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: new**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:65`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: new**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:821`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: new**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:112`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: next_f64**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1000`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: next_u128**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:990`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: pop_block**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1020`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: push_back**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:162`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: push_block**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1018`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: register_token_value**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:923`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: remove**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:183`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: reseed**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:971`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: resolve_token_value**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:929`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: rt**
  Audience: embedders and tooling authors.
  Source: `crates/goblin-interpreter/src/diagnostics.rs:15`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: set**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:121`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: set_global**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:937`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: set_module_var**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:201`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: set_var**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:1060`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: to_vec**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:155`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **enum: ExportedItem**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:105`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: ImportBaseMode**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:371`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: ModifierValue**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:419`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: OverlayApplyBehavior**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:408`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: SeqKind**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:83`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: SeqView**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:223`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: SpreadRule**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:398`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: SweepCtl**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:22516`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: Value**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **ExportedItem::Action**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:106`, `crates/goblin-interpreter/src/modules.rs:105`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **ExportedItem::Class**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:107`, `crates/goblin-interpreter/src/modules.rs:105`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **ExportedItem::Enum**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:108`, `crates/goblin-interpreter/src/modules.rs:105`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **ImportBaseMode::ImporterDir**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:373`, `crates/goblin-interpreter/src/lib.rs:371`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **ImportBaseMode::ProjectRoot**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:372`, `crates/goblin-interpreter/src/lib.rs:371`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **ModifierValue::Dynamic**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:421`, `crates/goblin-interpreter/src/lib.rs:419`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **ModifierValue::Static**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:420`, `crates/goblin-interpreter/src/lib.rs:419`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **OverlayApplyBehavior::Caps**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:410`, `crates/goblin-interpreter/src/lib.rs:408`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **OverlayApplyBehavior::Replaces**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:412`, `crates/goblin-interpreter/src/lib.rs:408`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **OverlayApplyBehavior::Stacks**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:414`, `crates/goblin-interpreter/src/lib.rs:408`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SeqView::Slice**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:224`, `crates/goblin-interpreter/src/lib.rs:223`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SpreadRule::All**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:400`, `crates/goblin-interpreter/src/lib.rs:398`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SpreadRule::Channel**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:399`, `crates/goblin-interpreter/src/lib.rs:398`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SpreadRule::Nearby**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:401`, `crates/goblin-interpreter/src/lib.rs:398`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SpreadRule::Ownership**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:402`, `crates/goblin-interpreter/src/lib.rs:398`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SpreadRule::Predicate**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:403`, `crates/goblin-interpreter/src/lib.rs:398`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **struct: ClassRelations**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:236`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: FileGuard**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:376`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: FormatSpec**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:298`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: GlamMeta**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:672`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: LinkDef**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:472`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: LinkOffset**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:486`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: Module**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:96`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: ModuleCache**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/modules.rs:92`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: OverlayConflictRule**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:440`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: OverlayDef**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:425`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: OverlayInstance**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:453`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: OverlaySpawnRule**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:446`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: RegexCache**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:60`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: ResponseState**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:389`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: Seq**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:104`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: SeqMetrics**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:86`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: Session**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:492`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **SweepCtl::Continue**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:22517`, `crates/goblin-interpreter/src/lib.rs:22516`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SweepCtl::Return**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:22520`, `crates/goblin-interpreter/src/lib.rs:22516`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SweepCtl::SkipMatch**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:22518`, `crates/goblin-interpreter/src/lib.rs:22516`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **SweepCtl::StopAll**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:22519`, `crates/goblin-interpreter/src/lib.rs:22516`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **trait: SourceLookup**
  Audience: embedders and tooling authors.
  Source: `crates/goblin-interpreter/src/diagnostics.rs:9`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **type: Diag**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:31`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **Value::Array**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:252`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Big**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:246`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Bool**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:249`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Char**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:248`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Class**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:292`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::CtrlReturn**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:261`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::CtrlSkip**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:259`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::CtrlStop**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:260`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Enum**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:287`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Float**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:245`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Formatted**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:251`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::GridRef**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:282`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Int**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:244`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Map**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:253`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::MapOrd**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:254`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Nil**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:257`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Object**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:262`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Pair**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:255`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Pct**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:250`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Ref**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:275`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Seq**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:256`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Str**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:247`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **Value::Unit**
  Audience: embedders and contributors.
  Source: `crates/goblin-interpreter/src/lib.rs:258`, `crates/goblin-interpreter/src/lib.rs:243`
  Coverage: Document semantic meaning, payload, and API behavior.

## Lexer and Parser APIs

- [ ] **API: derr**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/diagnostics_ext.rs:17`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: derr_expected_found**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/diagnostics_ext.rs:29`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: derr_help**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/diagnostics_ext.rs:23`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: lex**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1693`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: new**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:291`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: Operator**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:47`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: parse_expr_preview**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:11611`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: parse_module**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:5537`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: parse_program**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:11616`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: parse_program_preview**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:11637`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: s**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/diagnostics_ext.rs:5`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: s_help**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/diagnostics_ext.rs:10`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **API: shadow**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:72`
  Coverage: Document inputs, outputs, state effects, diagnostics/errors, and a minimal embedding example.
- [ ] **enum: LvSeg**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:268`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: StrPart**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:258`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **enum: TokenKind**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **LvSeg::IndexIdent**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:271`, `crates/goblin-parser/src/lib.rs:268`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **LvSeg::IndexNumber**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:270`, `crates/goblin-parser/src/lib.rs:268`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **LvSeg::Member**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:269`, `crates/goblin-parser/src/lib.rs:268`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **StrPart::LValue**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:260`, `crates/goblin-parser/src/lib.rs:258`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **StrPart::Text**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:259`, `crates/goblin-parser/src/lib.rs:258`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **struct: Parser**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:276`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **struct: Token**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:53`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.
- [ ] **TokenKind::Act**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:18`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Action**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:19`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Blob**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:29`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Char**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:23`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::ClassIdent**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:16`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Date**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:30`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::DateTime**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:32`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Dedent**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:27`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Duration**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:28`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Eof**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:35`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Export**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:38`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Float**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:21`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::HashIdent**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:17`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Ident**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:15`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Import**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:36`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Indent**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:26`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Int**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:20`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Money**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:34`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Newline**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:25`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Op**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:33`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Shadow**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:24`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::String**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:22`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Time**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:31`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::TripleBraceClose**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:41`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::TripleBraceOpen**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:40`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Use**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:37`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **TokenKind::Vault**
  Audience: tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:39`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document semantic meaning, payload, and API behavior.
- [ ] **type: ParseResult**
  Audience: tooling authors.
  Source: `crates/goblin-parser/src/lib.rs:274`
  Coverage: Document construction, fields/variants, invariants, lifecycle, thread/state assumptions, and examples.

## Lexical Structure

- [ ] **lexical form: binary integer**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:512`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: char literal**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1128`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: class identifier**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1351`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: decimal number**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:594`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: escape sequence**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:807`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: hash identifier**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1387`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: hex integer**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:372`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: identifier**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1201`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: leading dot float**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:765`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: lex**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1693`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: money dollar**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1457`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: money unicode**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:1520`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: number**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:748`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: octal integer**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:430`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: push token**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:154`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **lexical form: string literal**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:968`
  Coverage: Document accepted source forms, escapes/separators, edge cases, and diagnostics.
- [ ] **token kind: Act**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:18`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Action**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:19`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Blob**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:29`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Char**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:23`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: ClassIdent**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:16`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Date**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:30`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: DateTime**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:32`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Dedent**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:27`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Duration**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:28`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Eof**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:35`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Export**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:38`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Float**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:21`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: HashIdent**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:17`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Ident**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:15`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Import**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:36`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Indent**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:26`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Int**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:20`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Money**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:34`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Newline**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:25`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Op**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:33`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Shadow**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:24`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: String**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:22`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Time**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:31`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: TripleBraceClose**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:41`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: TripleBraceOpen**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:40`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Use**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:37`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.
- [ ] **token kind: Vault**
  Audience: language users and tooling authors.
  Source: `crates/goblin-lexer/src/lib.rs:39`, `crates/goblin-lexer/src/lib.rs:14`
  Coverage: Document spelling, lexical boundaries, precedence implications, and invalid forms.

## Source Files and Spans

- [ ] **API: add**
  Audience: tooling authors.
  Source: `crates/goblin-source/src/lib.rs:1`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.

## VM Embedding API

- [ ] **API: execute_source**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/exec.rs:41`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.

## VM Errors

- [ ] **API: type_error**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:56`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **enum: GoblinError**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:5`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **GoblinError::ArityMismatch**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:31`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::CompileError**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:43`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::CrossWorkerAccess**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:16`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::CrossWorkerMutation**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:13`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::DivisionByZero**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:22`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::EmptySlot**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:10`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::IndexOutOfBounds**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:25`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::KeyNotFound**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:28`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::NotCallable**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:34`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::NotImplemented**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:46`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::Runtime**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:49`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::StackOverflow**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:37`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::StaleAddress**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:7`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::TypeError**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:19`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::UndefinedVariable**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:40`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **GoblinError::WithLocation**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/error.rs:52`, `crates/goblin-vm/src/error.rs:5`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.

## VM Execution Model

- [ ] **API: call_named**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:1358`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: eval_tick_expr**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:1495`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: eval_tick_expr_bool**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:1524`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: execute**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:79`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:74`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: quicken**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:1230`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: run_until_depth**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:1328`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **struct: CallFrame**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:16`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: Vm**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/vm.rs:66`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.

## VM Session and State

- [ ] **API: deref_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:205`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: ensure_globals**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:189`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get_global**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:200`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:137`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: next_u128**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:176`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: set_global**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:195`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: with_worker_id**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:182`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **enum: OverlayApplyBehavior**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:30`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: LinkDef**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:51`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: LinkOffset**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:60`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: OverlayDef**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:15`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: OverlayInstance**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:35`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: ResponseState**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:67`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: Session**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/session.rs:75`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.

## Values, Memory, and Collection Backends

- [ ] **BackendHint::Auto**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:958`, `crates/goblin-vm/src/value.rs:957`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BackendHint::ChunkedSeq**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:961`, `crates/goblin-vm/src/value.rs:957`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BackendHint::FlatArray**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:959`, `crates/goblin-vm/src/value.rs:957`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BackendHint::HashMapBackend**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:963`, `crates/goblin-vm/src/value.rs:957`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BackendHint::RingBuf**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:960`, `crates/goblin-vm/src/value.rs:957`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **BackendHint::SmallMap**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:962`, `crates/goblin-vm/src/value.rs:957`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CollectionLayout::ChunkedSeq**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:788`, `crates/goblin-vm/src/value.rs:785`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CollectionLayout::FlatArray**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:786`, `crates/goblin-vm/src/value.rs:785`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CollectionLayout::HashMapBackend**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:790`, `crates/goblin-vm/src/value.rs:785`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CollectionLayout::RingBuf**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:787`, `crates/goblin-vm/src/value.rs:785`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **CollectionLayout::SmallMap**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:789`, `crates/goblin-vm/src/value.rs:785`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **enum: BackendHint**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:957`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: CollectionLayout**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:785`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: UpvalueDescriptor**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:261`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **enum: Value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:48`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Address**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:7`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: ChunkedSeq**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:884`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Closure**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:323`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: CollectionMeta**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:928`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: CollectionValue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:744`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: CompiledModule**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:288`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: FormatSpec**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:26`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: FunctionObject**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:270`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: RingBuf**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:795`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Seq**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:34`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Stash**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:249`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: Tether**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:17`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **struct: UpvalueCell**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:307`
  Coverage: Document purpose, invariants, fields/variants, lifecycle, and user-visible consequences.
- [ ] **UpvalueDescriptor::Local**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:263`, `crates/goblin-vm/src/value.rs:261`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **UpvalueDescriptor::Upvalue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:265`, `crates/goblin-vm/src/value.rs:261`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Array**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:62`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Big**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:55`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Bool**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:52`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Builtin**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:102`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Char**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:57`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Class**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:88`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Closure**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:99`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Collection**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:92`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::CtrlReturn**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:71`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::CtrlSkip**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:69`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::CtrlStop**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:70`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Enum**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:83`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Float**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:54`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Formatted**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:61`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Function**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:96`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::GridRef**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:82`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Int**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:53`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Map**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:63`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::MapOrd**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:64`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Nil**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:50`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Object**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:74`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Pair**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:65`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Pct**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:56`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Ref**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:81`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Seq**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:66`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Str**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:58`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.
- [ ] **Value::Unit**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/value.rs:51`, `crates/goblin-vm/src/value.rs:48`
  Coverage: Document meaning, payload, construction path, and observable behavior.

## Workers and Swarm Execution

- [ ] **API: execute_sync**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:221`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: from_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:35`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: get**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:296`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: into_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:119`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: new**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:283`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: recv**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:240`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: recv_message**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:194`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: run**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:179`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: run_sync**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:288`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: send_value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:228`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: spawn_worker**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:257`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: stop**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:235`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: stop_all**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:300`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: try_recv**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:246`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **API: try_recv_message**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:199`
  Coverage: Document signature semantics, preconditions, state effects, return/error behavior, and a minimal usage example.
- [ ] **enum: Message**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:146`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **enum: TransferValue**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **Message::Error**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:149`, `crates/goblin-vm/src/worker.rs:146`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Message::Stop**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:148`, `crates/goblin-vm/src/worker.rs:146`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **Message::Value**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:147`, `crates/goblin-vm/src/worker.rs:146`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **struct: Worker**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:159`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: WorkerHandle**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:208`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **struct: WorkerPool**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:278`
  Coverage: Document API contract, invariants, ownership/threading model, failure modes, and examples.
- [ ] **TransferValue::Array**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:28`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **TransferValue::Bool**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:24`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **TransferValue::Float**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:26`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **TransferValue::Int**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:25`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **TransferValue::Map**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:29`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **TransferValue::Nil**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:23`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.
- [ ] **TransferValue::Str**
  Audience: embedders and VM contributors.
  Source: `crates/goblin-vm/src/worker.rs:27`, `crates/goblin-vm/src/worker.rs:22`
  Coverage: Document semantics, operands/payload, state transition, and error behavior.

## YALL Data Format

- [ ] **API: array**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:21`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: as_array**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:32`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: as_map**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:28`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: as_str**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:36`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: bool**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:17`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: float**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:19`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: int**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:18`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: internal**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/error.rs:21`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: is_array**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:26`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: is_map**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:25`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: is_null**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:24`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: map**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:22`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: minify**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/writer.rs:295`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/error.rs:12`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:32`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: new**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/parser.rs:23`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: null**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:16`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: parse**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/parser.rs:86`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: str**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:20`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: tokenize**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:170`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_minify**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lib.rs:62`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_parse**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lib.rs:31`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_parse_file**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lib.rs:37`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_pretty**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lib.rs:57`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_stringify**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lib.rs:45`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_write**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/writer.rs:7`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **API: yall_write_file**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lib.rs:50`
  Coverage: Document invocation, inputs, outputs, side effects, errors, and example usage.
- [ ] **enum: TokenKind**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **enum: YallValue**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:5`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Lexer**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:22`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Parser**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/parser.rs:17`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: Token**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:16`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **struct: YallError**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/error.rs:5`
  Coverage: Document purpose, configuration/data model, supported operations, errors, and examples.
- [ ] **TokenKind::Bare**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:11`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::Colon**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:9`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::Comma**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:10`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::LBrace**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:5`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::LBracket**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:7`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::RBrace**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:6`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::RBracket**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:8`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **TokenKind::Str**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/lexer.rs:12`, `crates/goblin-yall/src/lexer.rs:4`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Array**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:11`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Bool**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:7`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Float**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:9`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Int**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:8`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Map**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:12`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Null**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:6`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.
- [ ] **YallValue::Str**
  Audience: language users and embedders.
  Source: `crates/goblin-yall/src/value.rs:10`, `crates/goblin-yall/src/value.rs:5`
  Coverage: Document meaning, accepted inputs, and observable behavior.

## Existing Documentation Index

Use this index to reconcile, reuse, or correct existing prose while completing the checklist.

### `docs/cheat-sheet.md`

- Goblin Language AI Cheat Sheet v1.5.6: `docs/cheat-sheet.md:1`
- GETTING STARTED: `docs/cheat-sheet.md:24`
- Basic Syntax Overview: `docs/cheat-sheet.md:26`
- Identifiers & the dot: `docs/cheat-sheet.md:31`
- Comments: `docs/cheat-sheet.md:46`
- Variables: `docs/cheat-sheet.md:55`
- LANGUAGE GUIDE: `docs/cheat-sheet.md:66`
- Core Concepts: `docs/cheat-sheet.md:68`
- Operators & Precedence (high → low): `docs/cheat-sheet.md:70`
- Key Operators: `docs/cheat-sheet.md:92`
- Data Types: `docs/cheat-sheet.md:132`
- Primitives: `docs/cheat-sheet.md:134`
- String Features: `docs/cheat-sheet.md:143`
- Casting: `docs/cheat-sheet.md:151`
- Quick rules: `docs/cheat-sheet.md:161`
- Errors: `docs/cheat-sheet.md:211`
- Cheat-ready examples: `docs/cheat-sheet.md:219`
- Calling Operations: `docs/cheat-sheet.md:229`
- Method-style (dot) — chaining powerhouse: `docs/cheat-sheet.md:235`
- Prefix-style (English-y) — for zero-arg & special forms: `docs/cheat-sheet.md:264`
- Prefix-Style Calls (What They're For): `docs/cheat-sheet.md:286`
- Design goals: `docs/cheat-sheet.md:342`
- What to do instead: `docs/cheat-sheet.md:348`
- Common pitfalls (and fixes): `docs/cheat-sheet.md:373`
- Strings: `docs/cheat-sheet.md:388`
- String/Text Operations (Core): `docs/cheat-sheet.md:390`
- Zero-arg (paren-optional via dot): `docs/cheat-sheet.md:392`
- Trimming & stripping (lead/trail family): `docs/cheat-sheet.md:419`
- Everyday text sugar: `docs/cheat-sheet.md:430`
- Literal find & replace (non-regex): `docs/cheat-sheet.md:444`
- Replace / remove / drop (strings are immutable): `docs/cheat-sheet.md:473`
- String Literals: `docs/cheat-sheet.md:482`
- Literals: raw & trim_lead at parse time: `docs/cheat-sheet.md:493`
- Strings — Split & Join: `docs/cheat-sheet.md:522`
- Split: `docs/cheat-sheet.md:526`
- Join: `docs/cheat-sheet.md:541`
- Quick Reference additions: `docs/cheat-sheet.md:559`
- Regex (lean core; full power via glam): `docs/cheat-sheet.md:575`
- Collections: `docs/cheat-sheet.md:601`
- Collection Operations: `docs/cheat-sheet.md:613`
- 🎲 Pick / 🌾 Reap / ✨ Unique — Random Selection, Destructiveness & Duplication: `docs/cheat-sheet.md:647`
- Syntax: `docs/cheat-sheet.md:658`
- Semantics & Defaults: `docs/cheat-sheet.md:693`
- Errors: `docs/cheat-sheet.md:724`
- Examples: `docs/cheat-sheet.md:733`
- 15. Date & Time (Core): `docs/cheat-sheet.md:775`
- 15.1 Types: `docs/cheat-sheet.md:785`
- 15.2 Policy & Defaults (Project‑wide): `docs/cheat-sheet.md:796`
- 15.3 Trusted Time (Server → Cache → Local) — Opt‑in: `docs/cheat-sheet.md:819`
- 15.3.1 Minimal preset: `docs/cheat-sheet.md:823`
- 15.3.2 API: `docs/cheat-sheet.md:839`
- 15.4 Construction & Parsing: `docs/cheat-sheet.md:852`
- 15.5 Duration Literals: `docs/cheat-sheet.md:875`
- 15.6 Now/Today & Zone Helpers: `docs/cheat-sheet.md:887`
- 15.7 Formatting & Accessors: `docs/cheat-sheet.md:897`
- 15.8 Arithmetic & Comparisons: `docs/cheat-sheet.md:906`
- 15.9 Truncation / Rounding: `docs/cheat-sheet.md:936`
- 15.10 Calendar‑Safe Adders (field changes, clamped): `docs/cheat-sheet.md:944`
- 15.11 Ranges: `docs/cheat-sheet.md:955`
- 15.12 Interop & Serialization (JSON/YAML/CSV): `docs/cheat-sheet.md:975`
- 15.13 Leap Seconds: `docs/cheat-sheet.md:1009`
- 15.14 Introspection & Debugging: `docs/cheat-sheet.md:1023`
- 15.15 Errors & Warnings: `docs/cheat-sheet.md:1032`
- 15.16 Compatibility & Migration (Non‑normative): `docs/cheat-sheet.md:1045`
- Design Charter (recap): `docs/cheat-sheet.md:1058`
- Binary Data: `docs/cheat-sheet.md:1063`
- Dice (only valid inside roll/roll_detail): `docs/cheat-sheet.md:1071`
- Control Flow: `docs/cheat-sheet.md:1078`
- Conditionals: `docs/cheat-sheet.md:1080`
- Between / !Between: `docs/cheat-sheet.md:1098`
- Loops: `docs/cheat-sheet.md:1138`
- Loops: `docs/cheat-sheet.md:1139`
- 5.2.x For … Where …: `docs/cheat-sheet.md:1161`
- Jump (staged strides): `docs/cheat-sheet.md:1192`
- Form: `docs/cheat-sheet.md:1196`
- Semantics: `docs/cheat-sheet.md:1211`
- Examples: `docs/cheat-sheet.md:1221`
- 5.2.x Repeat: `docs/cheat-sheet.md:1249`
- Form: `docs/cheat-sheet.md:1253`
- Error Handling: `docs/cheat-sheet.md:1276`
- Operations: `docs/cheat-sheet.md:1298`
- Definition: `docs/cheat-sheet.md:1300`
- Calls: `docs/cheat-sheet.md:1321`
- Object-Oriented Programming: `docs/cheat-sheet.md:1332`
- Classes: `docs/cheat-sheet.md:1334`
- Definition: `docs/cheat-sheet.md:1336`
- Instantiation (Template-Only): `docs/cheat-sheet.md:1348`
- Usage: `docs/cheat-sheet.md:1357`
- Rule: `docs/cheat-sheet.md:1364`
- Access & Privacy: `docs/cheat-sheet.md:1373`
- Templates: `docs/cheat-sheet.md:1390`
- Basic Templates: `docs/cheat-sheet.md:1418`
- Template Blocks: `docs/cheat-sheet.md:1428`
- With Loops: `docs/cheat-sheet.md:1438`
- Enums: `docs/cheat-sheet.md:1452`
- Definition: `docs/cheat-sheet.md:1454`
- Usage: `docs/cheat-sheet.md:1483`
- FINANCIAL PROGRAMMING: `docs/cheat-sheet.md:1503`
- Money & Currency (Cheat Sheet replacement): `docs/cheat-sheet.md:1505`
- Literals & Constructors: `docs/cheat-sheet.md:1511`
- Formatting (numbers • money • percent): `docs/cheat-sheet.md:1522`
- Policy (defaults & knobs): `docs/cheat-sheet.md:1556`
- Allowed / Forbidden ops: `docs/cheat-sheet.md:1591`
- Splitting helpers (fair & conservative): `docs/cheat-sheet.md:1614`
- Remainder ledger (global, per currency): `docs/cheat-sheet.md:1625`
- Currency conversion (explicit only): `docs/cheat-sheet.md:1637`
- Percent × Money: `docs/cheat-sheet.md:1647`
- Rounding timing: quick contrasts: `docs/cheat-sheet.md:1659`
- Common hiccups: `docs/cheat-sheet.md:1703`
- Tiny “opt-out” note: `docs/cheat-sheet.md:1718`
- Percent System (CIPO): `docs/cheat-sheet.md:1723`
- Percent Types: `docs/cheat-sheet.md:1725`
- Three Forms: `docs/cheat-sheet.md:1734`
- Money Integration: `docs/cheat-sheet.md:1746`
- 11.x Percent Variables: `docs/cheat-sheet.md:1755`
- Form: `docs/cheat-sheet.md:1759`
- Examples: `docs/cheat-sheet.md:1770`
- ADVANCED FEATURES: `docs/cheat-sheet.md:1812`
- Modules & Imports: `docs/cheat-sheet.md:1814`
- Import/Export: `docs/cheat-sheet.md:1816`
- Visibility: `docs/cheat-sheet.md:1827`
- Policy Control: `docs/cheat-sheet.md:1838`
- Glam Extensions: `docs/cheat-sheet.md:1845`
- Usage: `docs/cheat-sheet.md:1849`
- Contracts: `docs/cheat-sheet.md:1874`
- Special Forms: `docs/cheat-sheet.md:1881`
- Morphing (Temporary Type Adaptation): `docs/cheat-sheet.md:1883`
- Gmarks (Stable References): `docs/cheat-sheet.md:1900`
- compact ords to 1..N (stable order preserved): `docs/cheat-sheet.md:1923`
- Policies (Project Configuration): `docs/cheat-sheet.md:1929`
- Banish (Feature Blocking): `docs/cheat-sheet.md:1941`
- Feature IDs (namespaces): `docs/cheat-sheet.md:1946`
- core.<keyword>, op.<operator>, type.<type>, builtin.<function>, glam.<ns>.<symbol>: `docs/cheat-sheet.md:1947`
- Config (.goblin.banish.toml): `docs/cheat-sheet.md:1949`
- [[banish]]: `docs/cheat-sheet.md:1950`
- feature = "core.morph": `docs/cheat-sheet.md:1951`
- reason  = "Temporary safety": `docs/cheat-sheet.md:1952`
- CLI: `docs/cheat-sheet.md:1954`
- goblin banish <feature_id> --reason "<text>": `docs/cheat-sheet.md:1955`
- goblin unbanish <feature_id>: `docs/cheat-sheet.md:1956`
- goblin banish --list: `docs/cheat-sheet.md:1957`
- Banner (when any bans exist): `docs/cheat-sheet.md:1959`
- ⚠ This project has N banished features (run `goblin banish --list`).: `docs/cheat-sheet.md:1960`
- Non-banishable invariants (self-protection): sandbox/determinism, money safety, lockfile integrity: `docs/cheat-sheet.md:1962`
- Collections & Utilities: `docs/cheat-sheet.md:1965`
- Arrays: `docs/cheat-sheet.md:1967`
- Maps: `docs/cheat-sheet.md:1979`
- Math Helpers: `docs/cheat-sheet.md:1988`
- I/O & DATA: `docs/cheat-sheet.md:1997`
- File Operations: `docs/cheat-sheet.md:1999`
- JSON Handling: `docs/cheat-sheet.md:2016`
- JSON Options: `docs/cheat-sheet.md:2018`
- Patch: Files, Printing & I/O — Extensions: `docs/cheat-sheet.md:2040`
- New Built-ins & Defaults: `docs/cheat-sheet.md:2046`
- Built-ins: `docs/cheat-sheet.md:2048`
- JSON/YAML option defaults (explicit): `docs/cheat-sheet.md:2058`
- New Path/Filesystem Operations: `docs/cheat-sheet.md:2067`
- Path methods (method-style on strings): `docs/cheat-sheet.md:2069`
- Filesystem helpers (built-ins): `docs/cheat-sheet.md:2076`
- CSV Read (in addition to existing write): `docs/cheat-sheet.md:2093`
- In-Memory JSON (string ⇄ value): `docs/cheat-sheet.md:2112`
- Error Catalog Additions: `docs/cheat-sheet.md:2137`
- Quick Reference (additions): `docs/cheat-sheet.md:2157`
- Mini Examples: `docs/cheat-sheet.md:2192`
- ERROR REFERENCE: `docs/cheat-sheet.md:2228`
- Standard Error Catalog: `docs/cheat-sheet.md:2230`
- Split/Join specific guarantees: `docs/cheat-sheet.md:2245`
- Strings vs Collections: `docs/cheat-sheet.md:2250`
- Dot rules: `docs/cheat-sheet.md:2255`
- Core Error Types: `docs/cheat-sheet.md:2260`
- Goblin-Specific Errors: `docs/cheat-sheet.md:2266`
- List Operation Errors: `docs/cheat-sheet.md:2280`
- Errors I don't know where they go: `docs/cheat-sheet.md:2290`
- EXAMPLES: `docs/cheat-sheet.md:2297`
- Tiny end-to-end examples: `docs/cheat-sheet.md:2299`
- LANGUAGE REFERENCE: `docs/cheat-sheet.md:2333`
- Reserved Words: `docs/cheat-sheet.md:2335`
- Hard Keywords (cannot be shadowed): `docs/cheat-sheet.md:2337`
- Soft Keywords (context-dependent): `docs/cheat-sheet.md:2343`
- Built-ins (shadowable operations/types): `docs/cheat-sheet.md:2346`

### `docs/diagnostics.md`

- Goblin Diagnostics — Style Guide (v1): `docs/diagnostics.md:1`
- 1) Anatomy of a Diagnostic: `docs/diagnostics.md:7`
- 2) Tone & Wording: `docs/diagnostics.md:23`
- 3) Spans & Positions: `docs/diagnostics.md:43`
- 4) Required Output Shape (first lines): `docs/diagnostics.md:58`
- 5) Category Slugs: `docs/diagnostics.md:83`
- 6) Multi-Diagnostic Policies: `docs/diagnostics.md:101`
- 7) Examples: `docs/diagnostics.md:109`
- Unterminated string: `docs/diagnostics.md:111`
- Bad escape: `docs/diagnostics.md:119`
- Unexpected dedent: `docs/diagnostics.md:128`
- 8) Do & Don’t (quick checklist): `docs/diagnostics.md:139`

### `docs/formal-semantics.md`

- Goblin Money System — Formal Semantics (v0.3.2, Precision, Policy & Ledger Extensions): `docs/formal-semantics.md:1`
- 1. Overview: `docs/formal-semantics.md:3`
- 2. Syntax: `docs/formal-semantics.md:23`
- 2.1 Types: `docs/formal-semantics.md:25`
- 2.2 Expressions: `docs/formal-semantics.md:36`
- 3. Canonicalization: `docs/formal-semantics.md:47`
- 4. Typing Rules: `docs/formal-semantics.md:63`
- Same-Currency Arithmetic: `docs/formal-semantics.md:65`
- Scalar Multiply: `docs/formal-semantics.md:72`
- Integer Division with Remainder: `docs/formal-semantics.md:79`
- Even Split: `docs/formal-semantics.md:86`
- Escrow Split: `docs/formal-semantics.md:93`
- Drip Remainders: `docs/formal-semantics.md:100`
- Ledger Export: `docs/formal-semantics.md:109`
- 5. Evaluation (Ledger + Escrow): `docs/formal-semantics.md:114`
- 5.1 Money Construction: `docs/formal-semantics.md:116`
- 5.2 Addition/Subtraction: `docs/formal-semantics.md:123`
- 5.3 Scalar Multiplication: `docs/formal-semantics.md:130`
- 5.4 Division (//): `docs/formal-semantics.md:139`
- 5.5 Even Split: `docs/formal-semantics.md:148`
- 5.6 Escrow Split: `docs/formal-semantics.md:161`
- 5.7 Currency Conversion: `docs/formal-semantics.md:172`
- 5.8 Drip Remainders: `docs/formal-semantics.md:181`
- 6. JSON Serialization: `docs/formal-semantics.md:198`
- ledger_json() →: `docs/formal-semantics.md:200`
- Money serializes as:: `docs/formal-semantics.md:210`
- 7. Conservation Law: `docs/formal-semantics.md:215`
- 8. Error & Policy Behavior: `docs/formal-semantics.md:223`
- 9. Audit & External System Mode: `docs/formal-semantics.md:236`
- 10. Desugaring: `docs/formal-semantics.md:242`

### `docs/gears-philosophy.md`

- 19. Gears — Philosophy & Architecture: `docs/gears-philosophy.md:1`
- 19.1 Purpose: `docs/gears-philosophy.md:3`
- 19.2 Loading & Versioning: `docs/gears-philosophy.md:12`
- 19.3 Capability Resolution: `docs/gears-philosophy.md:30`
- 19.4 Contracts (First‑Class): `docs/gears-philosophy.md:52`
- 19.5 Gear Manifest & Permissions: `docs/gears-philosophy.md:71`
- 19.6 Event Bus: `docs/gears-philosophy.md:103`
- 19.7 Sandbox & Side‑Effects: `docs/gears-philosophy.md:127`
- 19.8 Determinism, Lockfile, Dry‑Run: `docs/gears-philosophy.md:138`
- 19.9 Logging & Telemetry: `docs/gears-philosophy.md:151`
- 19.10 Testing Hooks: `docs/gears-philosophy.md:163`
- 19.11 Introspection APIs: `docs/gears-philosophy.md:177`
- 19.12 Usage Patterns: `docs/gears-philosophy.md:185`
- 19.12.1 Single Export: `docs/gears-philosophy.md:187`
- 19.12.2 Multi‑Platform Chain: `docs/gears-philosophy.md:201`
- 19.12.3 Event‑Driven Pipeline: `docs/gears-philosophy.md:213`
- 19.13 Errors: `docs/gears-philosophy.md:223`
- 19.14 Project Config (excerpt): `docs/gears-philosophy.md:231`
- goblin.config.yaml: `docs/gears-philosophy.md:233`
- 19.15 Example Contract & Gear Implementation (sketch): `docs/gears-philosophy.md:247`
- Provided by shopify gear: `docs/gears-philosophy.md:253`

### `docs/goblin_ebnf_ast_codex.md`

- Goblin v1.18 — Codex‑ready Grammar Pack: `docs/goblin_ebnf_ast_codex.md:1`
- 0) Terminals (summary for grammar): `docs/goblin_ebnf_ast_codex.md:24`
- 1) EBNF — program, declarations, statements, expressions: `docs/goblin_ebnf_ast_codex.md:39`
- 1.1 Program: `docs/goblin_ebnf_ast_codex.md:43`
- 1.2 Declarations: `docs/goblin_ebnf_ast_codex.md:52`
- 1.3 Statements & blocks: `docs/goblin_ebnf_ast_codex.md:90`
- 1.4 Pattern control forms: `judge` and `morph`: `docs/goblin_ebnf_ast_codex.md:120`
- 1.5 Expressions (precedence via tiers): `docs/goblin_ebnf_ast_codex.md:136`
- 2) Operator precedence & associativity (low → high): `docs/goblin_ebnf_ast_codex.md:184`
- 3) AST node skeletons (Rust‑style): `docs/goblin_ebnf_ast_codex.md:203`
- 4) Lowering & semantic notes: `docs/goblin_ebnf_ast_codex.md:315`
- 5) Error handling & diagnostics (expectations): `docs/goblin_ebnf_ast_codex.md:326`
- 6) Ready‑to‑export bundles: `docs/goblin_ebnf_ast_codex.md:340`

### `docs/language-spec.md`

- = private variables in classes: `docs/language-spec.md:93`
- 13. Randomness: `docs/language-spec.md:1987`
- The Concept: `docs/language-spec.md:1991`
- Pick Operations — “Pick What From What”: `docs/language-spec.md:1999`
- Syntax: `docs/language-spec.md:2003`
- Duplication Rules: `docs/language-spec.md:2027`
- Practical Examples: `docs/language-spec.md:2049`
- Why Underscore Syntax?: `docs/language-spec.md:2067`
- Roll Operations — Dice Notation and Bell Curves: `docs/language-spec.md:2075`
- Syntax: `docs/language-spec.md:2079`
- Why Multiple Dice?: `docs/language-spec.md:2092`
- Roll Detail: `docs/language-spec.md:2104`
- Weighted Selection: `docs/language-spec.md:2115`
- Analysis Utilities: `docs/language-spec.md:2141`
- Deterministic Randomness: `docs/language-spec.md:2153`
- Errors: `docs/language-spec.md:2163`
- Quick Reference: `docs/language-spec.md:2172`
- 14. Collections: `docs/language-spec.md:2204`
- Arrays: `docs/language-spec.md:2213`
- Indexing and Length: `docs/language-spec.md:2223`
- Slicing: `docs/language-spec.md:2234`
- Adding and Inserting (Destructive): `docs/language-spec.md:2245`
- Non-Destructive Derivations: `docs/language-spec.md:2253`
- Mapping (Non-Destructive): `docs/language-spec.md:2265`
- Pick — Random Selection (Non-Destructive): `docs/language-spec.md:2274`
- Digit Shorthand and `unique`: `docs/language-spec.md:2298`
- Reap — Random Selection (Destructive): `docs/language-spec.md:2320`
- Replacement: `docs/language-spec.md:2341`
- `usurp` (Destructive, with history): `docs/language-spec.md:2343`
- `replace` (Destructive, simple overwrite): `docs/language-spec.md:2356`
- Maps: `docs/language-spec.md:2367`
- Keys and Values: `docs/language-spec.md:2378`
- Updates: `docs/language-spec.md:2387`
- Quick Reference: `docs/language-spec.md:2395`
- goblin.toml (project): `docs/language-spec.md:5057`
- usage: `docs/language-spec.md:5062`
- goblin.toml (snippets): `docs/language-spec.md:5072`
- env   = ["API_KEY"]                      /// explicit env whitelist: `docs/language-spec.md:5080`
- network = { domains=["api.example.com"], methods=["POST"] }: `docs/language-spec.md:5081`
- goblin glam gmark rebalance --prefix post/: `docs/language-spec.md:6103`
- glam.toml (inside the package): `docs/language-spec.md:6777`
- network = { domains=["api.shopify.com"], methods=["POST"] }: `docs/language-spec.md:6792`
- goblin.toml (project): `docs/language-spec.md:6817`
- glam.toml (canonical as of v1.5): `docs/language-spec.md:7004`
- identity: `docs/language-spec.md:7005`
- presentation: `docs/language-spec.md:7010`
- ops & contracts (ops must be exposed in code): `docs/language-spec.md:7014`
- capabilities (sandbox): `docs/language-spec.md:7022`
- network = { domains=["api.shopify.com"], methods=["POST"] }: `docs/language-spec.md:7027`

### `docs/lex-oracle.md`

- Files to add for Step 1: `docs/lex-oracle.md:1`
- `docs/lex-oracle.md`: `docs/lex-oracle.md:7`
- Lex Oracle Format v1: `docs/lex-oracle.md:10`
- Token entries (OK mode): `docs/lex-oracle.md:22`
- Error entries (ERR mode): `docs/lex-oracle.md:93`
- Examples: `docs/lex-oracle.md:113`
- `tests/lex/ok/_TEMPLATE.expect.txt`: `docs/lex-oracle.md:165`
- Copy this, then fill in token lines.: `docs/lex-oracle.md:168`
- Optional: @check=kind+value or +span: `docs/lex-oracle.md:170`
- IDENT(name): `docs/lex-oracle.md:171`
- `=`: `docs/lex-oracle.md:172`
- INT(123): `docs/lex-oracle.md:173`
- NEWLINE: `docs/lex-oracle.md:174`
- EOF: `docs/lex-oracle.md:175`
- `tests/lex/err/_TEMPLATE.expect.txt`: `docs/lex-oracle.md:180`
- Copy this, then add one error per line.: `docs/lex-oracle.md:183`
- !category @L:C-L:C : optional message substring: `docs/lex-oracle.md:185`
- !unterminated-string @2:5-2:999 : reached EOF: `docs/lex-oracle.md:186`

### `docs/lexer-notes.md`

- Lexer Notes — v1.18 (Part 1: Global Rules, Comments, Operators): `docs/lexer-notes.md:1`
- Global rules: `docs/lexer-notes.md:3`
- Comments: `docs/lexer-notes.md:15`
- Tokens: Operators & Punct (multi-char first): `docs/lexer-notes.md:20`
- Lexer Notes — v1.18 (Part 2: Keywords, Built-ins, Identifiers): `docs/lexer-notes.md:67`
- Keywords: `docs/lexer-notes.md:69`
- Hard Keywords (cannot be shadowed): `docs/lexer-notes.md:71`
- Soft Keywords (context-dependent): `docs/lexer-notes.md:97`
- Built-ins: `docs/lexer-notes.md:114`
- Text utilities: `docs/lexer-notes.md:143`
- Collection utilities: `docs/lexer-notes.md:158`
- Numeric operations: `docs/lexer-notes.md:166`
- Types: `docs/lexer-notes.md:175`
- I/O, filesystem, printing, serialization helpers: `docs/lexer-notes.md:184`
- Identifiers: `docs/lexer-notes.md:193`
- Lexer Notes — v1.18 (Part 3: Literals): `docs/lexer-notes.md:257`
- Numbers: `docs/lexer-notes.md:307`
- Add/confirm literal forms: `docs/lexer-notes.md:323`
- Add/confirm: `docs/lexer-notes.md:337`
- Confirm/extend string literal forms: `docs/lexer-notes.md:346`
- Randomness Literals: `docs/lexer-notes.md:354`
- Percent vs modulo (CIPO): `docs/lexer-notes.md:365`
- Durations (postfix): `docs/lexer-notes.md:376`
- Money literals: `docs/lexer-notes.md:396`
- Dates & Times: `docs/lexer-notes.md:409`
- Blob literals: `docs/lexer-notes.md:425`
- Classes: `docs/lexer-notes.md:432`
- Strings & interpolation: `docs/lexer-notes.md:438`
- Lexer Notes — v1.18 (Part 4: Operators, Precedence, Misc): `docs/lexer-notes.md:480`
- Math Operators (numeric): `docs/lexer-notes.md:482`
- Operator Precedence (for parser, recorded here): `docs/lexer-notes.md:497`
- Whitespace & pipelines: `docs/lexer-notes.md:578`
- Module paths (token shapes only): `docs/lexer-notes.md:585`
- Lexer Notes — v1.18 (Part 4): `docs/lexer-notes.md:596`
- Glams & Contracts (token shapes only): `docs/lexer-notes.md:598`
- `use` (glam loading & version pins): `docs/lexer-notes.md:600`
- Provider binding (`via`) and defaults (`prefer`): `docs/lexer-notes.md:607`
- Contracts (capability interfaces): `docs/lexer-notes.md:614`
- Events (provided by events GLAM, not core): `docs/lexer-notes.md:620`
- CLI verbs (out of language scope): `docs/lexer-notes.md:624`
- No operator/precedence changes: `docs/lexer-notes.md:629`
- Lambdas: `docs/lexer-notes.md:635`
- Actions: `docs/lexer-notes.md:639`
- Token Inventory (flat): `docs/lexer-notes.md:659`
- Token Inventory: `docs/lexer-notes.md:672`
- Misc: `docs/lexer-notes.md:710`

### `docs/lexer.md`

- Lexer Notes — v1.18 (Part 2: Keywords, Built-ins, Identifiers): `docs/lexer.md:1`
- Keywords: `docs/lexer.md:3`
- Hard Keywords (cannot be shadowed): `docs/lexer.md:5`
- Soft Keywords (context-dependent): `docs/lexer.md:27`
- Built-ins: `docs/lexer.md:44`
- Text utilities: `docs/lexer.md:70`
- Collection utilities: `docs/lexer.md:85`
- Numeric operations: `docs/lexer.md:93`
- Types: `docs/lexer.md:97`
- I/O, filesystem, printing, serialization helpers: `docs/lexer.md:102`
- Identifiers: `docs/lexer.md:111`
- Lexer Notes — v1.18 (Part 2: Keywords, Built-ins, Identifiers): `docs/lexer.md:135`
- Keywords: `docs/lexer.md:137`
- Hard Keywords (cannot be shadowed): `docs/lexer.md:139`
- Soft Keywords (context-dependent): `docs/lexer.md:161`
- Built-ins: `docs/lexer.md:178`
- Text utilities: `docs/lexer.md:204`
- Collection utilities: `docs/lexer.md:219`
- Numeric operations: `docs/lexer.md:227`
- Types: `docs/lexer.md:231`
- I/O, filesystem, printing, serialization helpers: `docs/lexer.md:236`
- Identifiers: `docs/lexer.md:245`
- Lexer Notes — v1.18 (Part 3: Literals): `docs/lexer.md:269`
- Numbers: `docs/lexer.md:271`
- Add/confirm literal forms: `docs/lexer.md:286`
- Add/confirm: `docs/lexer.md:300`
- Confirm/extend string literal forms: `docs/lexer.md:309`
- Randomness Literals: `docs/lexer.md:317`
- Percent vs modulo (CIPO): `docs/lexer.md:328`
- Durations (postfix): `docs/lexer.md:339`
- Money literals: `docs/lexer.md:359`
- Dates & Times: `docs/lexer.md:372`
- Blob literals: `docs/lexer.md:388`
- Strings & interpolation: `docs/lexer.md:395`
- Lexer Notes — v1.18 (Part 4: Operators, Precedence, Misc): `docs/lexer.md:437`
- Math Operators (numeric): `docs/lexer.md:439`
- Operator Precedence (for parser, recorded here): `docs/lexer.md:451`
- Whitespace & pipelines: `docs/lexer.md:493`
- Module paths (token shapes only): `docs/lexer.md:501`
- Lexer Notes — v1.18 (Part 4: Glams & Contracts): `docs/lexer.md:512`
- `use` (glam loading & version pins): `docs/lexer.md:514`
- Provider binding (`via`) and defaults (`prefer`): `docs/lexer.md:521`
- Contracts (capability interfaces): `docs/lexer.md:528`
- Events (provided by events GLAM, not core): `docs/lexer.md:534`
- CLI verbs (out of language scope): `docs/lexer.md:538`
- No operator/precedence changes: `docs/lexer.md:543`
- Token Inventory (flat): `docs/lexer.md:549`
- Misc: `docs/lexer.md:586`
