# Interpreter / VM Parity Audit

Branch: `claude/quirky-mendel-4df8rm`
VM source fingerprint: `1d13cb7178c7a740dce77e7fef9d9c7577a9a88d1e66334ddffdb1cd82b43f80` (14 files, 14584 lines).

Status policy:
- `PASS`: a genuine test executes equivalent behavior through both interpreter and VM and compares results.
- `PARTIAL`: VM implementation is architecturally traceable, but no genuine parity test exists.
- `MISSING`: no VM implementation can be traced, or the traced VM path is only an explicit unsupported stub for implemented interpreter behavior.
- `UNKNOWN`: source evidence is genuinely indeterminate.

Summary: PASS 0, PARTIAL 342, MISSING 6, UNKNOWN 0.

No genuine cross-engine parity test was found. The VM tests are VM-only smoke/unit tests; representative locations include `crates/goblin-vm/src/exec.rs:103`, `crates/goblin-vm/src/exec.rs:108`, `crates/goblin-vm/src/exec.rs:117`, `crates/goblin-vm/src/exec.rs:128`, `crates/goblin-vm/src/exec.rs:137`, `crates/goblin-vm/src/exec.rs:150`, `crates/goblin-vm/src/exec.rs:165`, `crates/goblin-vm/src/exec.rs:195`.

## Audit Entries

* name: abs
  interpreter: crates/goblin-interpreter/src/lib.rs:12509
  vm: crates/goblin-vm/src/compiler.rs:1958, crates/goblin-vm/src/value.rs:343, crates/goblin-vm/src/builtins.rs:55, crates/goblin-vm/src/compiler.rs:1959, crates/goblin-vm/src/vm.rs:1827
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: after
  interpreter: crates/goblin-interpreter/src/lib.rs:14769
  vm: crates/goblin-vm/src/compiler.rs:1991, crates/goblin-vm/src/value.rs:381, crates/goblin-vm/src/builtins.rs:483, crates/goblin-vm/src/compiler.rs:1992
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: after_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14823
  vm: crates/goblin-vm/src/compiler.rs:1993, crates/goblin-vm/src/value.rs:383, crates/goblin-vm/src/builtins.rs:503, crates/goblin-vm/src/compiler.rs:1994
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: append_file
  interpreter: crates/goblin-interpreter/src/lib.rs:13976
  vm: crates/goblin-vm/src/compiler.rs:2176, crates/goblin-vm/src/value.rs:595, crates/goblin-vm/src/builtins.rs:1035, crates/goblin-vm/src/compiler.rs:2177
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ask
  interpreter: crates/goblin-interpreter/src/lib.rs:11159
  vm: crates/goblin-vm/src/compiler.rs:2231, crates/goblin-vm/src/value.rs:650, crates/goblin-vm/src/builtins.rs:3194, crates/goblin-vm/src/compiler.rs:2232
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: avg
  interpreter: crates/goblin-interpreter/src/lib.rs:6399
  vm: crates/goblin-vm/src/compiler.rs:1961, crates/goblin-vm/src/value.rs:346, crates/goblin-vm/src/builtins.rs:130, crates/goblin-vm/src/compiler.rs:1962, crates/goblin-vm/src/vm.rs:1884
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: b
  interpreter: crates/goblin-interpreter/src/lib.rs:12413
  vm: crates/goblin-vm/src/compiler.rs:2194, crates/goblin-vm/src/value.rs:614, crates/goblin-vm/src/builtins.rs:2847, crates/goblin-vm/src/compiler.rs:2195, crates/goblin-vm/src/vm.rs:1890
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: backend
  interpreter: crates/goblin-interpreter/src/lib.rs:15304
  vm: crates/goblin-vm/src/compiler.rs:2240, crates/goblin-vm/src/value.rs:660, crates/goblin-vm/src/builtins.rs:3256, crates/goblin-vm/src/compiler.rs:2241
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: basename
  interpreter: crates/goblin-interpreter/src/lib.rs:13985
  vm: crates/goblin-vm/src/compiler.rs:2216, crates/goblin-vm/src/value.rs:634, crates/goblin-vm/src/builtins.rs:3018, crates/goblin-vm/src/compiler.rs:2217, crates/goblin-vm/src/vm.rs:1914
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: before
  interpreter: crates/goblin-interpreter/src/lib.rs:14742
  vm: crates/goblin-vm/src/compiler.rs:1990, crates/goblin-vm/src/value.rs:380, crates/goblin-vm/src/builtins.rs:473, crates/goblin-vm/src/compiler.rs:1991
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: before_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14796
  vm: crates/goblin-vm/src/compiler.rs:1992, crates/goblin-vm/src/value.rs:382, crates/goblin-vm/src/builtins.rs:493, crates/goblin-vm/src/compiler.rs:1993
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: between
  interpreter: crates/goblin-interpreter/src/lib.rs:14850
  vm: crates/goblin-vm/src/compiler.rs:2170, crates/goblin-vm/src/value.rs:589, crates/goblin-vm/src/builtins.rs:849, crates/goblin-vm/src/compiler.rs:2171
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: big
  interpreter: crates/goblin-interpreter/src/lib.rs:12413
  vm: crates/goblin-vm/src/compiler.rs:2194, crates/goblin-vm/src/value.rs:614, crates/goblin-vm/src/builtins.rs:2847, crates/goblin-vm/src/compiler.rs:2195, crates/goblin-vm/src/vm.rs:1890
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: bool
  interpreter: crates/goblin-interpreter/src/lib.rs:12429
  vm: crates/goblin-vm/src/compiler.rs:2211, crates/goblin-vm/src/value.rs:527, crates/goblin-vm/src/builtins.rs:2122, crates/goblin-vm/src/compiler.rs:2153, crates/goblin-vm/src/compiler.rs:2212, crates/goblin-vm/src/vm.rs:1803
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ceil
  interpreter: crates/goblin-interpreter/src/lib.rs:12487
  vm: crates/goblin-vm/src/compiler.rs:1964, crates/goblin-vm/src/value.rs:349, crates/goblin-vm/src/builtins.rs:153, crates/goblin-vm/src/compiler.rs:1965, crates/goblin-vm/src/vm.rs:1830
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: chars
  interpreter: crates/goblin-interpreter/src/lib.rs:14926
  vm: crates/goblin-vm/src/compiler.rs:2163, crates/goblin-vm/src/value.rs:580, crates/goblin-vm/src/builtins.rs:2654, crates/goblin-vm/src/compiler.rs:2164, crates/goblin-vm/src/vm.rs:1826
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: clamp
  interpreter: crates/goblin-interpreter/src/lib.rs:6596
  vm: crates/goblin-vm/src/compiler.rs:1967, crates/goblin-vm/src/value.rs:352, crates/goblin-vm/src/builtins.rs:177, crates/goblin-vm/src/compiler.rs:1968
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: clear_all_tokens
  interpreter: crates/goblin-interpreter/src/lib.rs:6083
  vm: crates/goblin-vm/src/compiler.rs:2251, crates/goblin-vm/src/value.rs:671, crates/goblin-vm/src/builtins.rs:3381, crates/goblin-vm/src/compiler.rs:2252
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: clear_format
  interpreter: crates/goblin-interpreter/src/lib.rs:12349
  vm: crates/goblin-vm/src/compiler.rs:2239, crates/goblin-vm/src/value.rs:659, crates/goblin-vm/src/builtins.rs:3233, crates/goblin-vm/src/compiler.rs:2240
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: clear_token
  interpreter: crates/goblin-interpreter/src/lib.rs:6047
  vm: crates/goblin-vm/src/compiler.rs:2249, crates/goblin-vm/src/value.rs:669, crates/goblin-vm/src/builtins.rs:3364, crates/goblin-vm/src/compiler.rs:2250
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: clear_tokens
  interpreter: crates/goblin-interpreter/src/lib.rs:6059
  vm: crates/goblin-vm/src/compiler.rs:2250, crates/goblin-vm/src/value.rs:670, crates/goblin-vm/src/builtins.rs:3373, crates/goblin-vm/src/compiler.rs:2251
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: clone_object
  interpreter: crates/goblin-interpreter/src/lib.rs:13923
  vm: crates/goblin-vm/src/compiler.rs:2270, crates/goblin-vm/src/value.rs:681, crates/goblin-vm/src/builtins.rs:3419, crates/goblin-vm/src/compiler.rs:2271
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: cookie
  interpreter: crates/goblin-interpreter/src/lib.rs:13965
  vm: crates/goblin-vm/src/compiler.rs:2035, crates/goblin-vm/src/value.rs:555, crates/goblin-vm/src/builtins.rs:2396, crates/goblin-vm/src/compiler.rs:2036
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: copy_file
  interpreter: crates/goblin-interpreter/src/lib.rs:13980
  vm: crates/goblin-vm/src/compiler.rs:2190, crates/goblin-vm/src/value.rs:610, crates/goblin-vm/src/builtins.rs:2806, crates/goblin-vm/src/compiler.rs:2191
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: count
  interpreter: crates/goblin-interpreter/src/lib.rs:13785
  vm: crates/goblin-vm/src/compiler.rs:1969, crates/goblin-vm/src/value.rs:370, crates/goblin-vm/src/builtins.rs:346, crates/goblin-vm/src/compiler.rs:1970
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: count_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:15104
  vm: crates/goblin-vm/src/compiler.rs:2004, crates/goblin-vm/src/value.rs:394, crates/goblin-vm/src/builtins.rs:620, crates/goblin-vm/src/compiler.rs:2005
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: create_dir
  interpreter: crates/goblin-interpreter/src/lib.rs:13974
  vm: crates/goblin-vm/src/compiler.rs:2189, crates/goblin-vm/src/value.rs:609, crates/goblin-vm/src/builtins.rs:2799, crates/goblin-vm/src/compiler.rs:2190
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: decision_debug
  interpreter: crates/goblin-interpreter/src/lib.rs:6725
  vm: crates/goblin-vm/src/compiler.rs:2264, crates/goblin-vm/src/value.rs:675, crates/goblin-vm/src/builtins.rs:3447, crates/goblin-vm/src/compiler.rs:2265
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete
  interpreter: crates/goblin-interpreter/src/lib.rs:14135
  vm: crates/goblin-vm/src/compiler.rs:2062, crates/goblin-vm/src/value.rs:444, crates/goblin-vm/src/builtins.rs:1499, crates/goblin-vm/src/compiler.rs:2063
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_all
  interpreter: crates/goblin-interpreter/src/lib.rs:14161
  vm: crates/goblin-vm/src/compiler.rs:2067, crates/goblin-vm/src/value.rs:449, crates/goblin-vm/src/builtins.rs:1554, crates/goblin-vm/src/compiler.rs:2068, crates/goblin-vm/src/vm.rs:1879
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_at
  interpreter: crates/goblin-interpreter/src/lib.rs:14150
  vm: crates/goblin-vm/src/compiler.rs:2065, crates/goblin-vm/src/value.rs:447, crates/goblin-vm/src/builtins.rs:1512, crates/goblin-vm/src/compiler.rs:2066
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_between
  interpreter: crates/goblin-interpreter/src/lib.rs:14172
  vm: crates/goblin-vm/src/compiler.rs:2095, crates/goblin-vm/src/value.rs:468, crates/goblin-vm/src/builtins.rs:1791, crates/goblin-vm/src/compiler.rs:2096
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_first
  interpreter: crates/goblin-interpreter/src/lib.rs:14140
  vm: crates/goblin-vm/src/compiler.rs:2063, crates/goblin-vm/src/value.rs:445, crates/goblin-vm/src/builtins.rs:1504, crates/goblin-vm/src/compiler.rs:2064, crates/goblin-vm/src/vm.rs:1876
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14145
  vm: crates/goblin-vm/src/compiler.rs:2064, crates/goblin-vm/src/value.rs:446, crates/goblin-vm/src/builtins.rs:1508, crates/goblin-vm/src/compiler.rs:2065, crates/goblin-vm/src/vm.rs:1877
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:14166
  vm: crates/goblin-vm/src/compiler.rs:2094, crates/goblin-vm/src/value.rs:468, crates/goblin-vm/src/builtins.rs:1785, crates/goblin-vm/src/compiler.rs:2095
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_path
  interpreter: crates/goblin-interpreter/src/lib.rs:13977
  vm: crates/goblin-vm/src/compiler.rs:2191, crates/goblin-vm/src/value.rs:611, crates/goblin-vm/src/builtins.rs:2819, crates/goblin-vm/src/compiler.rs:2192
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_random
  interpreter: crates/goblin-interpreter/src/lib.rs:14179
  vm: crates/goblin-vm/src/compiler.rs:2096, crates/goblin-vm/src/value.rs:468, crates/goblin-vm/src/builtins.rs:1798, crates/goblin-vm/src/compiler.rs:2097, crates/goblin-vm/src/vm.rs:1878
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: delete_where
  interpreter: crates/goblin-interpreter/src/lib.rs:14155
  vm: crates/goblin-vm/src/compiler.rs:2066, crates/goblin-vm/src/value.rs:448, crates/goblin-vm/src/builtins.rs:1518, crates/goblin-vm/src/compiler.rs:2067
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: dirname
  interpreter: crates/goblin-interpreter/src/lib.rs:13983
  vm: crates/goblin-vm/src/compiler.rs:2217, crates/goblin-vm/src/value.rs:635, crates/goblin-vm/src/builtins.rs:3024, crates/goblin-vm/src/compiler.rs:2218, crates/goblin-vm/src/vm.rs:1915
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: dups
  interpreter: crates/goblin-interpreter/src/lib.rs:13793
  vm: crates/goblin-vm/src/compiler.rs:2017, crates/goblin-vm/src/value.rs:418, crates/goblin-vm/src/builtins.rs:1380, crates/goblin-vm/src/compiler.rs:2018, crates/goblin-vm/src/vm.rs:1818
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ends_with
  interpreter: crates/goblin-interpreter/src/lib.rs:16259
  vm: crates/goblin-vm/src/compiler.rs:1988, crates/goblin-vm/src/value.rs:378, crates/goblin-vm/src/builtins.rs:454, crates/goblin-vm/src/compiler.rs:1989
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: env
  interpreter: crates/goblin-interpreter/src/lib.rs:13726
  vm: crates/goblin-vm/src/compiler.rs:2011, crates/goblin-vm/src/value.rs:401, crates/goblin-vm/src/builtins.rs:820, crates/goblin-vm/src/compiler.rs:2012
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: escape_html
  interpreter: crates/goblin-interpreter/src/lib.rs:13994
  vm: crates/goblin-vm/src/compiler.rs:2226, crates/goblin-vm/src/value.rs:644, crates/goblin-vm/src/builtins.rs:3130, crates/goblin-vm/src/compiler.rs:2227, crates/goblin-vm/src/vm.rs:1923
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ext
  interpreter: crates/goblin-interpreter/src/lib.rs:13982
  vm: crates/goblin-vm/src/compiler.rs:2219, crates/goblin-vm/src/value.rs:637, crates/goblin-vm/src/builtins.rs:3036, crates/goblin-vm/src/compiler.rs:2220, crates/goblin-vm/src/vm.rs:1917
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: f
  interpreter: crates/goblin-interpreter/src/lib.rs:12409
  vm: crates/goblin-vm/src/compiler.rs:2210, crates/goblin-vm/src/value.rs:525, crates/goblin-vm/src/builtins.rs:2106, crates/goblin-vm/src/compiler.rs:2151, crates/goblin-vm/src/compiler.rs:2211, crates/goblin-vm/src/vm.rs:1802
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: f32
  interpreter: crates/goblin-interpreter/src/lib.rs:12441
  vm: crates/goblin-vm/src/compiler.rs:2206, crates/goblin-vm/src/value.rs:627, crates/goblin-vm/src/builtins.rs:2961, crates/goblin-vm/src/compiler.rs:2207, crates/goblin-vm/src/vm.rs:1911
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: f64
  interpreter: crates/goblin-interpreter/src/lib.rs:12442
  vm: crates/goblin-vm/src/compiler.rs:2207, crates/goblin-vm/src/value.rs:628, crates/goblin-vm/src/builtins.rs:2983, crates/goblin-vm/src/compiler.rs:2208, crates/goblin-vm/src/vm.rs:1912
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: file_exists
  interpreter: crates/goblin-interpreter/src/lib.rs:13973
  vm: crates/goblin-vm/src/compiler.rs:2213, crates/goblin-vm/src/value.rs:631, crates/goblin-vm/src/builtins.rs:3003, crates/goblin-vm/src/compiler.rs:2214, crates/goblin-vm/src/vm.rs:1920
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: find
  interpreter: crates/goblin-interpreter/src/lib.rs:13774
  vm: crates/goblin-vm/src/compiler.rs:2115, crates/goblin-vm/src/value.rs:365, crates/goblin-vm/src/builtins.rs:289, crates/goblin-vm/src/compiler.rs:2116
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: find_all
  interpreter: crates/goblin-interpreter/src/lib.rs:13775
  vm: crates/goblin-vm/src/compiler.rs:1982, crates/goblin-vm/src/value.rs:366, crates/goblin-vm/src/builtins.rs:309, crates/goblin-vm/src/compiler.rs:1983, crates/goblin-vm/src/vm.rs:1893
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: float
  interpreter: crates/goblin-interpreter/src/lib.rs:12409
  vm: crates/goblin-vm/src/compiler.rs:2210, crates/goblin-vm/src/value.rs:525, crates/goblin-vm/src/builtins.rs:2106, crates/goblin-vm/src/compiler.rs:2151, crates/goblin-vm/src/compiler.rs:2211, crates/goblin-vm/src/vm.rs:1802
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: floor
  interpreter: crates/goblin-interpreter/src/lib.rs:12465
  vm: crates/goblin-vm/src/compiler.rs:1963, crates/goblin-vm/src/value.rs:348, crates/goblin-vm/src/builtins.rs:145, crates/goblin-vm/src/compiler.rs:1241, crates/goblin-vm/src/compiler.rs:1964, crates/goblin-vm/src/vm.rs:1829
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: format
  interpreter: crates/goblin-interpreter/src/lib.rs:12150
  vm: crates/goblin-vm/src/compiler.rs:2164, crates/goblin-vm/src/value.rs:581, crates/goblin-vm/src/builtins.rs:2662, crates/goblin-vm/src/compiler.rs:2165
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: format_info
  interpreter: crates/goblin-interpreter/src/lib.rs:12369
  vm: crates/goblin-vm/src/compiler.rs:2238, crates/goblin-vm/src/value.rs:658, crates/goblin-vm/src/builtins.rs:3240, crates/goblin-vm/src/compiler.rs:2239
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: freq
  interpreter: crates/goblin-interpreter/src/lib.rs:13788
  vm: crates/goblin-vm/src/compiler.rs:2014, crates/goblin-vm/src/value.rs:413, crates/goblin-vm/src/builtins.rs:1237, crates/goblin-vm/src/compiler.rs:2015, crates/goblin-vm/src/vm.rs:1888
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get
  interpreter: crates/goblin-interpreter/src/lib.rs:13999
  vm: untraced
  status: MISSING
  note: no compiler::builtin_by_name mapping
* name: get_all
  interpreter: crates/goblin-interpreter/src/lib.rs:14025
  vm: crates/goblin-vm/src/compiler.rs:2080, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1685, crates/goblin-vm/src/compiler.rs:2081, crates/goblin-vm/src/vm.rs:1874
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_at
  interpreter: crates/goblin-interpreter/src/lib.rs:14014
  vm: crates/goblin-vm/src/compiler.rs:2078, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1670, crates/goblin-vm/src/compiler.rs:2079
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_between
  interpreter: crates/goblin-interpreter/src/lib.rs:14036
  vm: crates/goblin-vm/src/compiler.rs:2082, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1699, crates/goblin-vm/src/compiler.rs:2083
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_first
  interpreter: crates/goblin-interpreter/src/lib.rs:14004
  vm: crates/goblin-vm/src/compiler.rs:2076, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1660, crates/goblin-vm/src/compiler.rs:2077, crates/goblin-vm/src/vm.rs:1872
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14009
  vm: crates/goblin-vm/src/compiler.rs:2077, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1665, crates/goblin-vm/src/compiler.rs:2078, crates/goblin-vm/src/vm.rs:1873
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:14030
  vm: crates/goblin-vm/src/compiler.rs:2081, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1690, crates/goblin-vm/src/compiler.rs:2082
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_random
  interpreter: crates/goblin-interpreter/src/lib.rs:14043
  vm: crates/goblin-vm/src/compiler.rs:2083, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1706, crates/goblin-vm/src/compiler.rs:2084, crates/goblin-vm/src/vm.rs:1875
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: get_where
  interpreter: crates/goblin-interpreter/src/lib.rs:14019
  vm: crates/goblin-vm/src/compiler.rs:2079, crates/goblin-vm/src/value.rs:462, crates/goblin-vm/src/builtins.rs:1676, crates/goblin-vm/src/compiler.rs:2080
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid
  interpreter: crates/goblin-interpreter/src/lib.rs:13811
  vm: crates/goblin-vm/src/compiler.rs:2279, crates/goblin-vm/src/value.rs:701, crates/goblin-vm/src/builtins.rs:3478, crates/goblin-vm/src/compiler.rs:2280
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_count
  interpreter: crates/goblin-interpreter/src/lib.rs:13822
  vm: crates/goblin-vm/src/compiler.rs:2294, crates/goblin-vm/src/value.rs:716, crates/goblin-vm/src/builtins.rs:3685, crates/goblin-vm/src/compiler.rs:2295
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_default_get
  interpreter: crates/goblin-interpreter/src/lib.rs:13829
  vm: crates/goblin-vm/src/compiler.rs:2287, crates/goblin-vm/src/value.rs:709, crates/goblin-vm/src/builtins.rs:3618, crates/goblin-vm/src/compiler.rs:2288
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_default_set
  interpreter: crates/goblin-interpreter/src/lib.rs:13830
  vm: crates/goblin-vm/src/compiler.rs:2288, crates/goblin-vm/src/value.rs:710, crates/goblin-vm/src/builtins.rs:3630, crates/goblin-vm/src/compiler.rs:2289
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_get
  interpreter: crates/goblin-interpreter/src/lib.rs:13814
  vm: crates/goblin-vm/src/compiler.rs:2280, crates/goblin-vm/src/value.rs:702, crates/goblin-vm/src/builtins.rs:3520, crates/goblin-vm/src/compiler.rs:2281
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_has
  interpreter: crates/goblin-interpreter/src/lib.rs:13824
  vm: crates/goblin-vm/src/compiler.rs:2296, crates/goblin-vm/src/value.rs:718, crates/goblin-vm/src/builtins.rs:3706, crates/goblin-vm/src/compiler.rs:2297
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_info
  interpreter: crates/goblin-interpreter/src/lib.rs:13831
  vm: crates/goblin-vm/src/compiler.rs:2297, crates/goblin-vm/src/value.rs:719, crates/goblin-vm/src/builtins.rs:3715, crates/goblin-vm/src/compiler.rs:2298
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_neighbors
  interpreter: crates/goblin-interpreter/src/lib.rs:13817
  vm: crates/goblin-vm/src/compiler.rs:2289, crates/goblin-vm/src/value.rs:711, crates/goblin-vm/src/builtins.rs:3641, crates/goblin-vm/src/compiler.rs:2290
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_occupied
  interpreter: crates/goblin-interpreter/src/lib.rs:13818
  vm: crates/goblin-vm/src/compiler.rs:2290, crates/goblin-vm/src/value.rs:712, crates/goblin-vm/src/builtins.rs:3655, crates/goblin-vm/src/compiler.rs:2291
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_occupied_by
  interpreter: crates/goblin-interpreter/src/lib.rs:13823
  vm: crates/goblin-vm/src/compiler.rs:2295, crates/goblin-vm/src/value.rs:717, crates/goblin-vm/src/builtins.rs:3694, crates/goblin-vm/src/compiler.rs:2296
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_occupied_count
  interpreter: crates/goblin-interpreter/src/lib.rs:13820
  vm: crates/goblin-vm/src/compiler.rs:2292, crates/goblin-vm/src/value.rs:714, crates/goblin-vm/src/builtins.rs:3673, crates/goblin-vm/src/compiler.rs:2293
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_region_get
  interpreter: crates/goblin-interpreter/src/lib.rs:13827
  vm: crates/goblin-vm/src/compiler.rs:2285, crates/goblin-vm/src/value.rs:707, crates/goblin-vm/src/builtins.rs:3591, crates/goblin-vm/src/compiler.rs:2286
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_region_info
  interpreter: crates/goblin-interpreter/src/lib.rs:13833
  vm: crates/goblin-vm/src/compiler.rs:2299, crates/goblin-vm/src/value.rs:721, crates/goblin-vm/src/builtins.rs:3768, crates/goblin-vm/src/compiler.rs:2300
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_region_set
  interpreter: crates/goblin-interpreter/src/lib.rs:13828
  vm: crates/goblin-vm/src/compiler.rs:2286, crates/goblin-vm/src/value.rs:708, crates/goblin-vm/src/builtins.rs:3605, crates/goblin-vm/src/compiler.rs:2287
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_set
  interpreter: crates/goblin-interpreter/src/lib.rs:13815
  vm: crates/goblin-vm/src/compiler.rs:2281, crates/goblin-vm/src/value.rs:703, crates/goblin-vm/src/builtins.rs:3536, crates/goblin-vm/src/compiler.rs:2282
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_tile_get
  interpreter: crates/goblin-interpreter/src/lib.rs:13825
  vm: crates/goblin-vm/src/compiler.rs:2283, crates/goblin-vm/src/value.rs:705, crates/goblin-vm/src/builtins.rs:3564, crates/goblin-vm/src/compiler.rs:2284
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_tile_info
  interpreter: crates/goblin-interpreter/src/lib.rs:13832
  vm: crates/goblin-vm/src/compiler.rs:2298, crates/goblin-vm/src/value.rs:720, crates/goblin-vm/src/builtins.rs:3747, crates/goblin-vm/src/compiler.rs:2299
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_tile_set
  interpreter: crates/goblin-interpreter/src/lib.rs:13826
  vm: crates/goblin-vm/src/compiler.rs:2284, crates/goblin-vm/src/value.rs:706, crates/goblin-vm/src/builtins.rs:3578, crates/goblin-vm/src/compiler.rs:2285
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_unoccupied
  interpreter: crates/goblin-interpreter/src/lib.rs:13819
  vm: crates/goblin-vm/src/compiler.rs:2291, crates/goblin-vm/src/value.rs:713, crates/goblin-vm/src/builtins.rs:3664, crates/goblin-vm/src/compiler.rs:2292
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_unoccupied_count
  interpreter: crates/goblin-interpreter/src/lib.rs:13821
  vm: crates/goblin-vm/src/compiler.rs:2293, crates/goblin-vm/src/value.rs:715, crates/goblin-vm/src/builtins.rs:3679, crates/goblin-vm/src/compiler.rs:2294
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: grid_void
  interpreter: crates/goblin-interpreter/src/lib.rs:13816
  vm: crates/goblin-vm/src/compiler.rs:2282, crates/goblin-vm/src/value.rs:704, crates/goblin-vm/src/builtins.rs:3552, crates/goblin-vm/src/compiler.rs:2283
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: has
  interpreter: crates/goblin-interpreter/src/lib.rs:13784
  vm: crates/goblin-vm/src/compiler.rs:2099, crates/goblin-vm/src/value.rs:409, crates/goblin-vm/src/builtins.rs:1096, crates/goblin-vm/src/compiler.rs:2100
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: highlight_code
  interpreter: crates/goblin-interpreter/src/lib.rs:11233
  vm: crates/goblin-vm/src/compiler.rs:2193, crates/goblin-vm/src/value.rs:613, crates/goblin-vm/src/builtins.rs:2838, crates/goblin-vm/src/compiler.rs:2194
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: i
  interpreter: crates/goblin-interpreter/src/lib.rs:12405
  vm: crates/goblin-vm/src/compiler.rs:2209, crates/goblin-vm/src/value.rs:524, crates/goblin-vm/src/builtins.rs:2087, crates/goblin-vm/src/compiler.rs:2150, crates/goblin-vm/src/compiler.rs:2210, crates/goblin-vm/src/vm.rs:1801
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: i16
  interpreter: crates/goblin-interpreter/src/lib.rs:12434
  vm: crates/goblin-vm/src/compiler.rs:2199, crates/goblin-vm/src/value.rs:620, crates/goblin-vm/src/builtins.rs:2909, crates/goblin-vm/src/compiler.rs:2200, crates/goblin-vm/src/vm.rs:1904
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: i32
  interpreter: crates/goblin-interpreter/src/lib.rs:12435
  vm: crates/goblin-vm/src/compiler.rs:2200, crates/goblin-vm/src/value.rs:621, crates/goblin-vm/src/builtins.rs:2917, crates/goblin-vm/src/compiler.rs:2201, crates/goblin-vm/src/vm.rs:1905
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: i64
  interpreter: crates/goblin-interpreter/src/lib.rs:12436
  vm: crates/goblin-vm/src/compiler.rs:2201, crates/goblin-vm/src/value.rs:622, crates/goblin-vm/src/builtins.rs:2925, crates/goblin-vm/src/compiler.rs:2202, crates/goblin-vm/src/vm.rs:1906
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: i8
  interpreter: crates/goblin-interpreter/src/lib.rs:12433
  vm: crates/goblin-vm/src/compiler.rs:2198, crates/goblin-vm/src/value.rs:619, crates/goblin-vm/src/builtins.rs:2901, crates/goblin-vm/src/compiler.rs:2199, crates/goblin-vm/src/vm.rs:1903
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_between
  interpreter: crates/goblin-interpreter/src/lib.rs:15576
  vm: crates/goblin-vm/src/compiler.rs:2009, crates/goblin-vm/src/value.rs:399, crates/goblin-vm/src/builtins.rs:678, crates/goblin-vm/src/compiler.rs:2010
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_blocks
  interpreter: crates/goblin-interpreter/src/lib.rs:15693
  vm: crates/goblin-vm/src/compiler.rs:2010, crates/goblin-vm/src/value.rs:400, crates/goblin-vm/src/builtins.rs:743, crates/goblin-vm/src/compiler.rs:2011
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_blocks_first
  interpreter: crates/goblin-interpreter/src/lib.rs:15829
  vm: crates/goblin-vm/src/compiler.rs:2172, crates/goblin-vm/src/value.rs:591, crates/goblin-vm/src/builtins.rs:869, crates/goblin-vm/src/compiler.rs:2173
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_lines_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:15505
  vm: crates/goblin-vm/src/compiler.rs:2002, crates/goblin-vm/src/value.rs:392, crates/goblin-vm/src/builtins.rs:635, crates/goblin-vm/src/compiler.rs:2003
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_lines_where
  interpreter: crates/goblin-interpreter/src/lib.rs:15421
  vm: crates/goblin-vm/src/compiler.rs:2000, crates/goblin-vm/src/value.rs:390, crates/goblin-vm/src/builtins.rs:597, crates/goblin-vm/src/compiler.rs:2001
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:15438
  vm: crates/goblin-vm/src/compiler.rs:2001, crates/goblin-vm/src/value.rs:391, crates/goblin-vm/src/builtins.rs:627, crates/goblin-vm/src/compiler.rs:2002
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ignore_where
  interpreter: crates/goblin-interpreter/src/lib.rs:15413
  vm: crates/goblin-vm/src/compiler.rs:1999, crates/goblin-vm/src/value.rs:389, crates/goblin-vm/src/builtins.rs:588, crates/goblin-vm/src/compiler.rs:2000
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: input
  interpreter: crates/goblin-interpreter/src/lib.rs:11159
  vm: crates/goblin-vm/src/compiler.rs:2231, crates/goblin-vm/src/value.rs:650, crates/goblin-vm/src/builtins.rs:3194, crates/goblin-vm/src/compiler.rs:2232
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: int
  interpreter: crates/goblin-interpreter/src/lib.rs:12405
  vm: crates/goblin-vm/src/compiler.rs:2209, crates/goblin-vm/src/value.rs:524, crates/goblin-vm/src/builtins.rs:2087, crates/goblin-vm/src/compiler.rs:2150, crates/goblin-vm/src/compiler.rs:2210, crates/goblin-vm/src/vm.rs:1801
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: invoke
  interpreter: crates/goblin-interpreter/src/lib.rs:11242
  vm: crates/goblin-vm/src/compiler.rs:2180, crates/goblin-vm/src/value.rs:600, crates/goblin-vm/src/builtins.rs:2752, crates/goblin-vm/src/compiler.rs:2181, crates/goblin-vm/src/vm.rs:718
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_alnum
  interpreter: crates/goblin-interpreter/src/lib.rs:11985
  vm: crates/goblin-vm/src/compiler.rs:2139, crates/goblin-vm/src/value.rs:512, crates/goblin-vm/src/builtins.rs:1995, crates/goblin-vm/src/compiler.rs:2140, crates/goblin-vm/src/vm.rs:1841
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_alpha
  interpreter: crates/goblin-interpreter/src/lib.rs:11792
  vm: crates/goblin-vm/src/compiler.rs:2140, crates/goblin-vm/src/value.rs:513, crates/goblin-vm/src/builtins.rs:2003, crates/goblin-vm/src/compiler.rs:2141, crates/goblin-vm/src/vm.rs:1842
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_array
  interpreter: crates/goblin-interpreter/src/lib.rs:11719
  vm: crates/goblin-vm/src/compiler.rs:2019, crates/goblin-vm/src/value.rs:501, crates/goblin-vm/src/builtins.rs:1984, crates/goblin-vm/src/compiler.rs:1525, crates/goblin-vm/src/compiler.rs:2020, crates/goblin-vm/src/vm.rs:1869
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_big
  interpreter: crates/goblin-interpreter/src/lib.rs:11670
  vm: crates/goblin-vm/src/compiler.rs:2132, crates/goblin-vm/src/value.rs:505, crates/goblin-vm/src/builtins.rs:1988, crates/goblin-vm/src/compiler.rs:2133, crates/goblin-vm/src/vm.rs:1834
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_bool
  interpreter: crates/goblin-interpreter/src/lib.rs:11634
  vm: crates/goblin-vm/src/compiler.rs:2126, crates/goblin-vm/src/value.rs:497, crates/goblin-vm/src/builtins.rs:1980, crates/goblin-vm/src/compiler.rs:2127, crates/goblin-vm/src/vm.rs:1865
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_bound_name
  interpreter: crates/goblin-interpreter/src/lib.rs:11058
  vm: crates/goblin-vm/src/compiler.rs:2179, crates/goblin-vm/src/value.rs:599, crates/goblin-vm/src/builtins.rs:2746, crates/goblin-vm/src/compiler.rs:2180
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_char
  interpreter: crates/goblin-interpreter/src/lib.rs:11710
  vm: crates/goblin-vm/src/compiler.rs:2135, crates/goblin-vm/src/value.rs:508, crates/goblin-vm/src/builtins.rs:1991, crates/goblin-vm/src/compiler.rs:2136, crates/goblin-vm/src/vm.rs:1837
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_control
  interpreter: crates/goblin-interpreter/src/lib.rs:11764
  vm: crates/goblin-vm/src/compiler.rs:2171, crates/goblin-vm/src/value.rs:590, crates/goblin-vm/src/builtins.rs:863, crates/goblin-vm/src/compiler.rs:2172, crates/goblin-vm/src/vm.rs:1898
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_digit
  interpreter: crates/goblin-interpreter/src/lib.rs:11777
  vm: crates/goblin-vm/src/compiler.rs:2141, crates/goblin-vm/src/value.rs:514, crates/goblin-vm/src/builtins.rs:2012, crates/goblin-vm/src/compiler.rs:2142, crates/goblin-vm/src/vm.rs:1843
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_dir
  interpreter: crates/goblin-interpreter/src/lib.rs:13988
  vm: crates/goblin-vm/src/compiler.rs:2215, crates/goblin-vm/src/value.rs:633, crates/goblin-vm/src/builtins.rs:3013, crates/goblin-vm/src/compiler.rs:2216, crates/goblin-vm/src/vm.rs:1922
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_even
  interpreter: crates/goblin-interpreter/src/lib.rs:11822
  vm: crates/goblin-vm/src/compiler.rs:2143, crates/goblin-vm/src/value.rs:516, crates/goblin-vm/src/builtins.rs:2028, crates/goblin-vm/src/compiler.rs:2144, crates/goblin-vm/src/vm.rs:1845
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_file
  interpreter: crates/goblin-interpreter/src/lib.rs:13987
  vm: crates/goblin-vm/src/compiler.rs:2214, crates/goblin-vm/src/value.rs:632, crates/goblin-vm/src/builtins.rs:3008, crates/goblin-vm/src/compiler.rs:2215, crates/goblin-vm/src/vm.rs:1921
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_float
  interpreter: crates/goblin-interpreter/src/lib.rs:11656
  vm: crates/goblin-vm/src/compiler.rs:2128, crates/goblin-vm/src/value.rs:499, crates/goblin-vm/src/builtins.rs:1982, crates/goblin-vm/src/compiler.rs:2129, crates/goblin-vm/src/vm.rs:1867
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_int
  interpreter: crates/goblin-interpreter/src/lib.rs:11643
  vm: crates/goblin-vm/src/compiler.rs:2127, crates/goblin-vm/src/value.rs:498, crates/goblin-vm/src/builtins.rs:1981, crates/goblin-vm/src/compiler.rs:2128, crates/goblin-vm/src/vm.rs:1866
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_map
  interpreter: crates/goblin-interpreter/src/lib.rs:11728
  vm: crates/goblin-vm/src/compiler.rs:2020, crates/goblin-vm/src/value.rs:502, crates/goblin-vm/src/builtins.rs:1985, crates/goblin-vm/src/compiler.rs:2021, crates/goblin-vm/src/vm.rs:1870
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:15069
  vm: crates/goblin-vm/src/compiler.rs:2003, crates/goblin-vm/src/value.rs:393, crates/goblin-vm/src/builtins.rs:613, crates/goblin-vm/src/compiler.rs:2004
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_multiple_of
  interpreter: crates/goblin-interpreter/src/lib.rs:11886
  vm: crates/goblin-vm/src/compiler.rs:2145, crates/goblin-vm/src/value.rs:518, crates/goblin-vm/src/builtins.rs:2044, crates/goblin-vm/src/compiler.rs:2146
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_negative
  interpreter: crates/goblin-interpreter/src/lib.rs:11967
  vm: crates/goblin-vm/src/compiler.rs:2147, crates/goblin-vm/src/value.rs:520, crates/goblin-vm/src/builtins.rs:2065, crates/goblin-vm/src/compiler.rs:2148, crates/goblin-vm/src/vm.rs:1848
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_nil
  interpreter: crates/goblin-interpreter/src/lib.rs:11625
  vm: crates/goblin-vm/src/compiler.rs:2125, crates/goblin-vm/src/value.rs:496, crates/goblin-vm/src/builtins.rs:1979, crates/goblin-vm/src/compiler.rs:2126, crates/goblin-vm/src/vm.rs:1864
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_nix
  interpreter: crates/goblin-interpreter/src/lib.rs:12014
  vm: crates/goblin-vm/src/compiler.rs:2148, crates/goblin-vm/src/value.rs:521, crates/goblin-vm/src/builtins.rs:2074, crates/goblin-vm/src/compiler.rs:2149, crates/goblin-vm/src/vm.rs:1849
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_num
  interpreter: crates/goblin-interpreter/src/lib.rs:11688
  vm: crates/goblin-vm/src/compiler.rs:2134, crates/goblin-vm/src/value.rs:507, crates/goblin-vm/src/builtins.rs:1990, crates/goblin-vm/src/compiler.rs:2135, crates/goblin-vm/src/vm.rs:1836
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_odd
  interpreter: crates/goblin-interpreter/src/lib.rs:11855
  vm: crates/goblin-vm/src/compiler.rs:2144, crates/goblin-vm/src/value.rs:517, crates/goblin-vm/src/builtins.rs:2036, crates/goblin-vm/src/compiler.rs:2145, crates/goblin-vm/src/vm.rs:1846
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_pair
  interpreter: crates/goblin-interpreter/src/lib.rs:11737
  vm: crates/goblin-vm/src/compiler.rs:2136, crates/goblin-vm/src/value.rs:509, crates/goblin-vm/src/builtins.rs:1992, crates/goblin-vm/src/compiler.rs:2137, crates/goblin-vm/src/vm.rs:1838
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_pct
  interpreter: crates/goblin-interpreter/src/lib.rs:11679
  vm: crates/goblin-vm/src/compiler.rs:2133, crates/goblin-vm/src/value.rs:506, crates/goblin-vm/src/builtins.rs:1989, crates/goblin-vm/src/compiler.rs:2134, crates/goblin-vm/src/vm.rs:1835
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_positive
  interpreter: crates/goblin-interpreter/src/lib.rs:11949
  vm: crates/goblin-vm/src/compiler.rs:2146, crates/goblin-vm/src/value.rs:519, crates/goblin-vm/src/builtins.rs:2056, crates/goblin-vm/src/compiler.rs:2147, crates/goblin-vm/src/vm.rs:1847
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_seq
  interpreter: crates/goblin-interpreter/src/lib.rs:11746
  vm: crates/goblin-vm/src/compiler.rs:2137, crates/goblin-vm/src/value.rs:510, crates/goblin-vm/src/builtins.rs:1993, crates/goblin-vm/src/compiler.rs:2138, crates/goblin-vm/src/vm.rs:1839
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_str
  interpreter: crates/goblin-interpreter/src/lib.rs:11701
  vm: crates/goblin-vm/src/compiler.rs:2129, crates/goblin-vm/src/value.rs:500, crates/goblin-vm/src/builtins.rs:1983, crates/goblin-vm/src/compiler.rs:2130, crates/goblin-vm/src/vm.rs:1868
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_type
  interpreter: crates/goblin-interpreter/src/lib.rs:11096
  vm: crates/goblin-vm/src/compiler.rs:2178, crates/goblin-vm/src/value.rs:598, crates/goblin-vm/src/builtins.rs:2727, crates/goblin-vm/src/compiler.rs:2179
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_unit
  interpreter: crates/goblin-interpreter/src/lib.rs:11755
  vm: crates/goblin-vm/src/compiler.rs:2138, crates/goblin-vm/src/value.rs:511, crates/goblin-vm/src/builtins.rs:1994, crates/goblin-vm/src/compiler.rs:2139, crates/goblin-vm/src/vm.rs:1840
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: is_whitespace
  interpreter: crates/goblin-interpreter/src/lib.rs:11998
  vm: crates/goblin-vm/src/compiler.rs:2142, crates/goblin-vm/src/value.rs:515, crates/goblin-vm/src/builtins.rs:2020, crates/goblin-vm/src/compiler.rs:2143, crates/goblin-vm/src/vm.rs:1844
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: items
  interpreter: crates/goblin-interpreter/src/lib.rs:13781
  vm: crates/goblin-vm/src/compiler.rs:2012, crates/goblin-vm/src/value.rs:406, crates/goblin-vm/src/builtins.rs:1076, crates/goblin-vm/src/compiler.rs:2013, crates/goblin-vm/src/vm.rs:1823
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: join
  interpreter: crates/goblin-interpreter/src/lib.rs:14990
  vm: crates/goblin-vm/src/compiler.rs:1985, crates/goblin-vm/src/value.rs:375, crates/goblin-vm/src/builtins.rs:399, crates/goblin-vm/src/compiler.rs:1986
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: json_parse
  interpreter: crates/goblin-interpreter/src/lib.rs:14392
  vm: crates/goblin-vm/src/compiler.rs:2006, crates/goblin-vm/src/value.rs:396, crates/goblin-vm/src/builtins.rs:659, crates/goblin-vm/src/compiler.rs:2007, crates/goblin-vm/src/vm.rs:1894
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: json_stringify
  interpreter: crates/goblin-interpreter/src/lib.rs:14426
  vm: crates/goblin-vm/src/compiler.rs:2007, crates/goblin-vm/src/value.rs:397, crates/goblin-vm/src/builtins.rs:665, crates/goblin-vm/src/compiler.rs:2008, crates/goblin-vm/src/vm.rs:1860
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: json_stringify_pretty
  interpreter: crates/goblin-interpreter/src/lib.rs:14457
  vm: crates/goblin-vm/src/compiler.rs:2008, crates/goblin-vm/src/value.rs:398, crates/goblin-vm/src/builtins.rs:671, crates/goblin-vm/src/compiler.rs:2009, crates/goblin-vm/src/vm.rs:1861
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: keep_after
  interpreter: crates/goblin-interpreter/src/lib.rs:16075
  vm: crates/goblin-vm/src/compiler.rs:1995, crates/goblin-vm/src/value.rs:385, crates/goblin-vm/src/builtins.rs:526, crates/goblin-vm/src/compiler.rs:1996
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: keep_before
  interpreter: crates/goblin-interpreter/src/lib.rs:16046
  vm: crates/goblin-vm/src/compiler.rs:1994, crates/goblin-vm/src/value.rs:384, crates/goblin-vm/src/builtins.rs:513, crates/goblin-vm/src/compiler.rs:1995
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: keep_between
  interpreter: crates/goblin-interpreter/src/lib.rs:16110
  vm: crates/goblin-vm/src/compiler.rs:1996, crates/goblin-vm/src/value.rs:386, crates/goblin-vm/src/builtins.rs:539, crates/goblin-vm/src/compiler.rs:1997
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: keep_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:15979
  vm: crates/goblin-vm/src/compiler.rs:2005, crates/goblin-vm/src/value.rs:395, crates/goblin-vm/src/builtins.rs:648, crates/goblin-vm/src/compiler.rs:2006
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: keys
  interpreter: crates/goblin-interpreter/src/lib.rs:13779
  vm: crates/goblin-vm/src/compiler.rs:2100, crates/goblin-vm/src/value.rs:404, crates/goblin-vm/src/builtins.rs:1058, crates/goblin-vm/src/compiler.rs:2101, crates/goblin-vm/src/vm.rs:1821
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: len
  interpreter: crates/goblin-interpreter/src/lib.rs:15268
  vm: crates/goblin-vm/src/compiler.rs:1969, crates/goblin-vm/src/value.rs:370, crates/goblin-vm/src/builtins.rs:346, crates/goblin-vm/src/compiler.rs:1970
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: lines
  interpreter: crates/goblin-interpreter/src/lib.rs:14886
  vm: crates/goblin-vm/src/compiler.rs:2161, crates/goblin-vm/src/value.rs:578, crates/goblin-vm/src/builtins.rs:2638, crates/goblin-vm/src/compiler.rs:2162, crates/goblin-vm/src/vm.rs:1824
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: link_score
  interpreter: crates/goblin-interpreter/src/lib.rs:6768
  vm: crates/goblin-vm/src/compiler.rs:2267, crates/goblin-vm/src/value.rs:678, crates/goblin-vm/src/builtins.rs:3472, crates/goblin-vm/src/compiler.rs:2268
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: list_dirs
  interpreter: crates/goblin-interpreter/src/lib.rs:13993
  vm: crates/goblin-vm/src/compiler.rs:2225, crates/goblin-vm/src/value.rs:643, crates/goblin-vm/src/builtins.rs:3112, crates/goblin-vm/src/compiler.rs:2226
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: list_tokens
  interpreter: crates/goblin-interpreter/src/lib.rs:6092
  vm: crates/goblin-vm/src/compiler.rs:2252, crates/goblin-vm/src/value.rs:672, crates/goblin-vm/src/builtins.rs:3385, crates/goblin-vm/src/compiler.rs:2253
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: lower
  interpreter: crates/goblin-interpreter/src/lib.rs:13766
  vm: crates/goblin-vm/src/compiler.rs:1976, crates/goblin-vm/src/value.rs:356, crates/goblin-vm/src/builtins.rs:202, crates/goblin-vm/src/compiler.rs:1977, crates/goblin-vm/src/vm.rs:1805
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: m
  interpreter: crates/goblin-interpreter/src/lib.rs:12425
  vm: crates/goblin-vm/src/compiler.rs:2195, crates/goblin-vm/src/value.rs:615, crates/goblin-vm/src/builtins.rs:2870, crates/goblin-vm/src/compiler.rs:2196, crates/goblin-vm/src/vm.rs:1891
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: map
  interpreter: crates/goblin-interpreter/src/lib.rs:13791
  vm: crates/goblin-vm/src/compiler.rs:2110, crates/goblin-vm/src/value.rs:416, crates/goblin-vm/src/builtins.rs:1326, crates/goblin-vm/src/compiler.rs:2111
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: max
  interpreter: crates/goblin-interpreter/src/lib.rs:6514
  vm: crates/goblin-vm/src/compiler.rs:1960, crates/goblin-vm/src/value.rs:345, crates/goblin-vm/src/builtins.rs:97, crates/goblin-vm/src/compiler.rs:1961, crates/goblin-vm/src/vm.rs:1886
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: md_to_html
  interpreter: crates/goblin-interpreter/src/lib.rs:11225
  vm: crates/goblin-vm/src/compiler.rs:2192, crates/goblin-vm/src/value.rs:612, crates/goblin-vm/src/builtins.rs:2832, crates/goblin-vm/src/compiler.rs:2193
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: mem_addr
  interpreter: crates/goblin-interpreter/src/lib.rs:13801
  vm: crates/goblin-vm/src/compiler.rs:1956, crates/goblin-vm/src/value.rs:337, crates/goblin-vm/src/builtins.rs:39, crates/goblin-vm/src/compiler.rs:1957
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: mem_human
  interpreter: crates/goblin-interpreter/src/lib.rs:13803
  vm: crates/goblin-vm/src/compiler.rs:2022, crates/goblin-vm/src/value.rs:339, crates/goblin-vm/src/builtins.rs:46, crates/goblin-vm/src/compiler.rs:2023
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: mem_total
  interpreter: crates/goblin-interpreter/src/lib.rs:13802
  vm: crates/goblin-vm/src/compiler.rs:2021, crates/goblin-vm/src/value.rs:338, crates/goblin-vm/src/builtins.rs:43, crates/goblin-vm/src/compiler.rs:2022
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: metrics
  interpreter: crates/goblin-interpreter/src/lib.rs:15338
  vm: crates/goblin-vm/src/compiler.rs:2241, crates/goblin-vm/src/value.rs:661, crates/goblin-vm/src/builtins.rs:3274, crates/goblin-vm/src/compiler.rs:2242
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: min
  interpreter: crates/goblin-interpreter/src/lib.rs:6432
  vm: crates/goblin-vm/src/compiler.rs:1959, crates/goblin-vm/src/value.rs:344, crates/goblin-vm/src/builtins.rs:63, crates/goblin-vm/src/compiler.rs:1960, crates/goblin-vm/src/vm.rs:1885
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: minimize
  interpreter: crates/goblin-interpreter/src/lib.rs:15217
  vm: crates/goblin-vm/src/compiler.rs:2106, crates/goblin-vm/src/value.rs:477, crates/goblin-vm/src/builtins.rs:1875, crates/goblin-vm/src/compiler.rs:2107, crates/goblin-vm/src/vm.rs:1857
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: mixed
  interpreter: crates/goblin-interpreter/src/lib.rs:13770
  vm: crates/goblin-vm/src/compiler.rs:1980, crates/goblin-vm/src/value.rs:360, crates/goblin-vm/src/builtins.rs:246, crates/goblin-vm/src/compiler.rs:1981, crates/goblin-vm/src/vm.rs:1809
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: mode
  interpreter: crates/goblin-interpreter/src/lib.rs:13789
  vm: crates/goblin-vm/src/compiler.rs:2015, crates/goblin-vm/src/value.rs:414, crates/goblin-vm/src/builtins.rs:1257, crates/goblin-vm/src/compiler.rs:2016, crates/goblin-vm/src/vm.rs:1889
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: normalize_newlines
  interpreter: crates/goblin-interpreter/src/lib.rs:15399
  vm: crates/goblin-vm/src/compiler.rs:1998, crates/goblin-vm/src/value.rs:388, crates/goblin-vm/src/builtins.rs:583, crates/goblin-vm/src/compiler.rs:1999, crates/goblin-vm/src/vm.rs:1859
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: ord
  interpreter: crates/goblin-interpreter/src/lib.rs:13776
  vm: crates/goblin-vm/src/compiler.rs:1983, crates/goblin-vm/src/value.rs:367, crates/goblin-vm/src/builtins.rs:330, crates/goblin-vm/src/compiler.rs:1984
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: overlay_strength
  interpreter: crates/goblin-interpreter/src/lib.rs:6757
  vm: crates/goblin-vm/src/compiler.rs:2266, crates/goblin-vm/src/value.rs:677, crates/goblin-vm/src/builtins.rs:3461, crates/goblin-vm/src/compiler.rs:2267
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: overlays_of
  interpreter: crates/goblin-interpreter/src/lib.rs:6746
  vm: crates/goblin-vm/src/compiler.rs:2265, crates/goblin-vm/src/value.rs:676, crates/goblin-vm/src/builtins.rs:3452, crates/goblin-vm/src/compiler.rs:2266
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: owned_by
  interpreter: crates/goblin-interpreter/src/lib.rs:13837
  vm: crates/goblin-vm/src/compiler.rs:2268, crates/goblin-vm/src/value.rs:679, crates/goblin-vm/src/builtins.rs:3399, crates/goblin-vm/src/compiler.rs:2269
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: owns_tree
  interpreter: crates/goblin-interpreter/src/lib.rs:13865
  vm: crates/goblin-vm/src/compiler.rs:2269, crates/goblin-vm/src/value.rs:680, crates/goblin-vm/src/builtins.rs:3414, crates/goblin-vm/src/compiler.rs:2270
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: pack
  interpreter: crates/goblin-interpreter/src/lib.rs:12086
  vm: crates/goblin-vm/src/compiler.rs:2159, crates/goblin-vm/src/value.rs:568, crates/goblin-vm/src/builtins.rs:2497, crates/goblin-vm/src/compiler.rs:2160, crates/goblin-vm/src/vm.rs:1819
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: parse_bool
  interpreter: crates/goblin-interpreter/src/lib.rs:15232
  vm: crates/goblin-vm/src/compiler.rs:2107, crates/goblin-vm/src/value.rs:478, crates/goblin-vm/src/builtins.rs:1888, crates/goblin-vm/src/compiler.rs:2108, crates/goblin-vm/src/vm.rs:1871
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: path_join
  interpreter: crates/goblin-interpreter/src/lib.rs:13984
  vm: crates/goblin-vm/src/compiler.rs:2220, crates/goblin-vm/src/value.rs:638, crates/goblin-vm/src/builtins.rs:3043, crates/goblin-vm/src/compiler.rs:2221
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: path_normalize
  interpreter: crates/goblin-interpreter/src/lib.rs:13986
  vm: crates/goblin-vm/src/compiler.rs:2222, crates/goblin-vm/src/value.rs:640, crates/goblin-vm/src/builtins.rs:3059, crates/goblin-vm/src/compiler.rs:2223, crates/goblin-vm/src/vm.rs:1919
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: path_relative_to
  interpreter: crates/goblin-interpreter/src/lib.rs:13990
  vm: crates/goblin-vm/src/compiler.rs:2223, crates/goblin-vm/src/value.rs:641, crates/goblin-vm/src/builtins.rs:3074, crates/goblin-vm/src/compiler.rs:2224
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: path_split
  interpreter: crates/goblin-interpreter/src/lib.rs:13989
  vm: crates/goblin-vm/src/compiler.rs:2221, crates/goblin-vm/src/value.rs:639, crates/goblin-vm/src/builtins.rs:3051, crates/goblin-vm/src/compiler.rs:2222, crates/goblin-vm/src/vm.rs:1918
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: pathfind
  interpreter: crates/goblin-interpreter/src/lib.rs:13991
  vm: crates/goblin-vm/src/compiler.rs:2229, crates/goblin-vm/src/value.rs:647, crates/goblin-vm/src/builtins.rs:3152, crates/goblin-vm/src/compiler.rs:2230
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: pct
  interpreter: crates/goblin-interpreter/src/lib.rs:12421
  vm: crates/goblin-vm/src/compiler.rs:2169, crates/goblin-vm/src/value.rs:588, crates/goblin-vm/src/builtins.rs:826, crates/goblin-vm/src/compiler.rs:2170, crates/goblin-vm/src/vm.rs:1833
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: percent
  interpreter: crates/goblin-interpreter/src/lib.rs:12421
  vm: untraced
  status: MISSING
  note: no compiler::builtin_by_name mapping
* name: pick
  interpreter: crates/goblin-interpreter/src/lib.rs:12671
  vm: crates/goblin-vm/src/compiler.rs:2173, crates/goblin-vm/src/value.rs:592, crates/goblin-vm/src/builtins.rs:929, crates/goblin-vm/src/compiler.rs:2174
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: pow
  interpreter: crates/goblin-interpreter/src/lib.rs:12531
  vm: crates/goblin-vm/src/compiler.rs:1968, crates/goblin-vm/src/value.rs:353, crates/goblin-vm/src/builtins.rs:188, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:1247, crates/goblin-vm/src/compiler.rs:1969
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: provoke
  interpreter: crates/goblin-interpreter/src/lib.rs:11543
  vm: crates/goblin-vm/src/compiler.rs:2182, crates/goblin-vm/src/value.rs:602, crates/goblin-vm/src/builtins.rs:2752, crates/goblin-vm/src/compiler.rs:2183, crates/goblin-vm/src/vm.rs:728
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put
  interpreter: crates/goblin-interpreter/src/lib.rs:14048
  vm: crates/goblin-vm/src/compiler.rs:2054, crates/goblin-vm/src/value.rs:432, crates/goblin-vm/src/builtins.rs:1447, crates/goblin-vm/src/compiler.rs:2055
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put_at
  interpreter: crates/goblin-interpreter/src/lib.rs:14063
  vm: crates/goblin-vm/src/compiler.rs:2057, crates/goblin-vm/src/value.rs:435, crates/goblin-vm/src/builtins.rs:1469, crates/goblin-vm/src/compiler.rs:2058
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put_between
  interpreter: crates/goblin-interpreter/src/lib.rs:14074
  vm: crates/goblin-vm/src/compiler.rs:2086, crates/goblin-vm/src/value.rs:464, crates/goblin-vm/src/builtins.rs:1727, crates/goblin-vm/src/compiler.rs:2087
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put_first
  interpreter: crates/goblin-interpreter/src/lib.rs:14053
  vm: crates/goblin-vm/src/compiler.rs:2055, crates/goblin-vm/src/value.rs:433, crates/goblin-vm/src/builtins.rs:1461, crates/goblin-vm/src/compiler.rs:2056
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14058
  vm: crates/goblin-vm/src/compiler.rs:2056, crates/goblin-vm/src/value.rs:434, crates/goblin-vm/src/builtins.rs:1465, crates/goblin-vm/src/compiler.rs:1367, crates/goblin-vm/src/compiler.rs:2057
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:14068
  vm: crates/goblin-vm/src/compiler.rs:2085, crates/goblin-vm/src/value.rs:464, crates/goblin-vm/src/builtins.rs:1720, crates/goblin-vm/src/compiler.rs:2086
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: put_random
  interpreter: crates/goblin-interpreter/src/lib.rs:14081
  vm: crates/goblin-vm/src/compiler.rs:2087, crates/goblin-vm/src/value.rs:464, crates/goblin-vm/src/builtins.rs:1735, crates/goblin-vm/src/compiler.rs:2088
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: rand_seed
  interpreter: crates/goblin-interpreter/src/lib.rs:13208
  vm: crates/goblin-vm/src/compiler.rs:2044, crates/goblin-vm/src/value.rs:547, crates/goblin-vm/src/builtins.rs:2208, crates/goblin-vm/src/compiler.rs:2045
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: raw
  interpreter: crates/goblin-interpreter/src/lib.rs:13769
  vm: crates/goblin-vm/src/compiler.rs:1981, crates/goblin-vm/src/value.rs:361, crates/goblin-vm/src/builtins.rs:252, crates/goblin-vm/src/compiler.rs:1982, crates/goblin-vm/src/vm.rs:1810
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: read_json
  interpreter: crates/goblin-interpreter/src/lib.rs:14502
  vm: crates/goblin-vm/src/compiler.rs:2174, crates/goblin-vm/src/value.rs:593, crates/goblin-vm/src/builtins.rs:1019, crates/goblin-vm/src/compiler.rs:2175
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: read_text
  interpreter: crates/goblin-interpreter/src/lib.rs:13979
  vm: crates/goblin-vm/src/compiler.rs:2196, crates/goblin-vm/src/value.rs:616, crates/goblin-vm/src/builtins.rs:1012, crates/goblin-vm/src/compiler.rs:2197
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap
  interpreter: crates/goblin-interpreter/src/lib.rs:14548
  vm: crates/goblin-vm/src/compiler.rs:2068, crates/goblin-vm/src/value.rs:597, crates/goblin-vm/src/builtins.rs:1632, crates/goblin-vm/src/compiler.rs:2069
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap_at
  interpreter: crates/goblin-interpreter/src/lib.rs:14194
  vm: crates/goblin-vm/src/compiler.rs:2071, crates/goblin-vm/src/value.rs:455, crates/goblin-vm/src/builtins.rs:1581, crates/goblin-vm/src/compiler.rs:2072
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap_between
  interpreter: crates/goblin-interpreter/src/lib.rs:14211
  vm: crates/goblin-vm/src/compiler.rs:2098, crates/goblin-vm/src/value.rs:470, crates/goblin-vm/src/builtins.rs:1833, crates/goblin-vm/src/compiler.rs:2099
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap_first
  interpreter: crates/goblin-interpreter/src/lib.rs:14184
  vm: crates/goblin-vm/src/compiler.rs:2069, crates/goblin-vm/src/value.rs:453, crates/goblin-vm/src/builtins.rs:1571, crates/goblin-vm/src/compiler.rs:2070
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14189
  vm: crates/goblin-vm/src/compiler.rs:2070, crates/goblin-vm/src/value.rs:454, crates/goblin-vm/src/builtins.rs:1576, crates/goblin-vm/src/compiler.rs:2071
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:14205
  vm: crates/goblin-vm/src/compiler.rs:2097, crates/goblin-vm/src/value.rs:470, crates/goblin-vm/src/builtins.rs:1827, crates/goblin-vm/src/compiler.rs:2098
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reap_where
  interpreter: crates/goblin-interpreter/src/lib.rs:14199
  vm: crates/goblin-vm/src/compiler.rs:2073, crates/goblin-vm/src/value.rs:457, crates/goblin-vm/src/builtins.rs:1601, crates/goblin-vm/src/compiler.rs:2074
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: register_token
  interpreter: crates/goblin-interpreter/src/lib.rs:5982
  vm: crates/goblin-vm/src/compiler.rs:2247, crates/goblin-vm/src/value.rs:667, crates/goblin-vm/src/builtins.rs:3343, crates/goblin-vm/src/compiler.rs:2248
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: req_body
  interpreter: crates/goblin-interpreter/src/lib.rs:13963
  vm: crates/goblin-vm/src/compiler.rs:2033, crates/goblin-vm/src/value.rs:553, crates/goblin-vm/src/builtins.rs:2374, crates/goblin-vm/src/compiler.rs:2034
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: req_header
  interpreter: crates/goblin-interpreter/src/lib.rs:13964
  vm: crates/goblin-vm/src/compiler.rs:2034, crates/goblin-vm/src/value.rs:554, crates/goblin-vm/src/builtins.rs:2377, crates/goblin-vm/src/compiler.rs:2035
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: req_method
  interpreter: crates/goblin-interpreter/src/lib.rs:13960
  vm: crates/goblin-vm/src/compiler.rs:2030, crates/goblin-vm/src/value.rs:550, crates/goblin-vm/src/builtins.rs:2365, crates/goblin-vm/src/compiler.rs:2031
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: req_path
  interpreter: crates/goblin-interpreter/src/lib.rs:13961
  vm: crates/goblin-vm/src/compiler.rs:2031, crates/goblin-vm/src/value.rs:551, crates/goblin-vm/src/builtins.rs:2368, crates/goblin-vm/src/compiler.rs:2032
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: req_query
  interpreter: crates/goblin-interpreter/src/lib.rs:13962
  vm: crates/goblin-vm/src/compiler.rs:2032, crates/goblin-vm/src/value.rs:552, crates/goblin-vm/src/builtins.rs:2371, crates/goblin-vm/src/compiler.rs:2033
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: resolve_token
  interpreter: crates/goblin-interpreter/src/lib.rs:5991
  vm: crates/goblin-vm/src/compiler.rs:2248, crates/goblin-vm/src/value.rs:668, crates/goblin-vm/src/builtins.rs:3353, crates/goblin-vm/src/compiler.rs:2249
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reverse
  interpreter: crates/goblin-interpreter/src/lib.rs:15190
  vm: crates/goblin-vm/src/compiler.rs:2104, crates/goblin-vm/src/value.rs:475, crates/goblin-vm/src/builtins.rs:1864, crates/goblin-vm/src/compiler.rs:2105, crates/goblin-vm/src/vm.rs:1814
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: reverse_chars
  interpreter: crates/goblin-interpreter/src/lib.rs:15211
  vm: crates/goblin-vm/src/compiler.rs:2105, crates/goblin-vm/src/value.rs:476, crates/goblin-vm/src/builtins.rs:1871, crates/goblin-vm/src/compiler.rs:2106, crates/goblin-vm/src/vm.rs:1856
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: roll
  interpreter: crates/goblin-interpreter/src/lib.rs:13218
  vm: crates/goblin-vm/src/compiler.rs:2041, crates/goblin-vm/src/value.rs:545, crates/goblin-vm/src/builtins.rs:2213, crates/goblin-vm/src/builtins.rs:3218, crates/goblin-vm/src/compiler.rs:2043
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: roll_detail
  interpreter: crates/goblin-interpreter/src/lib.rs:13423
  vm: crates/goblin-vm/src/compiler.rs:2043, crates/goblin-vm/src/value.rs:546, crates/goblin-vm/src/builtins.rs:2265, crates/goblin-vm/src/builtins.rs:3225, crates/goblin-vm/src/compiler.rs:2044
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: roll_detail_str
  interpreter: crates/goblin-interpreter/src/lib.rs:13672
  vm: crates/goblin-vm/src/compiler.rs:2234, crates/goblin-vm/src/value.rs:654, crates/goblin-vm/src/builtins.rs:3220, crates/goblin-vm/src/compiler.rs:2235
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: roll_str
  interpreter: crates/goblin-interpreter/src/lib.rs:13672
  vm: crates/goblin-vm/src/compiler.rs:2233, crates/goblin-vm/src/value.rs:653, crates/goblin-vm/src/builtins.rs:3213, crates/goblin-vm/src/compiler.rs:2234
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: round
  interpreter: crates/goblin-interpreter/src/lib.rs:12443
  vm: crates/goblin-vm/src/compiler.rs:1965, crates/goblin-vm/src/value.rs:350, crates/goblin-vm/src/builtins.rs:161, crates/goblin-vm/src/compiler.rs:1966, crates/goblin-vm/src/vm.rs:1831
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: run_cmd
  interpreter: crates/goblin-interpreter/src/lib.rs:13796
  vm: crates/goblin-vm/src/compiler.rs:2027, crates/goblin-vm/src/value.rs:544, crates/goblin-vm/src/builtins.rs:2155, crates/goblin-vm/src/compiler.rs:2028
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: sample_weighted
  interpreter: crates/goblin-interpreter/src/lib.rs:13790
  vm: crates/goblin-vm/src/compiler.rs:2016, crates/goblin-vm/src/value.rs:415, crates/goblin-vm/src/builtins.rs:1276, crates/goblin-vm/src/compiler.rs:2017
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: sanitize_bom
  interpreter: crates/goblin-interpreter/src/lib.rs:15379
  vm: crates/goblin-vm/src/compiler.rs:1997, crates/goblin-vm/src/value.rs:387, crates/goblin-vm/src/builtins.rs:570, crates/goblin-vm/src/compiler.rs:1998, crates/goblin-vm/src/vm.rs:1858
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: secure_pick
  interpreter: crates/goblin-interpreter/src/lib.rs:13806
  vm: crates/goblin-vm/src/compiler.rs:2156, crates/goblin-vm/src/value.rs:563, crates/goblin-vm/src/builtins.rs:2521, crates/goblin-vm/src/compiler.rs:2157
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: secure_random
  interpreter: crates/goblin-interpreter/src/lib.rs:13807
  vm: crates/goblin-vm/src/compiler.rs:2157, crates/goblin-vm/src/value.rs:564, crates/goblin-vm/src/builtins.rs:2605, crates/goblin-vm/src/compiler.rs:2158
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: secure_shuffle
  interpreter: crates/goblin-interpreter/src/lib.rs:13808
  vm: crates/goblin-vm/src/compiler.rs:2158, crates/goblin-vm/src/value.rs:565, crates/goblin-vm/src/builtins.rs:2616, crates/goblin-vm/src/compiler.rs:2159
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: set_cookie
  interpreter: crates/goblin-interpreter/src/lib.rs:13970
  vm: crates/goblin-vm/src/compiler.rs:2040, crates/goblin-vm/src/value.rs:560, crates/goblin-vm/src/builtins.rs:2450, crates/goblin-vm/src/compiler.rs:2041
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: set_header
  interpreter: crates/goblin-interpreter/src/lib.rs:13969
  vm: crates/goblin-vm/src/compiler.rs:2039, crates/goblin-vm/src/value.rs:559, crates/goblin-vm/src/builtins.rs:2435, crates/goblin-vm/src/compiler.rs:2040
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: set_status
  interpreter: crates/goblin-interpreter/src/lib.rs:13968
  vm: crates/goblin-vm/src/compiler.rs:2038, crates/goblin-vm/src/value.rs:558, crates/goblin-vm/src/builtins.rs:2426, crates/goblin-vm/src/compiler.rs:2039
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: shuffle
  interpreter: crates/goblin-interpreter/src/lib.rs:13786
  vm: crates/goblin-vm/src/compiler.rs:2013, crates/goblin-vm/src/value.rs:411, crates/goblin-vm/src/builtins.rs:1153, crates/goblin-vm/src/compiler.rs:2014, crates/goblin-vm/src/vm.rs:1815
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: slug
  interpreter: crates/goblin-interpreter/src/lib.rs:13768
  vm: crates/goblin-vm/src/compiler.rs:1979, crates/goblin-vm/src/value.rs:359, crates/goblin-vm/src/builtins.rs:229, crates/goblin-vm/src/compiler.rs:1980, crates/goblin-vm/src/vm.rs:1808
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: sort
  interpreter: crates/goblin-interpreter/src/lib.rs:13787
  vm: crates/goblin-vm/src/compiler.rs:2108, crates/goblin-vm/src/value.rs:412, crates/goblin-vm/src/builtins.rs:1200, crates/goblin-vm/src/compiler.rs:2109, crates/goblin-vm/src/vm.rs:1816
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: split
  interpreter: crates/goblin-interpreter/src/lib.rs:14945
  vm: crates/goblin-vm/src/compiler.rs:1984, crates/goblin-vm/src/value.rs:374, crates/goblin-vm/src/builtins.rs:376, crates/goblin-vm/src/compiler.rs:1985
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: sqrt
  interpreter: crates/goblin-interpreter/src/lib.rs:12618
  vm: crates/goblin-vm/src/compiler.rs:1966, crates/goblin-vm/src/value.rs:351, crates/goblin-vm/src/builtins.rs:169, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1967, crates/goblin-vm/src/vm.rs:1828
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: starts_with
  interpreter: crates/goblin-interpreter/src/lib.rs:16240
  vm: crates/goblin-vm/src/compiler.rs:1987, crates/goblin-vm/src/value.rs:377, crates/goblin-vm/src/builtins.rs:445, crates/goblin-vm/src/compiler.rs:1988
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: stem
  interpreter: crates/goblin-interpreter/src/lib.rs:13981
  vm: crates/goblin-vm/src/compiler.rs:2218, crates/goblin-vm/src/value.rs:636, crates/goblin-vm/src/builtins.rs:3030, crates/goblin-vm/src/compiler.rs:2219, crates/goblin-vm/src/vm.rs:1916
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: str
  interpreter: crates/goblin-interpreter/src/lib.rs:12417
  vm: crates/goblin-vm/src/compiler.rs:1970, crates/goblin-vm/src/value.rs:526, crates/goblin-vm/src/builtins.rs:358, crates/goblin-vm/src/compiler.rs:1971, crates/goblin-vm/src/compiler.rs:2152, crates/goblin-vm/src/vm.rs:1800
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: string
  interpreter: crates/goblin-interpreter/src/lib.rs:12417
  vm: untraced
  status: MISSING
  note: no compiler::builtin_by_name mapping
* name: sum
  interpreter: crates/goblin-interpreter/src/lib.rs:6370
  vm: crates/goblin-vm/src/compiler.rs:1962, crates/goblin-vm/src/value.rs:347, crates/goblin-vm/src/builtins.rs:138, crates/goblin-vm/src/compiler.rs:1963, crates/goblin-vm/src/vm.rs:1883
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: summon
  interpreter: crates/goblin-interpreter/src/lib.rs:11477
  vm: crates/goblin-vm/src/compiler.rs:2181, crates/goblin-vm/src/value.rs:601, crates/goblin-vm/src/builtins.rs:2752, crates/goblin-vm/src/compiler.rs:2182, crates/goblin-vm/src/vm.rs:723
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: tick
  interpreter: crates/goblin-interpreter/src/lib.rs:6715
  vm: crates/goblin-vm/src/compiler.rs:2261, crates/goblin-vm/src/value.rs:698, crates/goblin-vm/src/builtins.rs:3332, crates/goblin-vm/src/compiler.rs:2262, crates/goblin-vm/src/vm.rs:733
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: tick_db
  interpreter: crates/goblin-interpreter/src/lib.rs:6715
  vm: crates/goblin-vm/src/compiler.rs:2261, crates/goblin-vm/src/value.rs:698, crates/goblin-vm/src/builtins.rs:3332, crates/goblin-vm/src/compiler.rs:2262, crates/goblin-vm/src/vm.rs:733
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: title
  interpreter: crates/goblin-interpreter/src/lib.rs:13767
  vm: crates/goblin-vm/src/compiler.rs:1978, crates/goblin-vm/src/value.rs:358, crates/goblin-vm/src/builtins.rs:210, crates/goblin-vm/src/compiler.rs:1979, crates/goblin-vm/src/vm.rs:1807
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: to_map
  interpreter: crates/goblin-interpreter/src/lib.rs:12425
  vm: crates/goblin-vm/src/compiler.rs:2195, crates/goblin-vm/src/value.rs:615, crates/goblin-vm/src/builtins.rs:2870, crates/goblin-vm/src/compiler.rs:2196, crates/goblin-vm/src/vm.rs:1891
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: tokenize
  interpreter: crates/goblin-interpreter/src/lib.rs:15138
  vm: untraced
  status: MISSING
  note: no compiler::builtin_by_name mapping
* name: trim
  interpreter: crates/goblin-interpreter/src/lib.rs:13771
  vm: crates/goblin-vm/src/compiler.rs:1973, crates/goblin-vm/src/value.rs:362, crates/goblin-vm/src/builtins.rs:262, crates/goblin-vm/src/compiler.rs:1974, crates/goblin-vm/src/vm.rs:1811
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: trim_lead
  interpreter: crates/goblin-interpreter/src/lib.rs:13772
  vm: crates/goblin-vm/src/compiler.rs:1974, crates/goblin-vm/src/value.rs:363, crates/goblin-vm/src/builtins.rs:271, crates/goblin-vm/src/compiler.rs:1975, crates/goblin-vm/src/vm.rs:1812
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: trim_trail
  interpreter: crates/goblin-interpreter/src/lib.rs:13773
  vm: crates/goblin-vm/src/compiler.rs:1975, crates/goblin-vm/src/value.rs:364, crates/goblin-vm/src/builtins.rs:280, crates/goblin-vm/src/compiler.rs:1976, crates/goblin-vm/src/vm.rs:1813
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: u16
  interpreter: crates/goblin-interpreter/src/lib.rs:12438
  vm: crates/goblin-vm/src/compiler.rs:2203, crates/goblin-vm/src/value.rs:624, crates/goblin-vm/src/builtins.rs:2937, crates/goblin-vm/src/compiler.rs:2204, crates/goblin-vm/src/vm.rs:1908
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: u32
  interpreter: crates/goblin-interpreter/src/lib.rs:12439
  vm: crates/goblin-vm/src/compiler.rs:2204, crates/goblin-vm/src/value.rs:625, crates/goblin-vm/src/builtins.rs:2945, crates/goblin-vm/src/compiler.rs:2205, crates/goblin-vm/src/vm.rs:1909
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: u64
  interpreter: crates/goblin-interpreter/src/lib.rs:12440
  vm: crates/goblin-vm/src/compiler.rs:2205, crates/goblin-vm/src/value.rs:626, crates/goblin-vm/src/builtins.rs:2953, crates/goblin-vm/src/compiler.rs:2206, crates/goblin-vm/src/vm.rs:1910
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: u8
  interpreter: crates/goblin-interpreter/src/lib.rs:12437
  vm: crates/goblin-vm/src/compiler.rs:2202, crates/goblin-vm/src/value.rs:623, crates/goblin-vm/src/builtins.rs:2929, crates/goblin-vm/src/compiler.rs:2203, crates/goblin-vm/src/vm.rs:1907
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: unique
  interpreter: crates/goblin-interpreter/src/lib.rs:13792
  vm: crates/goblin-vm/src/compiler.rs:2119, crates/goblin-vm/src/value.rs:417, crates/goblin-vm/src/builtins.rs:1362, crates/goblin-vm/src/compiler.rs:2120, crates/goblin-vm/src/vm.rs:1817
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: unpack
  interpreter: crates/goblin-interpreter/src/lib.rs:12048
  vm: crates/goblin-vm/src/compiler.rs:2160, crates/goblin-vm/src/value.rs:569, crates/goblin-vm/src/builtins.rs:2502, crates/goblin-vm/src/compiler.rs:2161, crates/goblin-vm/src/vm.rs:1820
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update
  interpreter: crates/goblin-interpreter/src/lib.rs:14086
  vm: crates/goblin-vm/src/compiler.rs:2058, crates/goblin-vm/src/value.rs:438, crates/goblin-vm/src/builtins.rs:1477, crates/goblin-vm/src/compiler.rs:2059
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_all
  interpreter: crates/goblin-interpreter/src/lib.rs:14112
  vm: crates/goblin-vm/src/compiler.rs:2089, crates/goblin-vm/src/value.rs:466, crates/goblin-vm/src/builtins.rs:1749, crates/goblin-vm/src/compiler.rs:2090
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_at
  interpreter: crates/goblin-interpreter/src/lib.rs:14101
  vm: crates/goblin-vm/src/compiler.rs:2061, crates/goblin-vm/src/value.rs:441, crates/goblin-vm/src/builtins.rs:1491, crates/goblin-vm/src/compiler.rs:2062
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_between
  interpreter: crates/goblin-interpreter/src/lib.rs:14123
  vm: crates/goblin-vm/src/compiler.rs:2092, crates/goblin-vm/src/value.rs:466, crates/goblin-vm/src/builtins.rs:1769, crates/goblin-vm/src/compiler.rs:2093
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_first
  interpreter: crates/goblin-interpreter/src/lib.rs:14091
  vm: crates/goblin-vm/src/compiler.rs:2059, crates/goblin-vm/src/value.rs:439, crates/goblin-vm/src/builtins.rs:1483, crates/goblin-vm/src/compiler.rs:2060
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_last
  interpreter: crates/goblin-interpreter/src/lib.rs:14096
  vm: crates/goblin-vm/src/compiler.rs:2060, crates/goblin-vm/src/value.rs:440, crates/goblin-vm/src/builtins.rs:1487, crates/goblin-vm/src/compiler.rs:2061
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_matching
  interpreter: crates/goblin-interpreter/src/lib.rs:14117
  vm: crates/goblin-vm/src/compiler.rs:2091, crates/goblin-vm/src/value.rs:466, crates/goblin-vm/src/builtins.rs:1762, crates/goblin-vm/src/compiler.rs:2092
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_random
  interpreter: crates/goblin-interpreter/src/lib.rs:14130
  vm: crates/goblin-vm/src/compiler.rs:2093, crates/goblin-vm/src/value.rs:466, crates/goblin-vm/src/builtins.rs:1777, crates/goblin-vm/src/compiler.rs:2094
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: update_where
  interpreter: crates/goblin-interpreter/src/lib.rs:14106
  vm: crates/goblin-vm/src/compiler.rs:2090, crates/goblin-vm/src/value.rs:466, crates/goblin-vm/src/builtins.rs:1755, crates/goblin-vm/src/compiler.rs:2091
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: upper
  interpreter: crates/goblin-interpreter/src/lib.rs:13765
  vm: crates/goblin-vm/src/compiler.rs:1977, crates/goblin-vm/src/value.rs:357, crates/goblin-vm/src/builtins.rs:206, crates/goblin-vm/src/compiler.rs:1978, crates/goblin-vm/src/vm.rs:1806
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: uuid_v4
  interpreter: crates/goblin-interpreter/src/lib.rs:13995
  vm: crates/goblin-vm/src/compiler.rs:2227, crates/goblin-vm/src/value.rs:645, crates/goblin-vm/src/builtins.rs:3146, crates/goblin-vm/src/compiler.rs:2228
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: uuid_v7
  interpreter: crates/goblin-interpreter/src/lib.rs:13996
  vm: crates/goblin-vm/src/compiler.rs:2228, crates/goblin-vm/src/value.rs:646, crates/goblin-vm/src/builtins.rs:3149, crates/goblin-vm/src/compiler.rs:2229
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: valtype
  interpreter: crates/goblin-interpreter/src/lib.rs:11594
  vm: crates/goblin-vm/src/compiler.rs:2237, crates/goblin-vm/src/value.rs:657, crates/goblin-vm/src/builtins.rs:3229, crates/goblin-vm/src/compiler.rs:2238
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: values
  interpreter: crates/goblin-interpreter/src/lib.rs:13780
  vm: crates/goblin-vm/src/compiler.rs:2101, crates/goblin-vm/src/value.rs:405, crates/goblin-vm/src/builtins.rs:1067, crates/goblin-vm/src/compiler.rs:2102, crates/goblin-vm/src/vm.rs:1822
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: vt
  interpreter: crates/goblin-interpreter/src/lib.rs:11594
  vm: crates/goblin-vm/src/compiler.rs:2237, crates/goblin-vm/src/value.rs:657, crates/goblin-vm/src/builtins.rs:3229, crates/goblin-vm/src/compiler.rs:2238
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: walk
  interpreter: crates/goblin-interpreter/src/lib.rs:13992
  vm: crates/goblin-vm/src/compiler.rs:2224, crates/goblin-vm/src/value.rs:642, crates/goblin-vm/src/builtins.rs:3081, crates/goblin-vm/src/compiler.rs:2225
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: words
  interpreter: crates/goblin-interpreter/src/lib.rs:14908
  vm: crates/goblin-vm/src/compiler.rs:2162, crates/goblin-vm/src/value.rs:579, crates/goblin-vm/src/builtins.rs:2646, crates/goblin-vm/src/compiler.rs:2163, crates/goblin-vm/src/vm.rs:1825
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: write_json
  interpreter: crates/goblin-interpreter/src/lib.rs:14488
  vm: crates/goblin-vm/src/compiler.rs:2177, crates/goblin-vm/src/value.rs:596, crates/goblin-vm/src/builtins.rs:1045, crates/goblin-vm/src/compiler.rs:2178
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: write_text
  interpreter: crates/goblin-interpreter/src/lib.rs:13978
  vm: crates/goblin-vm/src/compiler.rs:2175, crates/goblin-vm/src/value.rs:594, crates/goblin-vm/src/builtins.rs:1027, crates/goblin-vm/src/compiler.rs:2176
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: yall_minify
  interpreter: crates/goblin-interpreter/src/lib.rs:14372
  vm: crates/goblin-vm/src/compiler.rs:2188, crates/goblin-vm/src/value.rs:608, crates/goblin-vm/src/builtins.rs:2792, crates/goblin-vm/src/compiler.rs:2189, crates/goblin-vm/src/vm.rs:1897
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: yall_parse
  interpreter: crates/goblin-interpreter/src/lib.rs:14219
  vm: crates/goblin-vm/src/compiler.rs:2183, crates/goblin-vm/src/value.rs:603, crates/goblin-vm/src/builtins.rs:2756, crates/goblin-vm/src/compiler.rs:2184
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: yall_parse_file
  interpreter: crates/goblin-interpreter/src/lib.rs:14258
  vm: crates/goblin-vm/src/compiler.rs:2184, crates/goblin-vm/src/value.rs:604, crates/goblin-vm/src/builtins.rs:2764, crates/goblin-vm/src/compiler.rs:2185
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: yall_pretty
  interpreter: crates/goblin-interpreter/src/lib.rs:14353
  vm: crates/goblin-vm/src/compiler.rs:2187, crates/goblin-vm/src/value.rs:607, crates/goblin-vm/src/builtins.rs:2786, crates/goblin-vm/src/compiler.rs:2188, crates/goblin-vm/src/vm.rs:1896
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: yall_write
  interpreter: crates/goblin-interpreter/src/lib.rs:14296
  vm: crates/goblin-vm/src/compiler.rs:2185, crates/goblin-vm/src/value.rs:605, crates/goblin-vm/src/builtins.rs:2771, crates/goblin-vm/src/compiler.rs:2186, crates/goblin-vm/src/vm.rs:1895
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: yall_write_file
  interpreter: crates/goblin-interpreter/src/lib.rs:14316
  vm: crates/goblin-vm/src/compiler.rs:2186, crates/goblin-vm/src/value.rs:606, crates/goblin-vm/src/builtins.rs:2777, crates/goblin-vm/src/compiler.rs:2187
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: zip_dir
  interpreter: crates/goblin-interpreter/src/lib.rs:13975
  vm: crates/goblin-vm/src/compiler.rs:2244, crates/goblin-vm/src/value.rs:664, crates/goblin-vm/src/builtins.rs:3300, crates/goblin-vm/src/compiler.rs:2245
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Array
  interpreter: crates/goblin-interpreter/src/lib.rs:18473
  vm: crates/goblin-vm/src/compiler.rs:820, crates/goblin-vm/src/compiler.rs:1785, crates/goblin-vm/src/compiler.rs:1786, crates/goblin-vm/src/compiler.rs:1829, crates/goblin-vm/src/compiler.rs:822, crates/goblin-vm/src/compiler.rs:1333, crates/goblin-vm/src/compiler.rs:1724, crates/goblin-vm/src/compiler.rs:1746, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:500, crates/goblin-vm/src/compiler.rs:831, crates/goblin-vm/src/compiler.rs:1053, crates/goblin-vm/src/debug.rs:62, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573, crates/goblin-vm/src/compiler.rs:255, crates/goblin-vm/src/compiler.rs:288, crates/goblin-vm/src/compiler.rs:428, crates/goblin-vm/src/compiler.rs:466
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Binary
  interpreter: crates/goblin-interpreter/src/lib.rs:17721
  vm: crates/goblin-vm/src/compiler.rs:963, crates/goblin-vm/src/compiler.rs:1148, crates/goblin-vm/src/compiler.rs:1151, crates/goblin-vm/src/compiler.rs:970, crates/goblin-vm/src/opcode.rs:255, crates/goblin-vm/src/vm.rs:810, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020, crates/goblin-vm/src/compiler.rs:1165, crates/goblin-vm/src/opcode.rs:248, crates/goblin-vm/src/vm.rs:507
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Block
  interpreter: crates/goblin-interpreter/src/lib.rs:18027
  vm: crates/goblin-vm/src/compiler.rs:181, crates/goblin-vm/src/compiler.rs:536, crates/goblin-vm/src/compiler.rs:994, crates/goblin-vm/src/compiler.rs:549, crates/goblin-vm/src/compiler.rs:915, crates/goblin-vm/src/compiler.rs:937, crates/goblin-vm/src/compiler.rs:1172, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Bool
  interpreter: crates/goblin-interpreter/src/lib.rs:17889
  vm: crates/goblin-vm/src/compiler.rs:756, crates/goblin-vm/src/compiler.rs:757, crates/goblin-vm/src/vm.rs:1783, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/opcode.rs:204, crates/goblin-vm/src/vm.rs:166, crates/goblin-vm/src/opcode.rs:205, crates/goblin-vm/src/vm.rs:169, crates/goblin-vm/src/vm.rs:2008
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::BoxVar
  interpreter: crates/goblin-interpreter/src/lib.rs:17973
  vm: crates/goblin-vm/src/compiler.rs:1070, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020
  status: MISSING
  note: VM explicitly reports that box_store is unavailable at crates/goblin-vm/src/builtins.rs:3908
* name: Expr::Call
  interpreter: crates/goblin-interpreter/src/lib.rs:20327
  vm: crates/goblin-vm/src/compiler.rs:870, crates/goblin-vm/src/compiler.rs:1100, crates/goblin-vm/src/compiler.rs:866, crates/goblin-vm/src/compiler.rs:878, crates/goblin-vm/src/compiler.rs:907, crates/goblin-vm/src/compiler.rs:927, crates/goblin-vm/src/compiler.rs:884, crates/goblin-vm/src/opcode.rs:250, crates/goblin-vm/src/vm.rs:580, crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Char
  interpreter: crates/goblin-interpreter/src/lib.rs:17992
  vm: crates/goblin-vm/src/compiler.rs:808, crates/goblin-vm/src/vm.rs:1789, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:822, crates/goblin-vm/src/compiler.rs:1333, crates/goblin-vm/src/compiler.rs:1724, crates/goblin-vm/src/compiler.rs:1746
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::EnumVariant
  interpreter: crates/goblin-interpreter/src/lib.rs:18303
  vm: crates/goblin-vm/src/compiler.rs:1041, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:500, crates/goblin-vm/src/compiler.rs:831, crates/goblin-vm/src/compiler.rs:1053, crates/goblin-vm/src/debug.rs:62, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::FreeCall
  interpreter: crates/goblin-interpreter/src/lib.rs:19112
  vm: crates/goblin-vm/src/compiler.rs:854, crates/goblin-vm/src/compiler.rs:1085, crates/goblin-vm/src/tick.rs:1057, crates/goblin-vm/src/tick.rs:1061, crates/goblin-vm/src/compiler.rs:866, crates/goblin-vm/src/compiler.rs:878, crates/goblin-vm/src/compiler.rs:907, crates/goblin-vm/src/compiler.rs:927, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Ident
  interpreter: crates/goblin-interpreter/src/lib.rs:17993
  vm: crates/goblin-vm/src/compiler.rs:493, crates/goblin-vm/src/compiler.rs:611, crates/goblin-vm/src/compiler.rs:624, crates/goblin-vm/src/compiler.rs:814, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:500, crates/goblin-vm/src/compiler.rs:831, crates/goblin-vm/src/compiler.rs:1053, crates/goblin-vm/src/debug.rs:62, crates/goblin-vm/src/compiler.rs:263, crates/goblin-vm/src/compiler.rs:317, crates/goblin-vm/src/compiler.rs:322, crates/goblin-vm/src/compiler.rs:502, crates/goblin-vm/src/compiler.rs:511, crates/goblin-vm/src/opcode.rs:272, crates/goblin-vm/src/vm.rs:1081, crates/goblin-vm/src/compiler.rs:614, crates/goblin-vm/src/opcode.rs:264, crates/goblin-vm/src/vm.rs:980, crates/goblin-vm/src/compiler.rs:627, crates/goblin-vm/src/opcode.rs:265, crates/goblin-vm/src/vm.rs:1017
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Index
  interpreter: crates/goblin-interpreter/src/lib.rs:18492
  vm: crates/goblin-vm/src/compiler.rs:835, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573, crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Index2
  interpreter: crates/goblin-interpreter/src/lib.rs:17723
  vm: crates/goblin-vm/src/compiler.rs:1033, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::IndexMap
  interpreter: crates/goblin-interpreter/src/lib.rs:18580
  vm: crates/goblin-vm/src/compiler.rs:841, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573, crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Judge
  interpreter: crates/goblin-interpreter/src/lib.rs:18385
  vm: crates/goblin-vm/src/compiler.rs:182, crates/goblin-vm/src/compiler.rs:518, crates/goblin-vm/src/compiler.rs:989, crates/goblin-vm/src/compiler.rs:126, crates/goblin-vm/src/compiler.rs:135, crates/goblin-vm/src/compiler.rs:527, crates/goblin-vm/src/compiler.rs:681
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::LiteralToken
  interpreter: crates/goblin-interpreter/src/lib.rs:17945
  vm: crates/goblin-vm/src/compiler.rs:1061, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Member
  interpreter: crates/goblin-interpreter/src/lib.rs:18865
  vm: crates/goblin-vm/src/compiler.rs:847, crates/goblin-vm/src/compiler.rs:1148, crates/goblin-vm/src/compiler.rs:1150, crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142, crates/goblin-vm/src/compiler.rs:1165, crates/goblin-vm/src/opcode.rs:248, crates/goblin-vm/src/vm.rs:507
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Nil
  interpreter: crates/goblin-interpreter/src/lib.rs:17888
  vm: crates/goblin-vm/src/compiler.rs:755, crates/goblin-vm/src/vm.rs:1782, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:756, crates/goblin-vm/src/opcode.rs:204, crates/goblin-vm/src/vm.rs:166, crates/goblin-vm/src/compiler.rs:757, crates/goblin-vm/src/opcode.rs:205, crates/goblin-vm/src/vm.rs:169, crates/goblin-vm/src/vm.rs:2008
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::NsCall
  interpreter: crates/goblin-interpreter/src/lib.rs:18075
  vm: crates/goblin-vm/src/compiler.rs:888, crates/goblin-vm/src/compiler.rs:1299, crates/goblin-vm/src/compiler.rs:1355, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020, crates/goblin-vm/src/compiler.rs:718, crates/goblin-vm/src/compiler.rs:918, crates/goblin-vm/src/compiler.rs:940, crates/goblin-vm/src/compiler.rs:1231, crates/goblin-vm/src/compiler.rs:126, crates/goblin-vm/src/compiler.rs:135, crates/goblin-vm/src/compiler.rs:527, crates/goblin-vm/src/compiler.rs:681, crates/goblin-vm/src/compiler.rs:684, crates/goblin-vm/src/compiler.rs:928
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Number
  interpreter: crates/goblin-interpreter/src/lib.rs:17890
  vm: crates/goblin-vm/src/compiler.rs:759, crates/goblin-vm/src/vm.rs:1784
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Object
  interpreter: crates/goblin-interpreter/src/lib.rs:18482
  vm: crates/goblin-vm/src/compiler.rs:825, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:500, crates/goblin-vm/src/compiler.rs:831, crates/goblin-vm/src/compiler.rs:1053, crates/goblin-vm/src/debug.rs:62, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::OptCall
  interpreter: crates/goblin-interpreter/src/lib.rs:20389
  vm: crates/goblin-vm/src/compiler.rs:912, crates/goblin-vm/src/compiler.rs:549, crates/goblin-vm/src/compiler.rs:915, crates/goblin-vm/src/compiler.rs:937, crates/goblin-vm/src/compiler.rs:1172, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:718, crates/goblin-vm/src/compiler.rs:918, crates/goblin-vm/src/compiler.rs:940, crates/goblin-vm/src/compiler.rs:1231, crates/goblin-vm/src/compiler.rs:126, crates/goblin-vm/src/compiler.rs:135, crates/goblin-vm/src/compiler.rs:919, crates/goblin-vm/src/compiler.rs:941, crates/goblin-vm/src/compiler.rs:447, crates/goblin-vm/src/compiler.rs:665, crates/goblin-vm/src/compiler.rs:921, crates/goblin-vm/src/compiler.rs:931, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:866, crates/goblin-vm/src/compiler.rs:878, crates/goblin-vm/src/compiler.rs:907, crates/goblin-vm/src/compiler.rs:927, crates/goblin-vm/src/compiler.rs:684, crates/goblin-vm/src/compiler.rs:928
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::OptMember
  interpreter: crates/goblin-interpreter/src/lib.rs:19062
  vm: crates/goblin-vm/src/compiler.rs:935, crates/goblin-vm/src/compiler.rs:549, crates/goblin-vm/src/compiler.rs:915, crates/goblin-vm/src/compiler.rs:937, crates/goblin-vm/src/compiler.rs:1172, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:718, crates/goblin-vm/src/compiler.rs:918, crates/goblin-vm/src/compiler.rs:940, crates/goblin-vm/src/compiler.rs:1231, crates/goblin-vm/src/compiler.rs:126, crates/goblin-vm/src/compiler.rs:135, crates/goblin-vm/src/compiler.rs:919, crates/goblin-vm/src/compiler.rs:941, crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142, crates/goblin-vm/src/compiler.rs:684, crates/goblin-vm/src/compiler.rs:928, crates/goblin-vm/src/compiler.rs:447, crates/goblin-vm/src/compiler.rs:665, crates/goblin-vm/src/compiler.rs:921, crates/goblin-vm/src/compiler.rs:931, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Postfix
  interpreter: crates/goblin-interpreter/src/lib.rs:20475
  vm: crates/goblin-vm/src/compiler.rs:967, crates/goblin-vm/src/compiler.rs:970, crates/goblin-vm/src/opcode.rs:255, crates/goblin-vm/src/vm.rs:810, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020, crates/goblin-vm/src/compiler.rs:982, crates/goblin-vm/src/compiler.rs:1225, crates/goblin-vm/src/compiler.rs:1590, crates/goblin-vm/src/compiler.rs:1660, crates/goblin-vm/src/compiler.rs:1227, crates/goblin-vm/src/opcode.rs:216, crates/goblin-vm/src/vm.rs:291
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Prefix
  interpreter: crates/goblin-interpreter/src/lib.rs:20411
  vm: crates/goblin-vm/src/compiler.rs:952, crates/goblin-vm/src/compiler.rs:955, crates/goblin-vm/src/opcode.rs:220, crates/goblin-vm/src/vm.rs:375, crates/goblin-vm/src/compiler.rs:956, crates/goblin-vm/src/compiler.rs:1456, crates/goblin-vm/src/opcode.rs:239, crates/goblin-vm/src/vm.rs:433, crates/goblin-vm/src/compiler.rs:970, crates/goblin-vm/src/opcode.rs:255, crates/goblin-vm/src/vm.rs:810
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Slice
  interpreter: crates/goblin-interpreter/src/lib.rs:18721
  vm: crates/goblin-vm/src/compiler.rs:1016, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Slice3
  interpreter: crates/goblin-interpreter/src/lib.rs:18776
  vm: crates/goblin-vm/src/compiler.rs:1024, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:898, crates/goblin-vm/src/compiler.rs:975, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1020
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Expr::Str
  interpreter: crates/goblin-interpreter/src/lib.rs:17926
  vm: crates/goblin-vm/src/compiler.rs:799, crates/goblin-vm/src/compiler.rs:1536, crates/goblin-vm/src/compiler.rs:1618, crates/goblin-vm/src/compiler.rs:1789, crates/goblin-vm/src/compiler.rs:802, crates/goblin-vm/src/opcode.rs:273, crates/goblin-vm/src/vm.rs:1099, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:255, crates/goblin-vm/src/compiler.rs:288, crates/goblin-vm/src/compiler.rs:428, crates/goblin-vm/src/compiler.rs:466, crates/goblin-vm/src/compiler.rs:372, crates/goblin-vm/src/compiler.rs:715, crates/goblin-vm/src/compiler.rs:729, crates/goblin-vm/src/compiler.rs:734
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: member access
  interpreter: crates/goblin-interpreter/src/lib.rs:18865
  vm: crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142, crates/goblin-vm/src/debug.rs:63, crates/goblin-vm/src/debug.rs:67, crates/goblin-vm/src/opcode.rs:108, crates/goblin-vm/src/opcode.rs:247, crates/goblin-vm/src/vm.rs:623, crates/goblin-vm/src/vm.rs:630
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: object instantiation
  interpreter: crates/goblin-interpreter/src/lib.rs:22098
  vm: crates/goblin-vm/src/builtins.rs:2387, crates/goblin-vm/src/builtins.rs:2406, crates/goblin-vm/src/builtins.rs:3337, crates/goblin-vm/src/builtins.rs:3338, crates/goblin-vm/src/builtins.rs:3404, crates/goblin-vm/src/builtins.rs:3419, crates/goblin-vm/src/builtins.rs:3424, crates/goblin-vm/src/builtins.rs:3425, crates/goblin-vm/src/builtins.rs:3435, crates/goblin-vm/src/builtins.rs:4204
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: object method dispatch
  interpreter: crates/goblin-interpreter/src/lib.rs:21825
  vm: crates/goblin-vm/src/compiler.rs:873, crates/goblin-vm/src/compiler.rs:884, crates/goblin-vm/src/opcode.rs:115, crates/goblin-vm/src/opcode.rs:250, crates/goblin-vm/src/vm.rs:580, crates/goblin-vm/src/vm.rs:585, crates/goblin-vm/src/vm.rs:630, crates/goblin-vm/src/vm.rs:1794
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: object method dispatch with AST args
  interpreter: crates/goblin-interpreter/src/lib.rs:21967
  vm: crates/goblin-vm/src/compiler.rs:873, crates/goblin-vm/src/compiler.rs:884, crates/goblin-vm/src/opcode.rs:115, crates/goblin-vm/src/opcode.rs:250, crates/goblin-vm/src/vm.rs:580, crates/goblin-vm/src/vm.rs:585, crates/goblin-vm/src/vm.rs:630, crates/goblin-vm/src/vm.rs:1794
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: optional member access
  interpreter: crates/goblin-interpreter/src/lib.rs:19062
  vm: crates/goblin-vm/src/compiler.rs:850, crates/goblin-vm/src/compiler.rs:935, crates/goblin-vm/src/compiler.rs:943, crates/goblin-vm/src/compiler.rs:1107, crates/goblin-vm/src/compiler.rs:1142, crates/goblin-vm/src/debug.rs:63, crates/goblin-vm/src/debug.rs:67, crates/goblin-vm/src/opcode.rs:108, crates/goblin-vm/src/opcode.rs:247, crates/goblin-vm/src/vm.rs:623
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator !
  interpreter: crates/goblin-interpreter/src/lib.rs:20442
  vm: crates/goblin-vm/src/compiler.rs:956
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator !=
  interpreter: crates/goblin-interpreter/src/lib.rs:21500
  vm: crates/goblin-vm/src/compiler.rs:1232, crates/goblin-vm/src/opcode.rs:234, crates/goblin-vm/src/vm.rs:409
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator !==
  interpreter: crates/goblin-interpreter/src/lib.rs:21491
  vm: crates/goblin-vm/src/compiler.rs:1232, crates/goblin-vm/src/opcode.rs:234, crates/goblin-vm/src/vm.rs:409
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator %
  interpreter: crates/goblin-interpreter/src/lib.rs:20562
  vm: crates/goblin-vm/src/compiler.rs:970, crates/goblin-vm/src/compiler.rs:1230, crates/goblin-vm/src/opcode.rs:219, crates/goblin-vm/src/vm.rs:357, crates/goblin-vm/src/vm.rs:1266
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator %o
  interpreter: crates/goblin-interpreter/src/lib.rs:21443
  vm: crates/goblin-vm/src/compiler.rs:1252, crates/goblin-vm/src/compiler.rs:1228, crates/goblin-vm/src/opcode.rs:217, crates/goblin-vm/src/vm.rs:305, crates/goblin-vm/src/vm.rs:1262, crates/goblin-vm/src/vm.rs:1263
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator &&
  interpreter: crates/goblin-interpreter/src/lib.rs:21582
  vm: crates/goblin-vm/src/compiler.rs:1191
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator *
  interpreter: crates/goblin-interpreter/src/lib.rs:20966
  vm: crates/goblin-vm/src/compiler.rs:1228, crates/goblin-vm/src/compiler.rs:1252, crates/goblin-vm/src/opcode.rs:217, crates/goblin-vm/src/vm.rs:305, crates/goblin-vm/src/vm.rs:1262, crates/goblin-vm/src/vm.rs:1263
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator **
  interpreter: crates/goblin-interpreter/src/lib.rs:20566
  vm: crates/goblin-vm/src/compiler.rs:971, crates/goblin-vm/src/compiler.rs:1246
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator +
  interpreter: crates/goblin-interpreter/src/lib.rs:20429
  vm: crates/goblin-vm/src/compiler.rs:1225, crates/goblin-vm/src/vm.rs:274, crates/goblin-vm/src/compiler.rs:982, crates/goblin-vm/src/compiler.rs:1590, crates/goblin-vm/src/compiler.rs:1660, crates/goblin-vm/src/compiler.rs:1765, crates/goblin-vm/src/opcode.rs:215
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator ++
  interpreter: crates/goblin-interpreter/src/lib.rs:20479
  vm: crates/goblin-vm/src/compiler.rs:978, crates/goblin-vm/src/compiler.rs:982, crates/goblin-vm/src/compiler.rs:1226, crates/goblin-vm/src/opcode.rs:232, crates/goblin-vm/src/vm.rs:394, crates/goblin-vm/src/vm.rs:1259
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator -
  interpreter: crates/goblin-interpreter/src/lib.rs:20413
  vm: crates/goblin-vm/src/builtins.rs:4331, crates/goblin-vm/src/compiler.rs:955, crates/goblin-vm/src/compiler.rs:982, crates/goblin-vm/src/compiler.rs:1227, crates/goblin-vm/src/opcode.rs:216, crates/goblin-vm/src/vm.rs:291, crates/goblin-vm/src/vm.rs:1260, crates/goblin-vm/src/vm.rs:1261
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator ..
  interpreter: crates/goblin-interpreter/src/lib.rs:21778
  vm: crates/goblin-vm/src/builtins.rs:3068, crates/goblin-vm/src/builtins.rs:3161
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator /
  interpreter: crates/goblin-interpreter/src/lib.rs:21006
  vm: crates/goblin-vm/src/builtins.rs:3027, crates/goblin-vm/src/builtins.rs:3049, crates/goblin-vm/src/compiler.rs:1229, crates/goblin-vm/src/compiler.rs:1240, crates/goblin-vm/src/opcode.rs:218, crates/goblin-vm/src/vm.rs:319, crates/goblin-vm/src/vm.rs:1264, crates/goblin-vm/src/vm.rs:1265
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator //
  interpreter: crates/goblin-interpreter/src/lib.rs:20567
  vm: crates/goblin-vm/src/builtins.rs:3184, crates/goblin-vm/src/compiler.rs:977, crates/goblin-vm/src/compiler.rs:1229, crates/goblin-vm/src/compiler.rs:1240, crates/goblin-vm/src/opcode.rs:218, crates/goblin-vm/src/vm.rs:319, crates/goblin-vm/src/vm.rs:1264, crates/goblin-vm/src/vm.rs:1265
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator /=
  interpreter: crates/goblin-interpreter/src/lib.rs:21674
  vm: crates/goblin-vm/src/compiler.rs:1232, crates/goblin-vm/src/opcode.rs:234, crates/goblin-vm/src/vm.rs:409
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator <
  interpreter: crates/goblin-interpreter/src/lib.rs:21523
  vm: crates/goblin-vm/src/compiler.rs:1233, crates/goblin-vm/src/vm.rs:415, crates/goblin-vm/src/compiler.rs:1565, crates/goblin-vm/src/compiler.rs:1643, crates/goblin-vm/src/compiler.rs:1754, crates/goblin-vm/src/opcode.rs:235, crates/goblin-vm/src/vm.rs:413
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator <=
  interpreter: crates/goblin-interpreter/src/lib.rs:21523
  vm: crates/goblin-vm/src/compiler.rs:1234, crates/goblin-vm/src/vm.rs:420, crates/goblin-vm/src/compiler.rs:737, crates/goblin-vm/src/opcode.rs:236, crates/goblin-vm/src/vm.rs:418
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator ==
  interpreter: crates/goblin-interpreter/src/lib.rs:21465
  vm: crates/goblin-vm/src/compiler.rs:1231, crates/goblin-vm/src/compiler.rs:718, crates/goblin-vm/src/compiler.rs:918, crates/goblin-vm/src/compiler.rs:940, crates/goblin-vm/src/compiler.rs:1307, crates/goblin-vm/src/compiler.rs:1359
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator ===
  interpreter: crates/goblin-interpreter/src/lib.rs:21456
  vm: crates/goblin-vm/src/compiler.rs:1231, crates/goblin-vm/src/compiler.rs:718, crates/goblin-vm/src/compiler.rs:918, crates/goblin-vm/src/compiler.rs:940, crates/goblin-vm/src/compiler.rs:1307, crates/goblin-vm/src/compiler.rs:1359
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator >
  interpreter: crates/goblin-interpreter/src/lib.rs:21523
  vm: crates/goblin-vm/src/compiler.rs:1235, crates/goblin-vm/src/vm.rs:425, crates/goblin-vm/src/opcode.rs:237, crates/goblin-vm/src/vm.rs:423
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator >=
  interpreter: crates/goblin-interpreter/src/lib.rs:21523
  vm: crates/goblin-vm/src/compiler.rs:1236, crates/goblin-vm/src/vm.rs:430, crates/goblin-vm/src/compiler.rs:732, crates/goblin-vm/src/opcode.rs:238, crates/goblin-vm/src/vm.rs:428
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator ??
  interpreter: crates/goblin-interpreter/src/lib.rs:21658
  vm: crates/goblin-vm/src/compiler.rs:1211
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: operator of
  interpreter: crates/goblin-interpreter/src/lib.rs:21426
  vm: crates/goblin-vm/src/compiler.rs:1252, crates/goblin-vm/src/compiler.rs:1228, crates/goblin-vm/src/opcode.rs:217, crates/goblin-vm/src/vm.rs:305, crates/goblin-vm/src/vm.rs:1262, crates/goblin-vm/src/vm.rs:1263
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Action
  interpreter: crates/goblin-interpreter/src/lib.rs:3435
  vm: crates/goblin-vm/src/compiler.rs:178, crates/goblin-vm/src/compiler.rs:505, crates/goblin-vm/src/vm.rs:1479, crates/goblin-vm/src/compiler.rs:511, crates/goblin-vm/src/opcode.rs:272, crates/goblin-vm/src/vm.rs:1081, crates/goblin-vm/src/compiler.rs:255, crates/goblin-vm/src/compiler.rs:288, crates/goblin-vm/src/compiler.rs:428, crates/goblin-vm/src/compiler.rs:466
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Bind
  interpreter: crates/goblin-interpreter/src/lib.rs:5656
  vm: crates/goblin-vm/src/compiler.rs:169, crates/goblin-vm/src/compiler.rs:299, crates/goblin-vm/src/compiler.rs:450, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:263, crates/goblin-vm/src/compiler.rs:317, crates/goblin-vm/src/compiler.rs:322, crates/goblin-vm/src/compiler.rs:502
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Block
  interpreter: crates/goblin-interpreter/src/lib.rs:4214
  vm: crates/goblin-vm/src/compiler.rs:181, crates/goblin-vm/src/compiler.rs:536, crates/goblin-vm/src/compiler.rs:994, crates/goblin-vm/src/compiler.rs:549, crates/goblin-vm/src/compiler.rs:915, crates/goblin-vm/src/compiler.rs:937, crates/goblin-vm/src/compiler.rs:1172, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::BoxBind
  interpreter: crates/goblin-interpreter/src/lib.rs:5510
  vm: crates/goblin-vm/src/compiler.rs:652, crates/goblin-vm/src/compiler.rs:447, crates/goblin-vm/src/compiler.rs:665, crates/goblin-vm/src/compiler.rs:921, crates/goblin-vm/src/compiler.rs:931
  status: MISSING
  note: compiler explicitly rejects box bind statements at crates/goblin-vm/src/compiler.rs:645
* name: Stmt::Class
  interpreter: crates/goblin-interpreter/src/lib.rs:3866
  vm: crates/goblin-vm/src/compiler.rs:562
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::ClearLink
  interpreter: crates/goblin-interpreter/src/lib.rs:4194
  vm: crates/goblin-vm/src/compiler.rs:638, crates/goblin-vm/src/compiler.rs:639, crates/goblin-vm/src/opcode.rs:269, crates/goblin-vm/src/vm.rs:1067, crates/goblin-vm/src/compiler.rs:642, crates/goblin-vm/src/opcode.rs:270, crates/goblin-vm/src/vm.rs:1073, crates/goblin-vm/src/compiler.rs:648, crates/goblin-vm/src/opcode.rs:271, crates/goblin-vm/src/vm.rs:1077
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Enum
  interpreter: crates/goblin-interpreter/src/lib.rs:3928
  vm: crates/goblin-vm/src/compiler.rs:565, crates/goblin-vm/src/compiler.rs:581, crates/goblin-vm/src/compiler.rs:587, crates/goblin-vm/src/compiler.rs:601, crates/goblin-vm/src/opcode.rs:262
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Expr
  interpreter: crates/goblin-interpreter/src/lib.rs:3433
  vm: crates/goblin-vm/src/compiler.rs:295, crates/goblin-vm/src/compiler.rs:445, crates/goblin-vm/src/compiler.rs:1003, crates/goblin-vm/src/tick.rs:1061, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:447, crates/goblin-vm/src/compiler.rs:665, crates/goblin-vm/src/compiler.rs:921, crates/goblin-vm/src/compiler.rs:931, crates/goblin-vm/src/compiler.rs:456, crates/goblin-vm/src/opcode.rs:249, crates/goblin-vm/src/vm.rs:535
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Import
  interpreter: crates/goblin-interpreter/src/lib.rs:4222
  vm: crates/goblin-vm/src/compiler.rs:570, crates/goblin-vm/src/compiler.rs:581, crates/goblin-vm/src/compiler.rs:587, crates/goblin-vm/src/compiler.rs:601, crates/goblin-vm/src/opcode.rs:262
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Judge
  interpreter: crates/goblin-interpreter/src/lib.rs:5396
  vm: crates/goblin-vm/src/compiler.rs:182, crates/goblin-vm/src/compiler.rs:518, crates/goblin-vm/src/compiler.rs:989, crates/goblin-vm/src/compiler.rs:126, crates/goblin-vm/src/compiler.rs:135, crates/goblin-vm/src/compiler.rs:527, crates/goblin-vm/src/compiler.rs:681
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::JudgeAll
  interpreter: crates/goblin-interpreter/src/lib.rs:5451
  vm: crates/goblin-vm/src/compiler.rs:189, crates/goblin-vm/src/compiler.rs:522, crates/goblin-vm/src/compiler.rs:126, crates/goblin-vm/src/compiler.rs:135, crates/goblin-vm/src/compiler.rs:527, crates/goblin-vm/src/compiler.rs:681
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::LinkDef
  interpreter: crates/goblin-interpreter/src/lib.rs:4125
  vm: crates/goblin-vm/src/compiler.rs:629, crates/goblin-vm/src/compiler.rs:630, crates/goblin-vm/src/opcode.rs:266, crates/goblin-vm/src/vm.rs:1024, crates/goblin-vm/src/compiler.rs:633, crates/goblin-vm/src/opcode.rs:267, crates/goblin-vm/src/vm.rs:1037, crates/goblin-vm/src/compiler.rs:636, crates/goblin-vm/src/opcode.rs:268, crates/goblin-vm/src/vm.rs:1050, crates/goblin-vm/src/compiler.rs:639, crates/goblin-vm/src/opcode.rs:269, crates/goblin-vm/src/vm.rs:1067, crates/goblin-vm/src/compiler.rs:642, crates/goblin-vm/src/opcode.rs:270, crates/goblin-vm/src/vm.rs:1073
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::LinkOffset
  interpreter: crates/goblin-interpreter/src/lib.rs:4163
  vm: crates/goblin-vm/src/compiler.rs:635, crates/goblin-vm/src/compiler.rs:636, crates/goblin-vm/src/opcode.rs:268, crates/goblin-vm/src/vm.rs:1050, crates/goblin-vm/src/compiler.rs:639, crates/goblin-vm/src/opcode.rs:269, crates/goblin-vm/src/vm.rs:1067, crates/goblin-vm/src/compiler.rs:642, crates/goblin-vm/src/opcode.rs:270, crates/goblin-vm/src/vm.rs:1073, crates/goblin-vm/src/compiler.rs:648, crates/goblin-vm/src/opcode.rs:271, crates/goblin-vm/src/vm.rs:1077
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::ObjectDecision
  interpreter: crates/goblin-interpreter/src/lib.rs:4154
  vm: crates/goblin-vm/src/compiler.rs:641, crates/goblin-vm/src/compiler.rs:642, crates/goblin-vm/src/opcode.rs:270, crates/goblin-vm/src/vm.rs:1073, crates/goblin-vm/src/compiler.rs:648, crates/goblin-vm/src/opcode.rs:271, crates/goblin-vm/src/vm.rs:1077
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::ObjectLinkDef
  interpreter: crates/goblin-interpreter/src/lib.rs:4140
  vm: crates/goblin-vm/src/compiler.rs:632, crates/goblin-vm/src/compiler.rs:633, crates/goblin-vm/src/opcode.rs:267, crates/goblin-vm/src/vm.rs:1037, crates/goblin-vm/src/compiler.rs:636, crates/goblin-vm/src/opcode.rs:268, crates/goblin-vm/src/vm.rs:1050, crates/goblin-vm/src/compiler.rs:639, crates/goblin-vm/src/opcode.rs:269, crates/goblin-vm/src/vm.rs:1067, crates/goblin-vm/src/compiler.rs:642, crates/goblin-vm/src/opcode.rs:270, crates/goblin-vm/src/vm.rs:1073, crates/goblin-vm/src/compiler.rs:648, crates/goblin-vm/src/opcode.rs:271, crates/goblin-vm/src/vm.rs:1077
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::OverlayApply
  interpreter: crates/goblin-interpreter/src/lib.rs:3981
  vm: crates/goblin-vm/src/compiler.rs:608, crates/goblin-vm/src/compiler.rs:614, crates/goblin-vm/src/opcode.rs:264, crates/goblin-vm/src/vm.rs:980
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::OverlayDef
  interpreter: crates/goblin-interpreter/src/lib.rs:3935
  vm: crates/goblin-vm/src/compiler.rs:605, crates/goblin-vm/src/compiler.rs:606, crates/goblin-vm/src/opcode.rs:263, crates/goblin-vm/src/vm.rs:958, crates/goblin-vm/src/compiler.rs:614, crates/goblin-vm/src/opcode.rs:264, crates/goblin-vm/src/vm.rs:980
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::OverlayDetach
  interpreter: crates/goblin-interpreter/src/lib.rs:4087
  vm: crates/goblin-vm/src/compiler.rs:621, crates/goblin-vm/src/compiler.rs:627, crates/goblin-vm/src/opcode.rs:265, crates/goblin-vm/src/vm.rs:1017, crates/goblin-vm/src/compiler.rs:630, crates/goblin-vm/src/opcode.rs:266, crates/goblin-vm/src/vm.rs:1024, crates/goblin-vm/src/compiler.rs:633, crates/goblin-vm/src/opcode.rs:267, crates/goblin-vm/src/vm.rs:1037, crates/goblin-vm/src/compiler.rs:636, crates/goblin-vm/src/opcode.rs:268, crates/goblin-vm/src/vm.rs:1050, crates/goblin-vm/src/compiler.rs:639, crates/goblin-vm/src/opcode.rs:269, crates/goblin-vm/src/vm.rs:1067
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Return
  interpreter: crates/goblin-interpreter/src/lib.rs:4825
  vm: crates/goblin-vm/src/compiler.rs:482, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:500, crates/goblin-vm/src/compiler.rs:831, crates/goblin-vm/src/compiler.rs:1053, crates/goblin-vm/src/debug.rs:62
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Sweep
  interpreter: crates/goblin-interpreter/src/lib.rs:4876
  vm: crates/goblin-vm/src/compiler.rs:196, crates/goblin-vm/src/compiler.rs:540, crates/goblin-vm/src/compiler.rs:549, crates/goblin-vm/src/compiler.rs:915, crates/goblin-vm/src/compiler.rs:937, crates/goblin-vm/src/compiler.rs:1172, crates/goblin-vm/src/compiler.rs:397, crates/goblin-vm/src/compiler.rs:497, crates/goblin-vm/src/compiler.rs:553, crates/goblin-vm/src/compiler.rs:717, crates/goblin-vm/src/compiler.rs:554, crates/goblin-vm/src/compiler.rs:838, crates/goblin-vm/src/compiler.rs:844, crates/goblin-vm/src/compiler.rs:1573, crates/goblin-vm/src/compiler.rs:255, crates/goblin-vm/src/compiler.rs:288, crates/goblin-vm/src/compiler.rs:428, crates/goblin-vm/src/compiler.rs:466
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::TupleBind
  interpreter: crates/goblin-interpreter/src/lib.rs:3444
  vm: crates/goblin-vm/src/compiler.rs:173, crates/goblin-vm/src/compiler.rs:304, crates/goblin-vm/src/compiler.rs:544, crates/goblin-vm/src/compiler.rs:254, crates/goblin-vm/src/compiler.rs:262, crates/goblin-vm/src/compiler.rs:287, crates/goblin-vm/src/compiler.rs:310, crates/goblin-vm/src/compiler.rs:263, crates/goblin-vm/src/compiler.rs:317, crates/goblin-vm/src/compiler.rs:322, crates/goblin-vm/src/compiler.rs:502
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::UnitDecl
  interpreter: crates/goblin-interpreter/src/lib.rs:4209
  vm: crates/goblin-vm/src/compiler.rs:647, crates/goblin-vm/src/compiler.rs:648, crates/goblin-vm/src/opcode.rs:271, crates/goblin-vm/src/vm.rs:1077, crates/goblin-vm/src/compiler.rs:447, crates/goblin-vm/src/compiler.rs:665, crates/goblin-vm/src/compiler.rs:921, crates/goblin-vm/src/compiler.rs:931
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
* name: Stmt::Use
  interpreter: crates/goblin-interpreter/src/lib.rs:4340
  vm: crates/goblin-vm/src/compiler.rs:595, crates/goblin-vm/src/compiler.rs:581, crates/goblin-vm/src/compiler.rs:587, crates/goblin-vm/src/compiler.rs:601, crates/goblin-vm/src/opcode.rs:262, crates/goblin-vm/src/compiler.rs:606, crates/goblin-vm/src/opcode.rs:263, crates/goblin-vm/src/vm.rs:958
  status: PARTIAL
  note: VM implementation is traceable, but no cross-engine parity test exists
