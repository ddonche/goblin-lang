# Interpreter Behavior Inventory

Generated from every Rust source file under `crates/goblin-interpreter/src/**` on branch `claude/quirky-mendel-4df8rm`.
Interpreter source fingerprint: `3947b09b8e9344fdbc57477a9210ba102243ec882c2af2fc13e340565ecc9e5d` (18 files, 28690 lines).

The inventory excludes `money`, `db_query`, `db_exec`, and `db_query_one` from parity work by explicit user instruction.

## Source Manifest

- `crates/goblin-interpreter/src/actions/collections.rs:793` sha256 `5ce46ddea7cd514a34826be2f8a7626348596a07fc142c3d830c6ad7717e1d2d`
- `crates/goblin-interpreter/src/actions/csprng.rs:778` sha256 `1e2a6d25e1ed978a3acd2bcfe4903d6222f954a2c75853f8a50d0a9b9b7eda3c`
- `crates/goblin-interpreter/src/actions/db.rs:298` sha256 `8852cbf66789276d29e458f47da122bd482f725db06a3d1ca2f249d5464dee83`
- `crates/goblin-interpreter/src/actions/files.rs:725` sha256 `c3cc12e6084848036fc7fa76700a8fada6ce7f7e985ce0ae4ad9be5f8add6d33`
- `crates/goblin-interpreter/src/actions/grid.rs:696` sha256 `0bd1394974360ebcfa731eb2969da1f014acaa8505003ed8715950a36ae9ad6b`
- `crates/goblin-interpreter/src/actions/grid_store.rs:660` sha256 `1043e8b3886e86fbf08148241523994132ca899659a9606d4ecf68b8e635073e`
- `crates/goblin-interpreter/src/actions/ipsum.rs:284` sha256 `6d793d86a0704e2a400e18e2cafab0252e5f41c410bd1121990fe512f4827fcc`
- `crates/goblin-interpreter/src/actions/maps.rs:144` sha256 `cf318a19f1cf3ec3d654c0e7f2e59bb3602805b366fabdb6799c5af9b51fee7a`
- `crates/goblin-interpreter/src/actions/mem.rs:258` sha256 `80309347456aa10fac8ef7c47eaea746a2a9540d988edfd49cd3913c99979a77`
- `crates/goblin-interpreter/src/actions/mod.rs:14` sha256 `def8c2f8cceae9136803ed0c720244d25de5c2da3a82431cd2255e3bb970d556`
- `crates/goblin-interpreter/src/actions/process.rs:109` sha256 `0180c48e135065dc693d6d5b5b67ea0e736b9abd8632b1eb3e35935ae00f2aef`
- `crates/goblin-interpreter/src/actions/request.rs:149` sha256 `3e9bd950ebd015ffc3e1f0b9e886b1345f09ab3a9bf1f8c6577455a8696658f1`
- `crates/goblin-interpreter/src/actions/response.rs:164` sha256 `fc46730d845a005ac4060979b8107360074584982d4ff9d3079d3f4376ca0385`
- `crates/goblin-interpreter/src/actions/strings.rs:329` sha256 `f3d48278dbf166da2c6e92dafcee6f6f61cdcaf7f1e04660daf0ae7e5845dc96`
- `crates/goblin-interpreter/src/actions/utils.rs:152` sha256 `1dd0927093a0676fb3be35bae9caa82bccc603b2b672af538564e1c661812bb6`
- `crates/goblin-interpreter/src/diagnostics.rs:223` sha256 `09e085f5a6243714ca28a504927b0f17e40b06f351b91a28d821ccc2b68c307c`
- `crates/goblin-interpreter/src/lib.rs:22708` sha256 `051872e7c00f758eaa297d7e4f1123133e2e8f65406bda5a69fcc186585128b4`
- `crates/goblin-interpreter/src/modules.rs:206` sha256 `98650f427eefe731c4b21cff5c7d71629aa3a0fb48dc846061c8c43e622fe8cf`

## Behaviors

* name: abs
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12509
* name: after
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14769
* name: after_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14823
* name: append_file
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13976
* name: ask
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11159
* name: avg
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6399
* name: b
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12413
* name: backend
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15304
* name: basename
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13985
* name: before
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14742
* name: before_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14796
* name: between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14850
* name: big
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12413
* name: bool
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12429
* name: ceil
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12487
* name: chars
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14926
* name: clamp
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6596
* name: clear_all_tokens
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6083
* name: clear_format
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12349
* name: clear_token
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6047
* name: clear_tokens
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6059
* name: clone_object
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13923
* name: cookie
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13965
* name: copy_file
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13980
* name: count
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13785
* name: count_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15104
* name: create_dir
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13974
* name: decision_debug
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6725
* name: delete
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14135
* name: delete_all
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14161
* name: delete_at
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14150
* name: delete_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14172
* name: delete_first
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14140
* name: delete_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14145
* name: delete_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14166
* name: delete_path
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13977
* name: delete_random
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14179
* name: delete_where
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14155
* name: dirname
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13983
* name: dups
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13793
* name: ends_with
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:16259
* name: env
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13726
* name: escape_html
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13994
* name: ext
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13982
* name: f
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12409
* name: f32
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12441
* name: f64
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12442
* name: file_exists
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13973
* name: find
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13774
* name: find_all
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13775
* name: float
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12409
* name: floor
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12465
* name: format
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12150
* name: format_info
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12369
* name: freq
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13788
* name: get
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13999
* name: get_all
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14025
* name: get_at
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14014
* name: get_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14036
* name: get_first
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14004
* name: get_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14009
* name: get_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14030
* name: get_random
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14043
* name: get_where
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14019
* name: grid
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13811
* name: grid_count
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13822
* name: grid_default_get
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13829
* name: grid_default_set
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13830
* name: grid_get
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13814
* name: grid_has
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13824
* name: grid_info
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13831
* name: grid_neighbors
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13817
* name: grid_occupied
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13818
* name: grid_occupied_by
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13823
* name: grid_occupied_count
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13820
* name: grid_region_get
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13827
* name: grid_region_info
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13833
* name: grid_region_set
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13828
* name: grid_set
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13815
* name: grid_tile_get
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13825
* name: grid_tile_info
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13832
* name: grid_tile_set
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13826
* name: grid_unoccupied
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13819
* name: grid_unoccupied_count
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13821
* name: grid_void
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13816
* name: has
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13784
* name: highlight_code
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11233
* name: i
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12405
* name: i16
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12434
* name: i32
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12435
* name: i64
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12436
* name: i8
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12433
* name: ignore_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15576
* name: ignore_blocks
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15693
* name: ignore_blocks_first
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15829
* name: ignore_lines_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15505
* name: ignore_lines_where
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15421
* name: ignore_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15438
* name: ignore_where
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15413
* name: input
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11159
* name: int
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12405
* name: invoke
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11242
* name: is_alnum
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11985
* name: is_alpha
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11792
* name: is_array
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11719
* name: is_big
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11670
* name: is_bool
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11634
* name: is_bound_name
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11058
* name: is_char
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11710
* name: is_control
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11764
* name: is_digit
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11777
* name: is_dir
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13988
* name: is_even
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11822
* name: is_file
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13987
* name: is_float
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11656
* name: is_int
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11643
* name: is_map
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11728
* name: is_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15069
* name: is_multiple_of
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11886
* name: is_negative
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11967
* name: is_nil
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11625
* name: is_nix
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12014
* name: is_num
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11688
* name: is_odd
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11855
* name: is_pair
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11737
* name: is_pct
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11679
* name: is_positive
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11949
* name: is_seq
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11746
* name: is_str
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11701
* name: is_type
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11096
* name: is_unit
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11755
* name: is_whitespace
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11998
* name: items
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13781
* name: join
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14990
* name: json_parse
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14392
* name: json_stringify
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14426
* name: json_stringify_pretty
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14457
* name: keep_after
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:16075
* name: keep_before
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:16046
* name: keep_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:16110
* name: keep_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15979
* name: keys
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13779
* name: len
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15268
* name: lines
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14886
* name: link_score
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6768
* name: list_dirs
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13993
* name: list_tokens
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6092
* name: lower
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13766
* name: m
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12425
* name: map
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13791
* name: max
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6514
* name: md_to_html
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11225
* name: mem_addr
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13801
* name: mem_human
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13803
* name: mem_total
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13802
* name: metrics
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15338
* name: min
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6432
* name: minimize
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15217
* name: mixed
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13770
* name: mode
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13789
* name: normalize_newlines
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15399
* name: ord
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13776
* name: overlay_strength
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6757
* name: overlays_of
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6746
* name: owned_by
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13837
* name: owns_tree
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13865
* name: pack
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12086
* name: parse_bool
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15232
* name: path_join
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13984
* name: path_normalize
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13986
* name: path_relative_to
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13990
* name: path_split
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13989
* name: pathfind
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13991
* name: pct
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12421
* name: percent
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12421
* name: pick
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12671
* name: pow
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12531
* name: provoke
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11543
* name: put
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14048
* name: put_at
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14063
* name: put_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14074
* name: put_first
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14053
* name: put_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14058
* name: put_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14068
* name: put_random
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14081
* name: rand_seed
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13208
* name: raw
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13769
* name: read_json
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14502
* name: read_text
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13979
* name: reap
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14548
* name: reap_at
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14194
* name: reap_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14211
* name: reap_first
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14184
* name: reap_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14189
* name: reap_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14205
* name: reap_where
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14199
* name: register_token
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:5982
* name: req_body
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13963
* name: req_header
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13964
* name: req_method
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13960
* name: req_path
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13961
* name: req_query
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13962
* name: resolve_token
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:5991
* name: reverse
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15190
* name: reverse_chars
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15211
* name: roll
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13218
* name: roll_detail
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13423
* name: roll_detail_str
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13672
* name: roll_str
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13672
* name: round
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12443
* name: run_cmd
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13796
* name: sample_weighted
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13790
* name: sanitize_bom
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15379
* name: secure_pick
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13806
* name: secure_random
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13807
* name: secure_shuffle
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13808
* name: set_cookie
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13970
* name: set_header
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13969
* name: set_status
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13968
* name: shuffle
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13786
* name: slug
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13768
* name: sort
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13787
* name: split
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14945
* name: sqrt
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12618
* name: starts_with
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:16240
* name: stem
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13981
* name: str
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12417
* name: string
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12417
* name: sum
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6370
* name: summon
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11477
* name: tick
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6715
* name: tick_db
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:6715
* name: title
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13767
* name: to_map
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12425
* name: tokenize
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:15138
* name: trim
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13771
* name: trim_lead
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13772
* name: trim_trail
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13773
* name: u16
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12438
* name: u32
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12439
* name: u64
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12440
* name: u8
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12437
* name: unique
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13792
* name: unpack
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:12048
* name: update
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14086
* name: update_all
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14112
* name: update_at
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14101
* name: update_between
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14123
* name: update_first
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14091
* name: update_last
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14096
* name: update_matching
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14117
* name: update_random
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14130
* name: update_where
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14106
* name: upper
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13765
* name: uuid_v4
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13995
* name: uuid_v7
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13996
* name: valtype
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11594
* name: values
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13780
* name: vt
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:11594
* name: walk
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13992
* name: words
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14908
* name: write_json
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14488
* name: write_text
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13978
* name: yall_minify
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14372
* name: yall_parse
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14219
* name: yall_parse_file
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14258
* name: yall_pretty
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14353
* name: yall_write
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14296
* name: yall_write_file
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:14316
* name: zip_dir
  kind: builtin/free call
  source: crates/goblin-interpreter/src/lib.rs:13975
* name: Expr::Array
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18473
* name: Expr::Binary
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17721
* name: Expr::Block
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18027
* name: Expr::Bool
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17889
* name: Expr::BoxVar
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17973
* name: Expr::Call
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:20327
* name: Expr::Char
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17992
* name: Expr::EnumVariant
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18303
* name: Expr::FreeCall
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:19112
* name: Expr::Ident
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17993
* name: Expr::Index
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18492
* name: Expr::Index2
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17723
* name: Expr::IndexMap
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18580
* name: Expr::Judge
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18385
* name: Expr::LiteralToken
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17945
* name: Expr::Member
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18865
* name: Expr::Nil
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17888
* name: Expr::NsCall
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18075
* name: Expr::Number
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17890
* name: Expr::Object
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18482
* name: Expr::OptCall
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:20389
* name: Expr::OptMember
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:19062
* name: Expr::Postfix
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:20475
* name: Expr::Prefix
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:20411
* name: Expr::Slice
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18721
* name: Expr::Slice3
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:18776
* name: Expr::Str
  kind: expression
  source: crates/goblin-interpreter/src/lib.rs:17926
* name: member access
  kind: member/object runtime
  source: crates/goblin-interpreter/src/lib.rs:18865
* name: object instantiation
  kind: member/object runtime
  source: crates/goblin-interpreter/src/lib.rs:22098
* name: object method dispatch
  kind: member/object runtime
  source: crates/goblin-interpreter/src/lib.rs:21825
* name: object method dispatch with AST args
  kind: member/object runtime
  source: crates/goblin-interpreter/src/lib.rs:21967
* name: optional member access
  kind: member/object runtime
  source: crates/goblin-interpreter/src/lib.rs:19062
* name: operator !
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20442
* name: operator !=
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21500
* name: operator !==
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21491
* name: operator %
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20562
* name: operator %o
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21443
* name: operator &&
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21582
* name: operator *
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20966
* name: operator **
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20566
* name: operator +
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20429
* name: operator ++
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20479
* name: operator -
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20413
* name: operator ..
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21778
* name: operator /
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21006
* name: operator //
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:20567
* name: operator /=
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21674
* name: operator <
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21523
* name: operator <=
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21523
* name: operator ==
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21465
* name: operator ===
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21456
* name: operator >
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21523
* name: operator >=
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21523
* name: operator ??
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21658
* name: operator of
  kind: operator/runtime
  source: crates/goblin-interpreter/src/lib.rs:21426
* name: Stmt::Action
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3435
* name: Stmt::Bind
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:5656
* name: Stmt::Block
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4214
* name: Stmt::BoxBind
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:5510
* name: Stmt::Class
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3866
* name: Stmt::ClearLink
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4194
* name: Stmt::Enum
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3928
* name: Stmt::Expr
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3433
* name: Stmt::Import
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4222
* name: Stmt::Judge
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:5396
* name: Stmt::JudgeAll
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:5451
* name: Stmt::LinkDef
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4125
* name: Stmt::LinkOffset
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4163
* name: Stmt::ObjectDecision
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4154
* name: Stmt::ObjectLinkDef
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4140
* name: Stmt::OverlayApply
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3981
* name: Stmt::OverlayDef
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3935
* name: Stmt::OverlayDetach
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4087
* name: Stmt::Return
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4825
* name: Stmt::Sweep
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4876
* name: Stmt::TupleBind
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:3444
* name: Stmt::UnitDecl
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4209
* name: Stmt::Use
  kind: statement
  source: crates/goblin-interpreter/src/lib.rs:4340
