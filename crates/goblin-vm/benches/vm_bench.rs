/// Microbenchmarks for the Goblin VM.
/// Run with: cargo bench -p goblin-vm
use std::hint::black_box;
use std::time::Instant;

use goblin_vm::exec::execute_source;

// ── Minimal hand-rolled benchmark harness (no external crate needed) ──────────

fn bench(name: &str, iters: u64, mut f: impl FnMut()) {
    // Warmup
    for _ in 0..iters / 10 {
        f();
    }
    let start = Instant::now();
    for _ in 0..iters {
        f();
    }
    let elapsed = start.elapsed();
    let ns_per_iter = elapsed.as_nanos() / iters as u128;
    println!("bench {name:<40} {:>8} ns/iter  ({iters} iters)", ns_per_iter);
}

fn main() {
    println!("=== Goblin VM Benchmarks ===\n");

    // ── Arithmetic ─────────────────────────────────────────────────────────────

    bench("int_add_literal", 100_000, || {
        let _ = black_box(execute_source("x | 1 + 2"));
    });

    bench("int_arith_loop", 10_000, || {
        let src = r#"
act sum_to
    n | 100
    s | 0
    each i from 1...n
        s |= s + i
    end
    s
xx
sum_to()
"#;
        let _ = black_box(execute_source(src));
    });

    bench("float_arith", 10_000, || {
        let _ = black_box(execute_source("x | 1.5 * 2.5 + 3.0 / 1.5"));
    });

    // ── Function calls ─────────────────────────────────────────────────────────

    bench("function_call_trivial", 50_000, || {
        let src = r#"
act id x
    x
xx
id(42)
"#;
        let _ = black_box(execute_source(src));
    });

    bench("function_call_recursive_fib", 1_000, || {
        let src = r#"
act fib n
    if n <= 1 => n
    else => fib(n - 1) + fib(n - 2)
    end
xx
fib(15)
"#;
        let _ = black_box(execute_source(src));
    });

    // ── Collections ────────────────────────────────────────────────────────────

    bench("make_array_10", 20_000, || {
        let _ = black_box(execute_source("x | [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"));
    });

    bench("collect_100", 5_000, || {
        let src = r#"
act build
    collect 100 i => i * 2
xx
build()
"#;
        let _ = black_box(execute_source(src));
    });

    bench("string_concat", 20_000, || {
        let _ = black_box(execute_source(r#"x | "hello" ++ " " ++ "world""#));
    });

    println!("\n=== done ===");
}
