use criterion::{
    criterion_group,
    criterion_main,
    Criterion,
};

use slothjs::runtime::{
    EsprimaParser,
    Runtime,
};

fn esprima_eval() {
    Runtime::<EsprimaParser>::load().expect("Runtime::load");
}

fn benchmark_esprimaparser(c: &mut Criterion) {
    c.bench_function("Runtime::<EsprimaParser>::load()", |b| {
        b.iter(|| esprima_eval())
    });
}

criterion_group!(benchmarks, benchmark_esprimaparser);
criterion_main!(benchmarks);
