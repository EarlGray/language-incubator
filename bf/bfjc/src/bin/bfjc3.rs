extern crate bfjc;
use bfjc::bf3;

fn main() {
    let mut contents = String::new();
    bfjc::read_file(&mut contents);

    bf3::run(&contents);
}
