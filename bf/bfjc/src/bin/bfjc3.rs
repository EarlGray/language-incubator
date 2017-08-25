extern crate bfjc;
use bfjc::bf3;

fn main() {
    let mut contents = String::new();
    bfjc::read_file(&mut contents);

    let mut lang = bf3::Impl::new();
    bfjc::execute(&contents, &mut lang);
}
