use std::path::PathBuf;
use std::process::Command;

const OUTPUT: &str = "src/sys/mod.rs";

const PYTHON_CONFIG: &str = "python3-config";
const PYTHON_H: &str = "Python.h";
const CLANG: &str = "clang";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo:rerun-if-changed=build.rs");

    Command::new(CLANG).arg("--version").status()
        .map_err(|_| format!("{CLANG} is required by bindgen"))?;

    let python_includes = Command::new(PYTHON_CONFIG).arg("--includes").output()?;
    let python_includes = String::from_utf8(python_includes.stdout)?;
    let Some(python_header) = python_includes.split_whitespace().find_map(|param| {
        let Some(dir) = param.strip_prefix("-I") else { return None };
        let path = PathBuf::from_iter(&[dir, PYTHON_H]);
        path.exists().then_some(path)
    }) else {
        return Err(format!("{PYTHON_H} not found in {python_includes}").into());
    };
    let python_header = python_header.to_str()
        .ok_or_else(|| format!("{python_header:?} is not UTF-8"))?;
    println!("cargo:rerun-if-changed={}", python_header);

    let python_cflags = Command::new(PYTHON_CONFIG).arg("--cflags").output()?;
    let python_cflags = String::from_utf8(python_cflags.stdout)?;

    let bindings = bindgen::builder()
        .clang_args(python_cflags.split_ascii_whitespace())
        .header(python_header)
        .generate()?;
    bindings.write_to_file(OUTPUT)?;

    // TODO: output linker args, https://doc.rust-lang.org/cargo/reference/build-scripts.html
    Ok(())
}
