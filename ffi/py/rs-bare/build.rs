use std::path::PathBuf;
use std::process::Command;

const PYTHON_CONFIG: &str = "python3-config";
const PYTHON_H: &str = "Python.h";
const CLANG: &str = "clang";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    Command::new(CLANG).arg("--version").status()
        .map_err(|_| format!("{CLANG} is required by bindgen"))?;

    let python_includes = Command::new(PYTHON_CONFIG).arg("--includes").output()?;
    let python_includes = String::from_utf8(python_includes.stdout)?;
    let Some(python_header) = python_includes.split_whitespace().find_map(|param| {
        let Some(dir) = param.strip_prefix("-I") else { return None };
        let path = PathBuf::from_iter(&[dir, PYTHON_H]);
        path.exists().then_some(path)
    }) else {
        return Err(format!("{} not found in {}", PYTHON_H, python_includes).into());
    };

    let python_cflags = Command::new(PYTHON_CONFIG).arg("--cflags").output()?;
    let python_cflags = String::from_utf8(python_cflags.stdout)?;

    let bindings = bindgen::builder()
        .clang_args(python_cflags.split_ascii_whitespace())
        .header(python_header.to_str().unwrap())
        .generate()?;
    bindings.write_to_file("src/sys/mod.rs")?;
    Ok(())
}
