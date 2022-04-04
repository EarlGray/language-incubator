use std::fs;
use std::io;
use std::path::Path;
use std::process::Command;

const ESPRIMA: &str = "./node_modules/esprima/dist/esprima.js";
const ESJSON: &str = "./tmp/esprima.json";

fn decode_utf16(bytes: &[u8]) -> Result<String, io::Error> {
    use std::convert::TryInto;
    let codepoints = bytes
        .chunks_exact(2)
        .map(|b| u16::from_le_bytes(b.try_into().unwrap()));
    char::decode_utf16(codepoints)
        .collect::<Result<String, _>>()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

fn main() -> Result<(), io::Error> {
    println!("cargo:rerun-if-changed={}", ESPRIMA);
    println!("cargo:rerun-if-changed={}", ESJSON);

    let npm = if cfg!(target_os = "windows") { "npm.cmd" } else { "npm" };
    let esjson = Path::new(ESJSON);

    if let Some(outdir) = esjson.parent() {
        fs::create_dir_all(outdir).unwrap();
    }

    let exitcode = Command::new(npm).arg("install").status().unwrap();
    println!("npm install: {}", exitcode);

    if cfg!(target_os = "windows") {
        let status = Command::new(npm)
            .args(["exec", "esparse", "--", ESPRIMA])
            .output().unwrap();
        // yes, Windows, I do hate you.
        let json = decode_utf16(&status.stdout).unwrap();
        fs::write(esjson, json).unwrap();
    } else {
        let esjson = fs::File::create(ESJSON)?;
        let exitcode = Command::new(npm)
            .args(["exec", "esparse", "--", ESPRIMA])
            .stdout(esjson)
            .status().unwrap();
        println!("esparse: {}", exitcode);
    }

    Ok(())
}
