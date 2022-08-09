use std::fs;
use std::io;
use std::path::Path;
use std::process as proc;

use serde_json::Value as JSON;

const ESPRIMA: &str = "./node_modules/esprima/dist/esprima.js";
const ESJSON: &str = "./tmp/esprima.json";

fn decode_utf16(bytes: &[u8]) -> Result<String, io::Error> {
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

    let npm = if cfg!(target_os = "windows") {
        "npm.cmd"
    } else {
        "npm"
    };
    let esjson = Path::new(ESJSON);

    if let Some(outdir) = esjson.parent() {
        fs::create_dir_all(outdir)?;
    }

    let exitcode = proc::Command::new(npm).arg("install").status()?;
    println!("npm install: {}", exitcode);

    let status = proc::Command::new(npm)
        .args(["exec", "esparse", "--", /*"--loc",*/ ESPRIMA])
        .output()?;
    println!("npm exec esparse: {}", status.status);

    if cfg!(target_os = "windows") {
        // yes, Windows, I do hate you.
        let jsonstr = decode_utf16(&status.stdout)?;
        // TODO: minify here as well
        fs::write(esjson, jsonstr)?;
    } else {
        // Streaming Stdio::piped() into serde_json::from_reader() is
        // unexpectedly embarrassingly slow. Try BufReader?
        let json: JSON = serde_json::from_slice(&status.stdout)?;
        let jsonfile = fs::File::create(ESJSON)?;
        serde_json::to_writer(jsonfile, &json)?;
    }

    Ok(())
}
