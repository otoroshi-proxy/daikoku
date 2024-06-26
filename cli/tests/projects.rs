use assert_cmd::prelude::*;
use serial_test::serial;
use std::{fs, process::Command};

const WASMO_TEST_FOLDER: &str = "/tmp/daikokucli";
struct Setup {
    temporary_path: String,
}

impl Setup {
    fn new() -> Self {
        let temporary_path = WASMO_TEST_FOLDER.to_string();

        let _ = fs::remove_dir_all(&temporary_path);

        match fs::create_dir(&temporary_path) {
            Err(err) => println!("{:?}", err),
            Ok(v) => println!("{:?}", v),
        }

        let mut cmd = Command::cargo_bin("daikokucli").unwrap();

        cmd.args(["projects", "clear"]).assert().success();

        cmd = Command::cargo_bin("daikokucli").unwrap();

        cmd.args([
            "create",
            "--name=cms",
            "--template=empty",
            format!("--path={}", &temporary_path).as_str(),
        ]);

        Setup {
            temporary_path: temporary_path,
        }
    }

    fn clean(&self) {
        fs::remove_dir_all(&self.temporary_path).expect("Failed to remove folder")
    }
}

#[test]
#[serial]
fn add_project() -> Result<(), Box<dyn std::error::Error>> {
    let setup = Setup::new();

    let mut cmd = Command::cargo_bin("daikokucli")?;

    cmd.args([
        "projects",
        "add",
        "--name=dev",
        &format!("--path={}", WASMO_TEST_FOLDER),
    ])
    .assert()
    .success();

    setup.clean();
    Ok(())
}

#[test]
#[serial]
fn default_project() -> Result<(), Box<dyn std::error::Error>> {
    let setup = Setup::new();

    let mut cmd = Command::cargo_bin("daikokucli")?;

    cmd.args([
        "projects",
        "add",
        "--name=dev",
        &format!("--path={}", WASMO_TEST_FOLDER),
    ])
    .assert()
    .success();

    cmd = Command::cargo_bin("daikokucli")?;

    cmd.args(["projects", "default", "--name=dev"])
        .assert()
        .success();

    setup.clean();
    Ok(())
}

#[test]
#[serial]
fn remove_project() -> Result<(), Box<dyn std::error::Error>> {
    let setup = Setup::new();

    let mut cmd = Command::cargo_bin("daikokucli")?;

    cmd.args([
        "projects",
        "add",
        "--name=dev",
        &format!("--path={}", WASMO_TEST_FOLDER),
    ])
    .assert()
    .success();

    cmd = Command::cargo_bin("daikokucli")?;

    cmd.args(["projects", "remove", "--name=dev"])
        .assert()
        .success();

    setup.clean();
    Ok(())
}

#[test]
#[serial]
fn clear_project() -> Result<(), Box<dyn std::error::Error>> {
    let setup = Setup::new();

    let mut cmd = Command::cargo_bin("daikokucli")?;

    cmd.args([
        "projects",
        "add",
        "--name=dev",
        &format!("--path={}", WASMO_TEST_FOLDER),
    ])
    .assert()
    .success();

    cmd = Command::cargo_bin("daikokucli")?;

    cmd.args(["projects", "clear"]).assert().success();

    setup.clean();
    Ok(())
}
