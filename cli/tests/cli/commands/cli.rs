use std::path::PathBuf;
use std::{ffi, fs};

use assert_cmd::{
    assert::{Assert, OutputAssertExt},
    cargo::CommandCargoExt,
};
use std::process::Command;

use testcontainers::{
    core::{wait::HealthWaitStrategy, IntoContainerPort, Mount, WaitFor},
    runners::AsyncRunner,
    ContainerAsync, GenericImage, ImageExt,
};

pub(crate) async fn run_test<T>(test: T) -> Result<(), Box<dyn std::error::Error + 'static>>
where
    T: FnOnce(CLI) -> (),
{
    let cli = CLI::start().await.unwrap();

    test(cli);

    Ok(())
}

pub(crate) async fn run_test_with_s3<T>(test: T) -> Result<(), Box<dyn std::error::Error + 'static>>
where
    T: FnOnce(CLI) -> (),
{
    let cli = CLI::start_with_s3().await.unwrap();

    test(cli);

    Ok(())
}

pub(crate) struct CLI {
    pub postgres_container: ContainerAsync<GenericImage>,
    pub daikoku_container: ContainerAsync<GenericImage>,
}

impl CLI {
    pub(crate) fn run<I, S>(args: I) -> Assert
    where
        I: IntoIterator<Item = S>,
        S: AsRef<ffi::OsStr>,
    {
        Command::cargo_bin("daikoku").unwrap().args(args).run()
    }

    pub(crate) fn build<I, S>(args: I) -> Assert
    where
        I: IntoIterator<Item = S>,
        S: AsRef<ffi::OsStr>,
    {
        Command::cargo_bin("daikoku").unwrap().args(args).assert()
    }

    pub(crate) async fn start() -> Result<CLI, Box<dyn std::error::Error + 'static>> {
        let (postgres_container, daikoku_container) = Self::start_containers().await?;

        Ok(CLI {
            postgres_container: postgres_container,
            daikoku_container: daikoku_container,
        })
    }

    pub(crate) async fn start_with_s3() -> Result<CLI, Box<dyn std::error::Error + 'static>> {
        let root_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        let content =
            fs::read_to_string(root_path.clone().join("tests/resources/.s3_env")).unwrap();

        let lines = content.split("\n").collect::<Vec<&str>>();

        let s3_bucket = lines
            .iter()
            .find(|line| line.contains("S3_BUCKET"))
            .unwrap()
            .replace("S3_BUCKET=", "");

        let s3_endpoint = lines
            .iter()
            .find(|line| line.contains("S3_ENDPOINT"))
            .unwrap()
            .replace("S3_ENDPOINT=", "");

        let s3_key = lines
            .iter()
            .find(|line| line.contains("S3_KEY"))
            .unwrap()
            .replace("S3_KEY=", "");

        let s3_secret = lines
            .iter()
            .find(|line| line.contains("S3_SECRET"))
            .unwrap()
            .replace("S3_SECRET=", "");

        let _ = fs::write(
            root_path
                .clone()
                .join("tests/resources/daikoku-state.ndjson"),
            fs::read_to_string(
                root_path
                    .clone()
                    .join("tests/resources/daikoku-state-template.ndjson"),
            )
            .unwrap()
            .replace("@@S3_ENDPOINT@@", s3_endpoint.trim())
            .replace("@@S3_KEY@@", s3_key.trim())
            .replace("@@S3_SECRET@@", s3_secret.trim())
            .replace("@@S3_BUCKET@@", s3_bucket.trim()),
        );

        let (postgres_container, daikoku_container) = Self::start_containers().await?;
        let cli = CLI {
            postgres_container: postgres_container,
            daikoku_container: daikoku_container,
        };
        Ok(cli)
    }

    pub(crate) async fn start_containers() -> Result<
        (ContainerAsync<GenericImage>, ContainerAsync<GenericImage>),
        Box<dyn std::error::Error + 'static>,
    > {
        let postgres = GenericImage::new("postgres", "13")
            .with_wait_for(WaitFor::message_on_stdout(
                "database system is ready to accept connections",
            ))
            .with_mapped_port(5432, 5432.tcp())
            .with_env_var("POSTGRES_USER", "postgres")
            .with_env_var("POSTGRES_PASSWORD", "postgres")
            .with_env_var("POSTGRES_DB", "daikoku");

        let postgres_container = postgres.start().await?;
        let host = postgres_container
            .get_bridge_ip_address()
            .await?
            .to_string();

        let mut state_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        state_path.push("tests/resources");

        let daikoku = GenericImage::new("maif/daikoku", "17.4.0-dev")
            .with_wait_for(WaitFor::message_on_stdout("Running missing evolutions"))
            .with_wait_for(WaitFor::seconds(5))
            .with_mapped_port(8080, 8080.tcp())
            .with_env_var("DAIKOKU_INIT_DATA_FROM", "/tmp/daikoku-state.ndjson")
            .with_env_var("DAIKOKU_POSTGRES_HOST", host)
            .with_env_var("DAIKOKU_MODE", "prod")
            .with_env_var("DAIKOKU_POSTGRES_DATABASE", "daikoku")
            .with_env_var("DAIKOKU_EXPOSED_ON", "8080")
            .with_mount(Mount::bind_mount(
                state_path.into_os_string().into_string().unwrap(),
                "/tmp",
            ));

        let daikoku_container = daikoku.start().await?;

        Ok((postgres_container, daikoku_container))
    }
}

pub trait AssertCommand {
    fn run(&mut self) -> Assert;
}

impl AssertCommand for Command {
    fn run(&mut self) -> Assert {
        self.assert().success()
    }
}

pub trait CustomRun {
    fn run_and_expect(&mut self, expected: &str);
    fn run_and_multiple_expect(&mut self, expected: Vec<&str>);
}

impl CustomRun for Assert {
    fn run_and_expect(&mut self, expected: &str) {
        self.run_and_multiple_expect(vec![expected]);
    }

    fn run_and_multiple_expect(&mut self, expected: Vec<&str>) {
        let stdout = String::from_utf8(self.get_output().stdout.clone()).unwrap();
        let stderr = String::from_utf8(self.get_output().stderr.clone()).unwrap();

        for expected_value in expected {
            assert!(stdout.contains(expected_value) || stderr.contains(expected_value));
        }
    }
}
