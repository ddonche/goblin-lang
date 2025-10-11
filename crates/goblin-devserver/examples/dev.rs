use goblin_devserver::{start, DevOptions};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    start(DevOptions::default()).await
}
