use goblin_host::HostBuilder;

#[tokio::main]
async fn main() {
    let mut host = HostBuilder::new().build();
    if let Err(e) = host.run().await {
        eprintln!("host error: {e}");
    }
}