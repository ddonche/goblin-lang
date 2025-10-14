//! goblin-devserver: thin wrapper around goblin-host for dev UX.
#[allow(unused_imports)]
use goblin_host::{HostBuilder, HostError};
use anyhow::Result;

#[derive(Debug, Clone)]
pub struct DevOptions {
    pub host: String,
    pub port: u16,
    pub proxies: Vec<(String, String)>,
}

impl Default for DevOptions {
    fn default() -> Self {
        Self { host: "0.0.0.0".into(), port: 5173, proxies: Vec::new() }
    }
}

/// Start a dev server with sensible defaults (wraps goblin-host).
/// Returns when the server shuts down (e.g., Ctrl+C).
pub async fn start(opts: DevOptions) -> anyhow::Result<()> {
    let mut b = HostBuilder::new().bind(opts.host, opts.port);
    for (prefix, target) in &opts.proxies {
        b = b.proxy(prefix.clone(), target.clone());
    }
    let mut host = b.build();
    host.run().await.map_err(|e| match e {
        HostError::Bind(s) => anyhow::anyhow!("bind failed: {s}"),
        HostError::Io(s) => anyhow::anyhow!("io: {s}"),
        HostError::Internal(s) => anyhow::anyhow!("internal: {s}"),
    })
}
