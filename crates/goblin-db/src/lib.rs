use deadpool_postgres::{Config, Pool, Runtime};
use tokio_postgres::{Error, Row, NoTls};

pub struct DbPool {
    pool: Pool,
}

impl DbPool {
    pub async fn new(database_url: &str) -> Result<Self, Error> {
        let mut cfg = Config::new();
        cfg.url = Some(database_url.to_string());
        let pool = cfg.create_pool(Some(Runtime::Tokio1), NoTls)?;
        Ok(Self { pool })
    }
    
    pub async fn query(&self, sql: &str, params: &[&(dyn tokio_postgres::types::ToSql + Sync)]) -> Result<Vec<Row>, Error> {
        let client = self.pool.get().await?;
        let rows = client.query(sql, params).await?;
        Ok(rows)
    }
    
    pub async fn execute(&self, sql: &str, params: &[&(dyn tokio_postgres::types::ToSql + Sync)]) -> Result<u64, Error> {
        let client = self.pool.get().await?;
        let affected = client.execute(sql, params).await?;
        Ok(affected)
    }
}