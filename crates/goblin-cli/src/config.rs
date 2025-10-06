use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub entry: String,
    #[serde(default)]
    pub module_paths: BTreeMap<String, String>,
}

impl ProjectConfig {
    pub fn load(path: &PathBuf) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read goblin.yaml: {}", e))?;
        
        serde_yaml::from_str(&content)
            .map_err(|e| format!("Failed to parse goblin.yaml: {}", e))
    }
    
    pub fn resolve_module_path(&self, import_path: &str) -> Option<PathBuf> {
        // import_path example: "game/hero"
        let parts: Vec<&str> = import_path.split('/').collect();
        if parts.is_empty() {
            return None;
        }
        
        let alias = parts[0];
        let module_root = self.module_paths.get(alias)?;
        
        // Build full path: ./game + /hero.gbln
        let mut path = PathBuf::from(module_root);
        for part in &parts[1..] {
            path.push(part);
        }
        path.set_extension("gbln");
        
        Some(path)
    }
}