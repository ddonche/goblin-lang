use std::collections::BTreeMap;
use goblin_ast as ast;

pub struct ModuleCache {
    loaded: BTreeMap<String, Module>,
}

pub struct Module {
    pub path: String,
    pub namespace: String,
    pub ast: ast::Module,
    pub exports: BTreeMap<String, ExportedItem>,
}

#[derive(Clone)]
pub enum ExportedItem {
    Action(ast::ActionDecl),
    Class(ast::ClassDecl),
    Enum(ast::EnumDecl),
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            loaded: BTreeMap::new(),
        }
    }
    
    pub fn load_module(&mut self, import_path: &str, alias: Option<&str>, base_dir: &std::path::Path) -> Result<String, String> {
        // Resolve namespace (alias or last part of path)
        let namespace = if let Some(a) = alias {
            a.to_string()
        } else {
            import_path.split('/').last().unwrap().to_string()
        };
        
        // Check for namespace collision
        if self.loaded.contains_key(&namespace) {
            return Err(format!(
                "Namespace collision: '{}' is already imported. Use 'import {} as other_name'",
                namespace, import_path
            ));
        }
        
        // For now, simple path resolution: base_dir/import_path.gbln
        let mut file_path = base_dir.to_path_buf();
        for part in import_path.split('/') {
            file_path.push(part);
        }
        file_path.set_extension("gbln");
        
        if !file_path.exists() {
            return Err(format!("Module file not found: {}", file_path.display()));
        }
        
        // Load and parse
        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| format!("Failed to read module: {}", e))?;
        
        let tokens = goblin_lexer::lex(&source, &file_path.to_string_lossy())
            .map_err(|diags| format!("Lex error in module '{}': {:?}", import_path, diags))?;
        
        let parser = goblin_parser::Parser::new(&tokens);
        let module_ast = parser.parse_module()
            .map_err(|diags| format!("Parse error in module '{}': {:?}", import_path, diags))?;
        
        // Extract exports (for now, export everything)
        let mut exports = BTreeMap::new();
        for stmt in &module_ast.items {
            match stmt {
                ast::Stmt::Action(decl) => {
                    exports.insert(decl.name.clone(), ExportedItem::Action(decl.clone()));
                }
                ast::Stmt::Class(decl) => {
                    exports.insert(decl.name.clone(), ExportedItem::Class(decl.clone()));
                }
                ast::Stmt::Enum(decl) => {
                    exports.insert(decl.name.clone(), ExportedItem::Enum(decl.clone()));
                }
                _ => {}
            }
        }
        
        let module = Module {
            path: import_path.to_string(),
            namespace: namespace.clone(),
            ast: module_ast,
            exports,
        };
        
        self.loaded.insert(namespace.clone(), module);
        
        Ok(namespace)
    }
    
    pub fn get_export(&self, namespace: &str, name: &str) -> Option<&ExportedItem> {
        self.loaded.get(namespace)?.exports.get(name)
    }
}