use std::collections::BTreeMap;
use goblin_ast as ast;
use crate::Value;

pub mod markdown {
    use comrak::{
        markdown_to_html, ComrakExtensionOptions, ComrakOptions, ComrakParseOptions, ComrakRenderOptions,
    };

    pub fn md_to_html(md: &str) -> String {
        let mut options = ComrakOptions::default();

        // Extensions
        options.extension.table = true;
        options.extension.autolink = true;
        options.extension.tasklist = true;
        options.extension.strikethrough = true;
        options.extension.superscript = true;
        options.extension.footnotes = true;
        // options.extension.tagfilter = false; // leave default unless you want HTML filtering

        // Parse
        options.parse.smart = true;

        // Render
        options.render.unsafe_ = false;       // set true to allow raw HTML passthrough
        options.render.hardbreaks = false;
        options.render.github_pre_lang = true;

        markdown_to_html(md, &options)
    }
}

pub struct ModuleCache {
    loaded: BTreeMap<String, Module>,
}

pub struct Module {
    pub path: String,
    pub namespace: String,
    pub ast: ast::Module,
    pub exports: BTreeMap<String, ExportedItem>,
    pub env: BTreeMap<String, Value>,
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
    
    pub fn load_module(
        &mut self,
        import_path: &str,
        alias: Option<&str>,
        base_dir: &std::path::Path,
    ) -> Result<(String, Option<ast::Module>), String> {
        let namespace = if let Some(a) = alias {
            a.to_string()
        } else {
            import_path.split('/').last().unwrap().to_string()
        };

        if self.loaded.contains_key(&namespace) {
            return Ok((namespace, None));
        }

        let mut file_path = base_dir.to_path_buf();
        for part in import_path.split('/') {
            file_path.push(part);
        }
        file_path.set_extension("gbln");

        if !file_path.exists() {
            return Err(format!("Module file not found: {}", file_path.display()));
        }

        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| format!("Failed to read module: {}", e))?;

        let tokens = goblin_lexer::lex(&source, &file_path.to_string_lossy())
            .map_err(|diags| format!("Lex error in module '{}': {:?}", import_path, diags))?;

        let parser = goblin_parser::Parser::new(&tokens);
        let module_ast = parser.parse_module()
            .map_err(|diags| format!("Parse error in module '{}': {:?}", import_path, diags))?;

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
            ast: module_ast.clone(),
            exports,
            env: BTreeMap::new(),
        };
        self.loaded.insert(namespace.clone(), module);

        Ok((namespace, Some(module_ast)))
    }
    
    pub fn get_export(&self, namespace: &str, name: &str) -> Option<&ExportedItem> {
        self.loaded.get(namespace)?.exports.get(name)
    }
    
    pub fn get_module_env(&self, namespace: &str) -> Option<&BTreeMap<String, Value>> {
        self.loaded.get(namespace).map(|m| &m.env)
    }
    
    pub fn set_module_var(&mut self, namespace: &str, name: String, value: Value) {
        if let Some(module) = self.loaded.get_mut(namespace) {
            module.env.insert(name, value);
        }
    }
}