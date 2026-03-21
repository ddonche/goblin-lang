use std::collections::BTreeMap;
use goblin_ast as ast;
use crate::Value;

pub mod markdown {
    use comrak::{markdown_to_html, ComrakOptions};

    pub fn md_to_html(md: &str) -> String {
        let mut options = ComrakOptions::default();

        // Extensions
        options.extension.table = true;
        options.extension.autolink = true;
        options.extension.tasklist = true;
        options.extension.strikethrough = true;
        options.extension.superscript = true;
        options.extension.footnotes = true;
        options.extension.header_ids = Some(String::new());

        // Parse
        options.parse.smart = false;

        // Render - allow all valid UTF-8 through
        options.render.unsafe_ = true;
        options.render.hardbreaks = false;
        options.render.github_pre_lang = true;

        markdown_to_html(md, &options)
    }
}

pub mod highlight {
    use std::sync::OnceLock;
    use syntect::highlighting::ThemeSet;
    use syntect::html::highlighted_html_for_string;
    use syntect::parsing::{SyntaxSet, SyntaxDefinition};

    const GOBLIN_SYNTAX: &str = include_str!("../syntaxes/Goblin.sublime-syntax");

    static SS: OnceLock<SyntaxSet> = OnceLock::new();
    static TS: OnceLock<ThemeSet> = OnceLock::new();

    fn ss() -> &'static SyntaxSet {
        SS.get_or_init(|| {
            let mut builder = SyntaxSet::load_defaults_newlines().into_builder();
            match SyntaxDefinition::load_from_str(GOBLIN_SYNTAX, true, None) {
                Ok(goblin) => { builder.add(goblin); }
                Err(e) => { eprintln!("Warning: failed to load Goblin syntax: {}", e); }
            }
            builder.build()
        })
    }

    fn ts() -> &'static ThemeSet {
        TS.get_or_init(ThemeSet::load_defaults)
    }

    pub fn highlight_code(code: &str, lang: &str, dark_theme: &str, light_theme: &str) -> String {
        let ss = ss();
        let ts = ts();

        let syntax = ss
            .find_syntax_by_token(lang)
            .or_else(|| ss.find_syntax_by_extension(lang))
            .unwrap_or_else(|| ss.find_syntax_plain_text());

        let render = |theme_name: &str| {
            let theme = ts.themes.get(theme_name)
                .unwrap_or_else(|| ts.themes.values().next().unwrap());
            match highlighted_html_for_string(code, ss, syntax, theme) {
                Ok(html) => {
                    let start = html.find("background-color:");
                    if let Some(s) = start {
                        let end = html[s..].find(';').map(|e| s + e + 1).unwrap_or(s);
                        format!("{}{}", &html[..s], &html[end..])
                    } else {
                        html
                    }
                }
                Err(_) => format!("<pre><code>{}</code></pre>", code),
            }
        };

        format!(
            "<div class=\"hl-dark\">{}</div><div class=\"hl-light\">{}</div>",
            render(dark_theme),
            render(light_theme)
        )
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