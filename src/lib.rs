extern crate syntex_syntax as syntax;
use std::vec::Vec;
use std::collections::BTreeMap;

pub type Path = Vec<String>;
pub fn as_path(p: &str) -> Path {
    p.split("::").map(String::from).collect()
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item(pub String, pub Option<String>);

impl<'a> From<&'a str> for Item {
    fn from(s: &str) -> Item {
        let trimmed = s.trim();
        let elements: Vec<&str> = trimmed.split_whitespace().collect();
        if elements.len() == 3 && elements[1] == "as" {
            Item(elements[0].to_string(), Some(elements[2].to_string()))
        } else {
            Item(trimmed.to_string(), None)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ViewPath {
    /// `foo::bar::baz as quux`
    ///
    /// or just
    ///
    /// `foo::bar::baz` (with `as baz` implicitly on the right)
    ViewPathSimple(Path, Option<String>),

    /// `foo::bar::*`
    ViewPathGlob(Path),

    /// `foo::bar::{a,b,c}`
    ViewPathList(Path, Vec<Item>),
}

impl<'a> From<&'a str> for ViewPath {
    fn from(s: &str) -> ViewPath {
        let path = as_path(s);
        let mut trimmed_path = path[0..path.len() - 1].to_vec();
        let last = path.last().map(|s| s.clone()).unwrap_or(String::new());
        if path.len() > 1 && "*" == last.as_str() {
            ViewPath::ViewPathGlob(trimmed_path)
        } else if last.starts_with("{") && last.ends_with("}") {
            let items: Vec<_> = last[1..last.len() - 1].split(",").map(|s| Item::from(s)).collect();
            if items.len() == 1 && items[0].0 == "self" {
                ViewPath::ViewPathSimple(trimmed_path, items[0].1.clone())
            } else {
                ViewPath::ViewPathList(trimmed_path, items)
            }
        } else {
            let last_path_element_as_item = Item::from(&last[..]);
            trimmed_path.push(last_path_element_as_item.0);
            ViewPath::ViewPathSimple(trimmed_path, last_path_element_as_item.1)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportNode {
    pub has_self: bool,
    pub has_glob: bool,
    pub renames: Vec<String>,
    pub children: BTreeMap<String, ImportNode>,
}

impl ImportNode {
    fn new() -> ImportNode {
        ImportNode {
            has_self: false,
            has_glob: false,
            renames: vec![],
            children: BTreeMap::new(),
        }
    }
    fn self_or_rename(rename: &Option<String>) -> ImportNode {
        ImportNode {
            has_self: rename.is_none(),
            has_glob: false,
            renames: rename.iter().map(String::clone).collect(),
            children: BTreeMap::new(),
        }
    }
    fn just_glob() -> ImportNode {
        ImportNode {
            has_self: false,
            has_glob: true,
            renames: vec![],
            children: BTreeMap::new(),
        }
    }
    fn combine_with(&mut self, b: &ImportNode) {
        self.has_self |= b.has_self;
        self.has_glob |= b.has_glob;
        for r in &b.renames {
            if !self.renames.contains(r) {
                self.renames.push(r.clone());
            }
        }
        self.renames.sort();
        for (k, v) in &b.children {
            if self.children.contains_key(k) {
                self.children.get_mut(k).map(|existing| existing.combine_with(v));
            } else {
                self.children.insert(k.clone(), v.clone());
            }
        }
    }
}

const CONFIG_MIN_IMPORT_ITEM_LIST_LENGTH: usize = 3;

// Define a representation of imports that is intended to simpliy the process of compressing and
// optimising the import list.
#[derive(Clone, Debug, PartialEq)]
pub struct ImportCombiner {
    root: ImportNode,
}

impl ImportCombiner {
    pub fn new() -> ImportCombiner {
        ImportCombiner { root: ImportNode::new() }
    }

    pub fn add_imports(&mut self, vps: &[&ViewPath]) {
        for vp in vps {
            self.add_import(vp);
        }
    }

    pub fn add_import(&mut self, vp: &ViewPath) {
        use ViewPath::*;
        match vp {
            // Globs and simple declarations are easy enough.
            &ViewPathGlob(ref p) => self.add_node(p, ImportNode::just_glob()),
            &ViewPathSimple(ref p, ref rename) => {
                self.add_node(p, ImportNode::self_or_rename(rename))
            }
            &ViewPathList(ref p, ref items) => {
                let mut path = p.clone();
                for i in items {
                    if i.0 == "self" {
                        self.add_node(&path, ImportNode::self_or_rename(&i.1));
                    } else {
                        path.push(i.0.clone());
                        self.add_node(&path, ImportNode::self_or_rename(&i.1));
                        path.pop();
                    }
                }
            }
        }
    }
    fn add_node(&mut self, path: &[String], node: ImportNode) {
        fn add_node_internal<'a>(node: &'a mut ImportNode, path: &[String]) -> &'a mut ImportNode {
            if path.len() == 0 {
                node
            } else {
                let next_node =
                    node.children.entry(path[0].clone()).or_insert_with(|| ImportNode::new());
                add_node_internal(next_node, &path[1..])
            }
        }
        add_node_internal(&mut self.root, path).combine_with(&node);
    }
    pub fn get_import_list(&self) -> Vec<ViewPath> {
        fn get_imports_for_node(node: &ImportNode,
                                self_already_consumed: bool,
                                renames_already_consumed: bool,
                                mut node_path: &mut Path,
                                mut imports: &mut Vec<ViewPath>) {
            let mut consumed_child_selves = false;
            let mut consumed_child_renames = false;
            let need_self_declaration = node.has_self && !self_already_consumed;

            // First construct a list of the imports that can be expressed for this node
            let mut use_list: Vec<Item> = vec![];
            if need_self_declaration {
                use_list.push(Item("self".to_string(), None));
            }
            if !renames_already_consumed {
                use_list.extend(node.renames.iter().map(|r| Item("self".to_string(), Some(r.clone()))));
            }
            for (child_name, child_node) in &node.children {
                if child_node.has_self && !node.has_glob {
                    use_list.push(Item(child_name.clone(), None));
                }
                use_list.extend(child_node.renames
                    .iter()
                    .map(|r| Item(child_name.clone(), Some(r.clone()))));
            }
            // Now - are we going to use the list? Yes, if it has sufficient elements...
            let will_use_list = use_list.len() >= CONFIG_MIN_IMPORT_ITEM_LIST_LENGTH;
            if will_use_list {
                // As we're using the list, add in any 'self' declaration
                imports.push(ViewPath::ViewPathList(node_path.clone(), use_list));
                consumed_child_selves = true;
                consumed_child_renames = true;
            } else {
                if need_self_declaration {
                    imports.push(ViewPath::ViewPathSimple(node_path.clone(), None));
                }
                if !renames_already_consumed {
                    imports.extend(node.renames
                        .iter()
                        .map(|r| ViewPath::ViewPathSimple(node_path.clone(), Some(r.clone()))));
                }
            }
            if node.has_glob {
                imports.push(ViewPath::ViewPathGlob(node_path.clone()));
                consumed_child_selves = true;
            }
            for (child_name, child_node) in &node.children {
                node_path.push(child_name.clone());
                get_imports_for_node(child_node,
                                     consumed_child_selves,
                                     consumed_child_renames,
                                     &mut node_path,
                                     &mut imports);
                node_path.pop();
            }
        }
        let mut import_list: Vec<ViewPath> = vec![];
        get_imports_for_node(&self.root, false, false, &mut vec![], &mut import_list);
        import_list
    }
}

pub fn combine_imports(vps: &[&ViewPath]) -> Vec<ViewPath> {
    let mut combiner = ImportCombiner::new();
    combiner.add_imports(vps);
    combiner.get_import_list()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn split_path() {
        assert_eq!(ViewPath::from("a::b::c"),
                   ViewPath::ViewPathSimple(vec!["a".to_string(),
                                                 "b".to_string(),
                                                 "c".to_string()],
                                            None));
        assert_eq!(ViewPath::from("a::b::c as rename"),
                   ViewPath::ViewPathSimple(vec!["a".to_string(),
                                                 "b".to_string(),
                                                 "c".to_string()],
                                            Some("rename".to_string())));
        assert_eq!(ViewPath::from("::a::b::c"),
                   ViewPath::ViewPathSimple(vec!["".to_string(),
                                                 "a".to_string(),
                                                 "b".to_string(),
                                                 "c".to_string()],
                                            None));
        assert_eq!(ViewPath::from("::a::b::*"),
                   ViewPath::ViewPathGlob(vec!["".to_string(), "a".to_string(), "b".to_string()]));
        assert_eq!(ViewPath::from("::a::b::{self, d ,e as   x, f}"),
                   ViewPath::ViewPathList(vec!["".to_string(), "a".to_string(), "b".to_string()],
                                          vec![Item("self".to_string(), None),
                                               Item("d".to_string(), None),
                                               Item("e".to_string(), Some("x".to_string())),
                                               Item("f".to_string(), None)]));
        assert_eq!(ViewPath::from("::a::b::{self}"),
                   ViewPath::ViewPathSimple(vec!["".to_string(),
                                                 "a".to_string(),
                                                 "b".to_string()],
                                            None));
    }
    #[test]
    fn combine_glob_and_child() {
        assert_eq!(combine_imports(&(vec![&ViewPath::from("a::b::c"),
                                          &ViewPath::from("a::b::*")])),
                   vec![ViewPath::from("a::b::*")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::c"),
                                         &ViewPath::from("a::b::*"),
                                         &ViewPath::from("a::b as x")]),
                   vec![ViewPath::from("a::b as x"), ViewPath::from("a::b::*")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::c"),
                                         &ViewPath::from("a::b::*"),
                                         &ViewPath::from("a::b::c as x")]),
                   vec![ViewPath::from("a::b::*"), ViewPath::from("a::b::c as x")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::c"),
                                         &ViewPath::from("a::b as y"),
                                         &ViewPath::from("a::b::*"),
                                         &ViewPath::from("a::b::c as x")]),
                   vec![ViewPath::from("a::b as y"),
                        ViewPath::from("a::b::*"),
                        ViewPath::from("a::b::c as x")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::c"),
                                         &ViewPath::from("a::b as y"),
                                         &ViewPath::from("a::b::*"),
                                         &ViewPath::from("a::b::d as e"),
                                         &ViewPath::from("a::b::c as z"),
                                         &ViewPath::from("a::b::c as x")]),
                   vec![ViewPath::from("a::b::{self as y, c as x, c as z, d as e}"),
                        ViewPath::from("a::b::*")]);
    }
    #[test]
    fn combine_glob_and_list() {
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{c, d, e}"),
                                         &ViewPath::from("a::b::*")]),
                   vec![ViewPath::from("a::b::*")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{c as b, d, e}"),
                                         &ViewPath::from("a::b::*")]),
                   vec![ViewPath::from("a::b::*"), ViewPath::from("a::b::c as b")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::*"),
                                         &ViewPath::from("a::b::{c as cc, d as dd, e as ee}")]),
                   vec![ViewPath::from("a::b::{c as cc, d as dd, e as ee}"),
                        ViewPath::from("a::b::*")]);
    }
    #[test]
    fn combine_glob_and_list_and_self() {
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{self,c,d,e}"),
                                         &ViewPath::from("a::b::*")]),
                   vec![ViewPath::from("a::b"), ViewPath::from("a::b::*")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{self, self as x,c,d,e}"),
                                         &ViewPath::from("a::b::*")]),
                   vec![ViewPath::from("a::b"),
                        ViewPath::from("a::b as x"),
                        ViewPath::from("a::b::*")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{self, self as x,c,d,d as \
                                                          dd,e}"),
                                         &ViewPath::from("a::b::*")]),
                   vec![ViewPath::from("a::b::{self, self as x,d as dd}"),
                        ViewPath::from("a::b::*")]);
    }
    #[test]
    fn combine_lists() {
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{g,h,a,b}"),
                                         &ViewPath::from("a::b::{c,d,e}")]),
                   vec![ViewPath::from("a::b::{a,b,c,d,e,g,h}")]);
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{g,h,a,b}"),
                                         &ViewPath::from("a::b::{c,h as fgh,d,e}")]),
                   vec![ViewPath::from("a::b::{a,b,c,d,e,g,h, h as fgh}")]);
    }
    #[test]
    fn combine_lists_with_self() {
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::{g,h,a,b}"),
                                         &ViewPath::from("a::b::{c,d,e}"),
                                         &ViewPath::from("a::b")]),
                   vec![ViewPath::from("a::b::{self,a,b,c,d,e,g,h}")]);
    }
    #[test]
    fn combine_simples() {
        assert_eq!(combine_imports(&vec![&ViewPath::from("a::b::c"),
                                         &ViewPath::from("a::b::e"),
                                         &ViewPath::from("a::b::d"),
                                         &ViewPath::from("a::b::b"),
                                         &ViewPath::from("a::b::h"),
                                         &ViewPath::from("a::b::k"),
                                         &ViewPath::from("a::b"),
                                         &ViewPath::from("a")]),
                   vec![ViewPath::from("a"), ViewPath::from("a::b::{self,b,c,d,e,h,k}")]);
    }
    #[test]
    fn combine_simples_and_glob() {
        assert_eq!(combine_imports(&[&ViewPath::from("a::b::c"),
                                     &ViewPath::from("a::b::e"),
                                     &ViewPath::from("a::b::d"),
                                     &ViewPath::from("a::b::b"),
                                     &ViewPath::from("c"),
                                     &ViewPath::from("a::b::h"),
                                     &ViewPath::from("a::b::k"),
                                     &ViewPath::from("a::b::*"),
                                     &ViewPath::from("a")]),
                   vec![ViewPath::from("a"), ViewPath::from("a::b::*"), ViewPath::from("c")]);

        assert_eq!(combine_imports(&[&ViewPath::from("a::b::c"),
                                     &ViewPath::from("a::b::e"),
                                     &ViewPath::from("a::b::d as yy"),
                                     &ViewPath::from("a::b::b"),
                                     &ViewPath::from("c"),
                                     &ViewPath::from("a::b::h"),
                                     &ViewPath::from("a::b::k"),
                                     &ViewPath::from("a::b::*"),
                                     &ViewPath::from("a")]),
                   vec![ViewPath::from("a"),
                        ViewPath::from("a::b::*"),
                        ViewPath::from("a::b::d as yy"),
                        ViewPath::from("c")]);
    }
}
