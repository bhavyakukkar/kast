use std::{
    borrow::{Borrow, BorrowMut, Cow},
    sync::Mutex,
};

use crate::{lexer::Result, SyntaxDefinitionPart};
use kast_util::*;
use tracing::trace;
use SyntaxDefinitionPart::*;

#[derive(Debug)]
pub enum GroupTag {
    Named,
    Unnamed,
}

#[derive(Debug, PartialEq)]
pub enum Quantifier {
    /// `rest[";" expr]`
    /// Note: only exists for ease of implementation, absence of a quantifier is currently not
    /// allowed
    One,
    /// `rest[";" expr]?`
    ZeroOrOne,
    /// `rest[";" expr]+`
    OneOrMore,
    /// `rest[";" expr]*`
    ZeroOrMore,
    /// `rest[";" expr]{2}`
    Exact(usize),
}

/// A group of syntax-defintion parts that has a quantifier indicating how many occurences should be
/// parsed
#[derive(Debug)]
pub struct Group<V = Vec<SyntaxDefinitionPart>> {
    pub name: Option<String>,
    pub quantifier: Quantifier,
    sub_parts: V,
}

impl<V> Group<V> {
    pub fn is_named(&self) -> bool {
        self.name.is_some()
    }

    pub fn is_unnamed(&self) -> bool {
        self.name.is_none()
    }
}

/* impl<V> Group<V> where V: Borrow<Vec<SyntaxDefinitionPart>> {
    pub fn flat_parts(&self) -> FlatParts<V> {
        FlatParts::new(self)
    }
} */

impl<V> Group<V>
where
    V: BorrowMut<Vec<SyntaxDefinitionPart>>,
{
    pub fn try_push_named_group(&mut self) -> Result<Parc<Mutex<Group>>> {
        match &self.sub_parts.borrow_mut().last() {
            Some(SyntaxDefinitionPart::NamedBinding(name)) => {
                let name = name.clone();
                _ = self.sub_parts.borrow_mut().pop();
                let group = Parc::new(Mutex::new(Group::named(name)));
                self.sub_parts
                    .borrow_mut()
                    .push(GroupBinding(group.clone()));
                Ok(group)
            }
            None | Some(_) => {
                error!("named groups (`[ ... ]`) should be preceded with their name")
            }
        }
    }

    pub fn push_unnamed_group(&mut self) -> Parc<Mutex<Group>> {
        let group = Parc::new(Mutex::new(Group::unnamed()));
        self.sub_parts
            .borrow_mut()
            .push(GroupBinding(group.clone()));
        group
    }
}

impl Group {
    /// Create a new named group `fields[key ":" value]*`
    pub fn named(name: impl Into<String>) -> Self {
        Group {
            name: Some(name.into()),
            quantifier: Quantifier::One,
            sub_parts: Vec::new(),
        }
    }

    /// Create a new unnamed group `("->" returns)?`
    pub fn unnamed() -> Self {
        Group {
            name: None,
            quantifier: Quantifier::One,
            sub_parts: Vec::new(),
        }
    }
}

impl<V> PartialEq for Group<V>
where
    V: Borrow<Vec<SyntaxDefinitionPart>>,
{
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.quantifier == other.quantifier
            && self
                .sub_parts
                .borrow()
                .iter()
                .zip(other.sub_parts.borrow().iter())
                .try_for_each(|(a, b)| match (a, b) {
                    (Keyword(a), Keyword(b)) if a == b => Some(()),
                    (UnnamedBinding, UnnamedBinding) => Some(()),
                    (NamedBinding(a), NamedBinding(b)) if a == b => Some(()),
                    (GroupBinding(a), GroupBinding(b)) => {
                        let a: &Group = &a.lock().unwrap();
                        let b: &Group = &b.lock().unwrap();
                        (*a == *b).then_some(())
                    }
                    _ => None,
                })
                .is_some()
    }
}

/// Reverse linked-list data-structure so that syntax-definitions may contain nested [`Groups`](Group)
///
/// NOTE: This linked-list cannot represent a list of length 0, only 1 or more
pub enum RevLinkedList<Item> {
    /// the current group that parts being read will be pushed into
    /// *None* means we are in no group right now
    First { item: Item },
    /// the pointer to the previous group-ptr node so we can get back to it when the current group
    /// gets closed
    /// *None* means we are in no group right now, or we are in a group inside the root
    Nth {
        item: Item,
        previous: Parc<RevLinkedList<Item>>,
    },
}

impl<Item: Clone> RevLinkedList<Item> {
    pub fn step_in(&mut self, new_item: Item) {
        *self = match &self {
            RevLinkedList::First { item } => RevLinkedList::Nth {
                item: new_item,
                previous: Parc::new(RevLinkedList::First { item: item.clone() }),
            },
            RevLinkedList::Nth { item, previous } => RevLinkedList::Nth {
                item: new_item,
                previous: Parc::new(RevLinkedList::Nth {
                    item: item.clone(),
                    previous: previous.clone(),
                }),
            },
        };
    }

    pub fn step_out(&mut self) -> Result<Item> {
        match &self {
            RevLinkedList::First { .. } => {
                error!("unexpected token to close group when no group is open")
            }
            RevLinkedList::Nth { item, previous } => {
                let item = item.clone();
                let previous = previous.clone();
                *self = match *previous {
                    RevLinkedList::First { ref item } => {
                        RevLinkedList::First { item: item.clone() }
                    }
                    RevLinkedList::Nth {
                        ref item,
                        ref previous,
                    } => RevLinkedList::Nth {
                        item: item.clone(),
                        previous: previous.clone(),
                    },
                };
                Ok(item.clone())
            }
        }
    }
}

impl<Item> RevLinkedList<Item> {
    pub fn current(&self) -> &Item {
        match self {
            RevLinkedList::First { item } | RevLinkedList::Nth { item, previous: _ } => item,
        }
    }

    pub fn current_mut(&mut self) -> &mut Item {
        match self {
            RevLinkedList::First { item } | RevLinkedList::Nth { item, previous: _ } => item,
        }
    }
}

// Wrapping instead of aliasing because might send RevLinkedList to different crate in the future
pub(crate) struct GroupPtr<V = Vec<SyntaxDefinitionPart>>(pub RevLinkedList<Parc<Mutex<Group<V>>>>);

impl<V> GroupPtr<V> {
    pub fn step_out(&mut self) -> Result<Parc<Mutex<Group<V>>>> {
        self.0.step_out()
    }

    pub fn step_in(&mut self, group: Parc<Mutex<Group<V>>>) {
        self.0.step_in(group)
    }

    pub fn current(&self) -> Parc<Mutex<Group<V>>> {
        self.0.current().clone()
    }
}

/// A type to read the parts of a syntax-definition and accordingly nest when groups are read
pub struct PartsAccumulator {
    pub(crate) root: Parc<Mutex<Group>>,
    ptr: GroupPtr,
    // If in the future the `One` quantifier is allowed for groups, remove this field
    unassigned_group: bool,
}

impl PartsAccumulator {
    pub fn new() -> Self {
        let root = Parc::new(Mutex::new(Group::unnamed()));
        PartsAccumulator {
            root: root.clone(),
            ptr: GroupPtr(RevLinkedList::First { item: root }),
            unassigned_group: false,
        }
    }

    /// Request an inserter for this `PartsAccumulator`
    ///
    /// This extra step is added because inserting a part requires that any previously inserted
    /// groups have been assigned a quantifier after being closed
    pub fn insert<'a>(&'a mut self) -> Result<PartsAccumulatorInserter<'a>> {
        if self.unassigned_group {
            return error!("unexpected token encountered when expecting quantifier for group");
        } else {
            Ok(PartsAccumulatorInserter { inner: self })
        }
    }

    /// Finish this accumulator, returning the collected `Vec<SyntaxDefinitionPart>`s
    ///
    /// This fails if the any opened groups have not all been closed
    pub fn finish(self) -> Result<Vec<SyntaxDefinitionPart>> {
        if self.unassigned_group {
            return error!("syntax definition ended when expecting quantifier for group");
        }
        let current = self.ptr;
        match current.0 {
            RevLinkedList::First { .. } => {
                let group = self.root.lock().unwrap();
                Ok(group.sub_parts.clone())
            }
            RevLinkedList::Nth { .. } => {
                error!("syntax definition ended when expecting group to be closed")
            }
        }
    }

    /// Close a group (match the `[` or `(` that marked the opening of a group with a `]` or `)`
    /// respectively)
    ///
    /// This fails in only one way, which is when its called while we are not in any groups
    pub fn close_group(&mut self, tag: GroupTag) -> Result<()> {
        if self.unassigned_group {
            return error!("unexpected token encountered when expecting quantifier for group");
        }

        let current = &mut self.ptr;
        let closed_group = current.step_out()?;
        let closed_group = closed_group.lock().unwrap();
        if closed_group.is_named() && !matches!(tag, GroupTag::Named) {
            return error!("mismatch of tokens, named group `[` cannot be closed with `)`");
        } else if closed_group.is_unnamed() && !matches!(tag, GroupTag::Unnamed) {
            return error!("mismatch of tokens, unnamed group `(` cannot be closed with `]`");
        };

        // Group that has been closed is now waiting to be assigned a quantifier
        self.unassigned_group = true;
        Ok(())
    }

    /// Try to assign a quantifier to the previously inserted group
    ///
    /// This fails if the previously inserted part was not a group
    // INFO Modify this fn if quantifiers are to be allowed for every kind of
    // [part](SyntaxDefinitionPart), not just [group](SyntaxDefinitionPart::Group)
    pub fn assign_quantifier(&mut self, quantifier: Quantifier) -> Result<()> {
        // Quantifier when group hasn't even been closed
        if !self.unassigned_group {
            return error!(
                "unexpected symbol: quantifiers are only allowed after named/unnamed groups"
            );
        }
        self.unassigned_group = false;
        match &self.ptr.0 {
            RevLinkedList::First { item: group }
            | RevLinkedList::Nth {
                item: group,
                previous: _,
            } => {
                let group = group.lock().unwrap(); // temporary value
                Self::update_group(group.sub_parts.last(), quantifier)
            }
        }
    }

    // Utility function to try to update a group with a quantifier
    //
    // This fails if the part is None (vector of parts is empty or group just started) or if its not
    // a group
    fn update_group(part: Option<&SyntaxDefinitionPart>, quantifier: Quantifier) -> Result<()> {
        match part {
            Some(GroupBinding(group)) => {
                trace!(
                    "Assinging {quantifier:?} to group with sub-parts: `{:?}`",
                    group.lock().unwrap().sub_parts
                );
                group.lock().unwrap().quantifier = quantifier;
                Ok(())
            }
            // Quantifier is the first encountered part, or Quantifier after part that isn't a group
            None | Some(_) => {
                error!("unexpected symbol: quantifiers are only allowed after named/unnamed groups")
            }
        }
    }

    // Utility function to insert a part THAT WE KNOW is not a group
    // Used by `PartsAccumulatorInserter`'s `keyword()`, `unnamed_binding()` and `named_binding()`
    fn insert_non_group_part(&mut self, part: SyntaxDefinitionPart) {
        match &self.ptr.0 {
            RevLinkedList::First { item: group }
            | RevLinkedList::Nth {
                item: group,
                previous: _,
            } => group.lock().unwrap().sub_parts.push(part),
        }
    }
}

/// Interface through which we can insert parts into the (`PartsAccumulator`)[PartsAccumulator]
pub struct PartsAccumulatorInserter<'a> {
    inner: &'a mut PartsAccumulator,
}

impl<'a> PartsAccumulatorInserter<'a> {
    /// Insert a new unnamed group, thus increasing the nest depth
    pub fn unnamed_group(&mut self) {
        self.inner.ptr.step_in(
            // push unnamed group into current relative-part
            // take this new group and step into it
            self.inner
                .ptr
                .current()
                .lock()
                .unwrap()
                .push_unnamed_group(),
        );
    }

    /// Insert a new named group, thus increasing the nest depth
    ///
    /// This fails if there isn't a named-binding at the tail of the `parts` of the currently
    /// pointed-to group, which is supposed to be popped out to become the name of this group
    pub fn named_group(&mut self) -> Result<()> {
        self.inner.ptr.step_in(
            self.inner
                .ptr
                .current()
                .lock()
                .unwrap()
                .try_push_named_group()?,
        );
        Ok(())
    }

    /// Insert a keyword
    pub fn keyword(&mut self, keyword: String) {
        self.inner
            .insert_non_group_part(SyntaxDefinitionPart::Keyword(keyword))
    }

    /// Insert an unnamed-binding
    pub fn unnamed_binding(&mut self) {
        self.inner
            .insert_non_group_part(SyntaxDefinitionPart::UnnamedBinding);
    }

    /// Insert a named-binding
    pub fn named_binding(&mut self, name: impl Into<String>) {
        self.inner
            .insert_non_group_part(SyntaxDefinitionPart::NamedBinding(name.into()));
    }
}

// TODO here
#[derive(Clone)]
pub struct FlatPartsPtr<'a> {
    // Result is being used as Either and doesn't indicate success/failure
    // + Ok indicates reference to Group that owns its sub-parts
    // + Err indicates reference to Group that owns a reference to some sub-parts
    group: Result<Parc<Mutex<Group>>, Parc<Mutex<Group<&'a Vec<SyntaxDefinitionPart>>>>>,
    idx: usize,
}

pub(crate) struct FlatParts<'a> {
    root: Parc<Mutex<Group<&'a Vec<SyntaxDefinitionPart>>>>,
    ptr: RevLinkedList<FlatPartsPtr<'a>>,
}

impl<'a> FlatParts<'a> {
    pub fn new(root: Parc<Mutex<Group<&'a Vec<SyntaxDefinitionPart>>>>) -> Self {
        FlatParts {
            ptr: RevLinkedList::First {
                item: FlatPartsPtr {
                    group: Err(root.clone()),
                    idx: 0,
                },
            },
            root,
        }
    }
}

// TODO here
impl<'a> Iterator for FlatParts<'a> {
    // type Item = (SyntaxDefinitionPart, Parc<Mutex<Group>>);
    type Item = SyntaxDefinitionPart;

    fn next(&mut self) -> Option<Self::Item> {
        // Find next non-group part
        let non_group_part = loop {
            let FlatPartsPtr { group, idx } = self.ptr.current();
            match group
                .as_ref()
                .map(|own_group| own_group.lock().unwrap().sub_parts.get(*idx).cloned())
                .unwrap_or_else(|ref_group| ref_group.lock().unwrap().sub_parts.get(*idx).cloned())
            {
                // encountered group, step-in to it and start from id
                Some(SyntaxDefinitionPart::GroupBinding(group)) => {
                    self.ptr.step_in(FlatPartsPtr {
                        group: Ok(group.clone()),
                        idx: 0,
                    });
                }
                // sub-parts of current-group are over, exit group but continue looking for
                // non-group part in parent group
                None => {
                    let Ok(_) = self.ptr.step_out() else {
                        // current-group is actually the root, meaning we are done with all parts
                        return None;
                    };
                    self.ptr.current_mut().idx += 1;
                }
                // encountered non-group part, break
                Some(part @ _) => {
                    self.ptr.current_mut().idx += 1;
                    break part;
                }
            }
        };
        assert!(!matches!(
            non_group_part,
            SyntaxDefinitionPart::GroupBinding(_)
        ));
        Some(non_group_part)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        group::{Group, Quantifier},
        lex, read_syntax_def, ParsedSyntax, Parser, SyntaxDefinitionPart,
    };
    use kast_util::{Parc, SourceFile};
    use std::sync::Mutex;
    use tracing::info;

    use Quantifier::*;
    use SyntaxDefinitionPart::*;

    use super::FlatParts;

    fn group_ptr(group: super::Group) -> SyntaxDefinitionPart {
        GroupBinding(Parc::new(Mutex::new(group)))
    }

    fn test<'a>(source: impl Into<String>, expected_parts: &Vec<SyntaxDefinitionPart>) {
        let syntax_str = format!("syntax foo <- 10 = {}", source.into());
        info!("testing: `{syntax_str}`");
        let mut parser = Parser {
            reader: lex(SourceFile {
                contents: syntax_str,
                filename: "<stdin>".into(),
            })
            .unwrap(),
        };
        let syntax = read_syntax_def(&mut parser.reader).unwrap();
        let ParsedSyntax::Definition(syntax_def) = syntax.0 else {
            panic!();
        };
        let parts = syntax_def.parts;

        assert_eq!(
            &Group {
                name: None,
                quantifier: One,
                sub_parts: &parts,
            },
            &Group {
                name: None,
                quantifier: One,
                sub_parts: expected_parts,
            }
        );
    }

    #[test]
    fn group_named() {
        test(
            r#"fields [ key ":" value ]?"#,
            &vec![group_ptr(Group {
                name: Some("fields".into()),
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("key".into()),
                    Keyword(":".into()),
                    NamedBinding("value".into()),
                ],
            })],
        );
    }

    #[test]
    fn group_unnamed() {
        test(
            r#"( keys ":" values )?"#,
            &vec![group_ptr(Group {
                name: None,
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("keys".into()),
                    Keyword(":".into()),
                    NamedBinding("values".into()),
                ],
            })],
        );
    }

    #[test]
    fn group_named_nested() {
        test(
            r#"hashTableFields[ bucket "=>" values[ value "," ]* ]?"#,
            &vec![group_ptr(Group {
                name: Some("hashTableFields".into()),
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("bucket".into()),
                    Keyword("=>".into()),
                    group_ptr(Group {
                        name: Some("values".into()),
                        quantifier: ZeroOrMore,
                        sub_parts: vec![NamedBinding("value".into()), Keyword(",".into())],
                    }),
                ],
            })],
        );
    }

    #[test]
    fn group_unnamed_nested() {
        test(
            r#"( buckets "=>" ( values "," )* )?"#,
            &vec![group_ptr(Group {
                name: None,
                quantifier: ZeroOrOne,
                sub_parts: vec![
                    NamedBinding("buckets".into()),
                    Keyword("=>".into()),
                    group_ptr(Group {
                        name: None,
                        quantifier: ZeroOrMore,
                        sub_parts: vec![NamedBinding("values".into()), Keyword(",".into())],
                    }),
                ],
            })],
        );
    }

    #[test]
    fn group_flat_parts_iter() {
        let parts = vec![group_ptr(Group {
            name: None,
            quantifier: ZeroOrOne,
            sub_parts: vec![
                NamedBinding("buckets".into()),
                Keyword("=>".into()),
                group_ptr(Group {
                    name: None,
                    quantifier: ZeroOrMore,
                    sub_parts: vec![NamedBinding("values".into()), Keyword(",".into())],
                }),
            ],
        })];
        let root = Parc::new(Mutex::new(Group {
            name: None,
            quantifier: Quantifier::One,
            sub_parts: &parts,
        }));
        let flat_parts = FlatParts::new(root);
        flat_parts.for_each(|part| println!("Next non-group part: `{:?}`", part));
    }
}
