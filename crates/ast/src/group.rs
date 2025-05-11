use std::{
    borrow::{Borrow, BorrowMut},
    sync::Mutex,
};

use crate::{Ast, ProgressPart, SyntaxDefinition, SyntaxDefinitionPart, lexer::Result};
use kast_util::*;
use replace_with::{replace_with_or_abort, replace_with_or_abort_and_return};
use tracing::trace;

#[derive(Debug)]
pub enum GroupTag {
    Named,
    Unnamed,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Quantifier {
    /// `rest[";" expr]`
    /// Note: only exists for ease of implementation, absence of a quantifier is currently not
    /// allowed
    #[default]
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
#[derive(Clone, Debug, Default)]
pub struct Group<V = Vec<SyntaxDefinitionPart>> {
    pub name: Option<String>,
    pub quantifier: Quantifier,
    pub sub_parts: V,
}

impl Group {
    pub fn is_named(&self) -> bool {
        self.name.is_some()
    }

    pub fn is_unnamed(&self) -> bool {
        self.name.is_none()
    }
}

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
                    .push(SyntaxDefinitionPart::GroupBinding(group.clone()));
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
            .push(SyntaxDefinitionPart::GroupBinding(group.clone()));
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
        use SyntaxDefinitionPart::*;
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

#[derive(Debug)]
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
        previous: Box<RevLinkedList<Item>>,
    },
}

impl<Item> RevLinkedList<Item> {
    pub fn step_in(&mut self, new_item: Item) {
        replace_with_or_abort(self, |this| match this {
            RevLinkedList::First { item } => RevLinkedList::Nth {
                item: new_item,
                previous: Box::new(RevLinkedList::First { item }),
            },
            RevLinkedList::Nth { item, previous } => RevLinkedList::Nth {
                item: new_item,
                previous: Box::new(RevLinkedList::Nth { item, previous }),
            },
        });
    }

    pub fn step_out(&mut self) -> Result<Item> {
        replace_with_or_abort_and_return(self, |this| match this {
            RevLinkedList::First { .. } => (
                error!("unexpected token to close group when no group is open"),
                this,
            ),
            RevLinkedList::Nth { item, previous } => (
                Ok(item),
                match *previous {
                    RevLinkedList::First { item } => RevLinkedList::First { item },
                    RevLinkedList::Nth { item, previous } => RevLinkedList::Nth { item, previous },
                },
            ),
        })
    }

    pub fn current(&self) -> &Item {
        match self {
            RevLinkedList::First { item } | RevLinkedList::Nth { item, previous: _ } => item,
        }
    }

    pub fn current_mut<'a>(&'a mut self) -> &'a mut Item {
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
            Some(SyntaxDefinitionPart::GroupBinding(group)) => {
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

#[derive(Debug)]
pub struct GroupLocation {
    // Group not used directly here because we can have either &Group or MutexGuard<Group>
    // NOTE: this is a Parc<Mutex<>>
    group: Parc<Mutex<Group>>,
    part_index: usize,
    quantified: usize,
    t_fields: Tuple<Ast>, // `(.key :: ast, .value :: ast)`
    t_groups: Tuple<Ast>, // `(_unnamed :: list[(.key :: ast, .value :: ast), ..])`
}

impl GroupLocation {
    pub fn new(group: Parc<Mutex<Group>>) -> Self {
        Self {
            group,
            part_index: 0,
            quantified: 0,
            t_fields: Tuple::empty(),
            t_groups: Tuple::empty(),
        }
    }

    pub fn name(&self) -> Option<String> {
        self.group.lock().unwrap().name.clone()
    }
}

pub struct GroupTupleCreator<'a> {
    syntax_def: &'a Parc<SyntaxDefinition>,
    span: &'a Span,
    shape: RevLinkedList<GroupLocation>,
}

impl<'a> GroupTupleCreator<'a> {
    pub fn new(
        group: Parc<Mutex<Group>>,
        syntax_def: &'a Parc<SyntaxDefinition>,
        span: &'a Span,
    ) -> Self {
        Self {
            syntax_def,
            span,
            shape: RevLinkedList::First {
                item: GroupLocation::new(group),
            },
        }
    }

    // Pushes the field tuples into the group tuple, then pushes the group tuple into the parent
    // group's tuple and finally closing the group
    fn close_group(&mut self) -> Result<()> {
        // Group being closed
        let child_group = self.shape.step_out()?;
        let name = child_group.name();

        let GroupLocation {
            t_fields: mut child_tuple_fields,
            t_groups: mut child_tuple_groups,
            ..
        } = child_group;
        child_tuple_groups.add_unnamed(Ast::Complex {
            definition: self.syntax_def.clone(),
            data: self.span.clone(),
            values: std::mem::replace(&mut child_tuple_fields, Tuple::empty()),
        });

        // Parent group of group being closed
        self.shape.current_mut().t_fields.add(
            name,
            Ast::Complex {
                definition: self.syntax_def.clone(),
                values: child_tuple_groups,
                data: self.span.clone(),
            },
        );
        Ok(())
    }

    fn bail(&mut self, custom_err: ErrorMessage) -> Result<()> {
        let GroupLocation {
            group, quantified, ..
        } = match &mut self.shape {
            RevLinkedList::First { item: group } => group,
            RevLinkedList::Nth { item: group, .. } => group,
        };
        let group = group.clone();
        let group = &*group.lock().unwrap();
        match (&group.quantifier, quantified) {
            // Failure to finish group
            (Quantifier::One, 0) | (Quantifier::OneOrMore, 0) => return Err(custom_err),
            (Quantifier::Exact(m), n) if *n < m - 1 => return Err(custom_err),

            // Commit empty group tuple to parent group
            (Quantifier::ZeroOrOne, 0) | (Quantifier::ZeroOrMore, 0) => {
                let name = self.shape.step_out()?.name();
                self.shape.current_mut().t_fields.add(
                    name,
                    Ast::Complex {
                        definition: self.syntax_def.clone(),
                        values: Tuple::empty(),
                        data: self.span.clone(),
                    },
                );
                Ok(())
            }

            // Commit existing group tuple but abandon any field tuples this round
            (Quantifier::OneOrMore, 1..) | (Quantifier::ZeroOrMore, 1..) => {
                let child_group = self.shape.step_out()?;
                let name = child_group.name();
                self.shape.current_mut().t_fields.add(
                    name,
                    Ast::Complex {
                        definition: self.syntax_def.clone(),
                        values: child_group.t_groups,
                        data: self.span.clone(),
                    },
                );
                Ok(())
            }

            (Quantifier::Exact(_), _) => unreachable!(),
            (Quantifier::One, _) => unreachable!(),
            (Quantifier::ZeroOrOne, _) => unreachable!(),
        }
    }

    fn _close_group_empty(&mut self) -> Result<()> {
        // Group being closed
        let GroupLocation {
            group: child_group, ..
        } = self.shape.step_out()?;
        let name = &child_group.borrow().lock().unwrap().name;

        // Parent group of group being closed
        let GroupLocation {
            t_fields: parent_tuple_fields,
            ..
        } = self.shape.current_mut();

        let ast = Ast::Complex {
            definition: self.syntax_def.clone(),
            values: Tuple::empty(),
            data: self.span.clone(),
        };

        if let Some(name) = &name {
            parent_tuple_fields.add_named(name, ast);
        } else {
            parent_tuple_fields.add_unnamed(ast);
        }
        Ok(())
    }

    fn repeat_group(&mut self) {
        // Rerun group
        let GroupLocation {
            part_index,
            quantified,
            t_fields,
            t_groups,
            ..
        } = self.shape.current_mut();
        // Set index back to 0
        *part_index = 0;
        *quantified += 1;
        // Move field tuple from previous parse into group tuple
        t_groups.add_unnamed(Ast::Complex {
            definition: self.syntax_def.clone(),
            data: self.span.clone(),
            values: std::mem::replace(t_fields, Tuple::empty()),
        });
    }

    fn _fit(part: SyntaxDefinitionPart, progress: ProgressPart) -> bool {
        match part {
            SyntaxDefinitionPart::Keyword(expected) => {
                progress.into_keyword().is_some_and(|s| &s == &expected)
            }
            SyntaxDefinitionPart::UnnamedBinding => matches!(progress, ProgressPart::Value(_)),
            SyntaxDefinitionPart::NamedBinding(_name) => {
                matches!(progress, ProgressPart::Value(_))
            }
            SyntaxDefinitionPart::GroupBinding(_) => unreachable!(),
        }
    }

    pub fn insert(&mut self, progress: ProgressPart) -> Result<()> {
        let GroupLocation {
            group,
            part_index,
            quantified,
            ..
        } = match &mut self.shape {
            RevLinkedList::First { item: group } => group,
            RevLinkedList::Nth { item: group, .. } => group,
        };
        let group = group.clone();
        let group = &*group.lock().unwrap();
        match group.sub_parts.get(*part_index) {
            Some(SyntaxDefinitionPart::GroupBinding(new_group)) => {
                // warn!("got part group");
                self.shape.step_in(GroupLocation::new(new_group.clone()));
                // We're still yet to insert the value so recurse
                self.insert(progress)?;
                Ok(())
            }
            Some(SyntaxDefinitionPart::Keyword(expected)) => match progress {
                ProgressPart::Keyword(keyword, _) if keyword.as_str() == expected => {
                    trace!(
                        "parsed part keyword `{keyword}` for syntax-def: {:#?} and span: {}",
                        self.syntax_def, self.span
                    );
                    *part_index += 1;
                    Ok(())
                }
                _ => self.bail(error_fmt!("expected keyword not found")),
            },
            Some(SyntaxDefinitionPart::UnnamedBinding) => match progress {
                ProgressPart::Value(value) => {
                    // warn!(
                    //     "parsed part unnamed-binding for syntax-def: {:#?} and span: {}",
                    //     self.syntax_def, self.span
                    // );
                    *part_index += 1;
                    let GroupLocation {
                        t_fields: tuple, ..
                    } = self.shape.current_mut();
                    tuple.add_unnamed(value);
                    // warn!("tuple now: {tuple}");
                    Ok(())
                }
                _ => self.bail(error_fmt!("expected a value")),
            },
            Some(SyntaxDefinitionPart::NamedBinding(name)) => match progress {
                ProgressPart::Value(value) => {
                    // warn!(
                    //     "parsed part named-binding `{name}` for syntax-def: {:#?} and span: {}",
                    //     self.syntax_def, self.span
                    // );
                    *part_index += 1;
                    let GroupLocation {
                        t_fields: tuple, ..
                    } = self.shape.current_mut();
                    tuple.add_named(name.clone(), value);
                    // warn!("tuple now: {tuple}");
                    Ok(())
                }
                _ => self.bail(error_fmt!("expected a value")),
            },
            // Current group over
            None => {
                match (&group.quantifier, &quantified) {
                    // Close group and step-out to previous group
                    (Quantifier::One, 0) | (Quantifier::ZeroOrOne, 0) => self.close_group()?,
                    (Quantifier::Exact(m), n) if **n == m - 1 => self.close_group()?,

                    // Rerun same group
                    (Quantifier::OneOrMore, 0..) | (Quantifier::ZeroOrMore, 0..) => {
                        self.repeat_group()
                    }
                    (Quantifier::Exact(m), n) if **n < m - 1 => self.repeat_group(),

                    (Quantifier::One, 1..) => unreachable!(),
                    (Quantifier::ZeroOrOne, 1..) => unreachable!(),
                    (Quantifier::Exact(_), _) => unreachable!(),
                }
                // We're still yet to insert the value so recurse
                self.insert(progress)?;
                Ok(())
            }
        }
    }

    pub fn finish(self) -> Result<Tuple<Ast>> {
        match self.shape {
            RevLinkedList::First { item } => Ok(item.t_fields),
            RevLinkedList::Nth { .. } => error!("not enough progress was made"),
        }
    }
}

#[derive(Debug)]
pub struct GroupLocation2 {
    group: Parc<Mutex<Group>>,
    part_index: usize,
    quantified: usize,
}

#[derive(Debug)]
pub struct SimplePartsIter {
    shape: RevLinkedList<GroupLocation2>,
}

#[derive(Debug)]
pub enum SimplePart {
    Keyword(String),
    UnnamedBinding,
    NamedBinding(String),
}

impl Iterator for SimplePartsIter {
    type Item = SimplePart;

    fn next(&mut self) -> Option<Self::Item> {
        let GroupLocation2 {
            group,
            part_index,
            quantified,
        } = match &mut self.shape {
            RevLinkedList::First { item: group } => group,
            RevLinkedList::Nth { item: group, .. } => group,
        };
        let group = group.clone();
        let group = &*group.lock().unwrap();
        match group.sub_parts.get(*part_index) {
            // Increase group nesting
            Some(SyntaxDefinitionPart::GroupBinding(new_group)) => {
                self.shape.step_in(GroupLocation2 {
                    group: new_group.clone(),
                    part_index: 0,
                    quantified: 0,
                });
                Self::next(self)
            }
            // Decrease group nesting
            None => {
                _ = self.shape.step_out().ok()?;
                Self::next(self)
            }
            // Simple Part
            Some(SyntaxDefinitionPart::Keyword(k)) => {
                *part_index += 1;
                Some(SimplePart::Keyword(k.clone()))
            }
            Some(SyntaxDefinitionPart::UnnamedBinding) => {
                *part_index += 1;
                Some(SimplePart::UnnamedBinding)
            }
            Some(SyntaxDefinitionPart::NamedBinding(n)) => {
                *part_index += 1;
                Some(SimplePart::NamedBinding(n.clone()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ParsedSyntax, Parser, SyntaxDefinitionPart,
        group::{Group, Quantifier},
        lex, read_syntax_def,
    };
    use kast_util::{Parc, SourceFile};
    use std::sync::Mutex;
    use tracing::info;

    use Quantifier::*;
    use SyntaxDefinitionPart::*;

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
        )
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
}
