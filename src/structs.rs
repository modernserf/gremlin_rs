use std::collections::HashMap;
use std::rc::Rc;

use crate::memory::*;
use crate::runtime::Word;
use crate::ty::*;
use crate::{Compile, CompileError::*};

pub type RcTyStruct = Rc<TyStruct>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyStruct {
    id: usize,
    pub size: Word,
    pub fields: HashMap<(StructCase, String), StructField>,
    pub cases: HashMap<String, Word>,
    pub case_field: Option<StructField>,
}

pub type StructCase = Option<Word>;

impl TyStruct {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            size: 0,
            fields: HashMap::new(),
            cases: HashMap::new(),
            case_field: None,
        }
    }
    pub fn insert_case(&mut self, k: String) -> Compile<Word> {
        let id = self.cases.len() as Word;
        // insert a field for the case discriminator itself
        if self.case_field.is_none() {
            self.case_field = Some(StructField {
                ty: Ty::int(),
                offset: self.size,
                case: None,
            });
            self.size += 1;
        }

        if self.cases.insert(k, id).is_some() {
            return Err(Expected("duplicate struct case"));
        }

        Ok(id)
    }
    // TODO: cases use shared space
    pub fn insert(&mut self, k: String, ty: Ty, case: StructCase) -> Compile<()> {
        let key = (case, k);
        if self.fields.contains_key(&key) {
            return Err(DuplicateField);
        }
        let size = ty.size();
        self.fields.insert(
            key,
            StructField {
                ty,
                offset: self.size,
                case: None,
            },
        );
        self.size += size;
        Ok(())
    }
    pub fn get(&self, k: &str, case: StructCase) -> Compile<&StructField> {
        self.fields
            .get(&(case, k.to_string())) // fixme
            .or_else(|| self.fields.get(&(None, k.to_string())))
            .ok_or(MissingField)
    }
    pub fn get_case(&self, k: &str) -> Compile<Word> {
        self.cases.get(k).copied().ok_or(Expected("case"))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub ty: Ty,
    pub offset: Word,
    pub case: Option<Word>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyOneOfMember {
    pub index: Word,
}

#[derive(Debug)]
pub struct MatchBuilder {
    table_index: usize,
    ty: Ty,
    cases: HashMap<String, Word>,
    end_addrs: Vec<usize>,
    block: Block,
}
impl MatchBuilder {
    pub fn new(ty: Ty, block: Block, table_index: usize, cases: HashMap<String, Word>) -> Self {
        Self {
            ty,
            block,
            table_index,
            cases,
            end_addrs: Vec::new(),
        }
    }
    pub fn add_case(&self, tag: &str, memory: &mut Memory) -> Compile<MatchCaseBuilder> {
        let case_id = *self.cases.get(tag).ok_or(Expected("case"))?;
        memory.set_jump_target(self.table_index, case_id as usize);

        Ok(MatchCaseBuilder::new(self.ty.clone(), case_id, self.block))
    }
    pub fn end_case(&mut self, case: MatchCaseBuilder, memory: &mut Memory) {
        self.end_addrs.push(memory.end_case());
    }
    pub fn resolve(self, memory: &mut Memory) {
        memory.set_jump_end_targets(self.table_index, self.cases.len(), &self.end_addrs)
    }
}

#[derive(Debug)]
pub struct MatchCaseBuilder {
    ty: Ty,
    case_id: Word,
    parent_block: Block,
}

impl MatchCaseBuilder {
    fn new(ty: Ty, case_id: Word, parent_block: Block) -> Self {
        Self {
            ty,
            case_id,
            parent_block,
        }
    }
    // TODO: create a new scope for each match case
    pub fn add_binding(&self, binding: String, memory: &mut Memory) -> Compile<()> {
        let field = self.ty.struct_field(&binding, Some(self.case_id))?;
        let block = self.parent_block.focus(Slice::from_struct_field(field));
        let frame_offset = match block {
            Block::Stack(slice) => slice.offset,
            Block::Local(slice) => slice.offset,
            _ => panic!("invalid block"),
        };
        memory.locals.insert(
            binding,
            ScopeRecord {
                frame_offset,
                ty: field.ty.clone(),
            },
        );
        Ok(())
    }
}
