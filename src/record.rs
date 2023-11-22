use std::collections::HashMap;
use std::rc::Rc;

use crate::expr::*;
use crate::memory::*;
use crate::runtime::Word;
use crate::ty::*;
use crate::{Compile, CompileError::*};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyRecord {
    id: usize,
    pub size: Word,
    pub fields: HashMap<(RecordCase, String), RecordField>,
    pub cases: HashMap<String, Word>,
    pub case_field: Option<RecordField>,
}

pub type RecordCase = Option<Word>;

impl TyRecord {
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
            self.case_field = Some(RecordField {
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
    pub fn insert(&mut self, k: String, ty: Ty, case: RecordCase) -> Compile<()> {
        let key = (case, k);
        if self.fields.contains_key(&key) {
            return Err(DuplicateField);
        }
        let size = ty.size();
        self.fields.insert(
            key,
            RecordField {
                ty,
                offset: self.size,
                case: None,
            },
        );
        self.size += size;
        Ok(())
    }
    pub fn get(&self, k: &str, case: RecordCase) -> Compile<&RecordField> {
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
pub struct RecordField {
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
    case_index: CaseIndex,
    record: Rc<TyRecord>,
    end_addrs: Vec<usize>,
    block: Block,
}
impl MatchBuilder {
    pub fn new(expr: Expr, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        let res = expr.resolve(memory, target);
        let record = res.ty.get_record()?;
        let case_field = record.case_field.as_ref().ok_or(Expected("case field"))?;
        let case_value = res.block.record_field(&case_field);
        let case_index = memory.begin_match(case_value, record.cases.len());
        Ok(Self {
            record,
            block: res.block,
            case_index,
            end_addrs: Vec::new(),
        })
    }
    pub fn add_case(&self, tag: &str, memory: &mut Memory) -> Compile<MatchCaseBuilder> {
        let case_id = *self.record.cases.get(tag).ok_or(Expected("case"))?;
        memory.set_jump_target(self.case_index, case_id as usize);

        Ok(MatchCaseBuilder::new(
            self.record.clone(),
            case_id,
            self.block,
        ))
    }
    pub fn end_case(&mut self, memory: &mut Memory) {
        self.end_addrs.push(memory.end_case());
    }
    pub fn resolve(self, memory: &mut Memory) {
        memory.set_jump_end_targets(self.case_index, self.record.cases.len(), &self.end_addrs)
    }
}

#[derive(Debug)]
pub struct MatchCaseBuilder {
    pub record: Rc<TyRecord>,
    pub case_id: Word,
    pub parent_block: Block,
}

impl MatchCaseBuilder {
    fn new(record: Rc<TyRecord>, case_id: Word, parent_block: Block) -> Self {
        Self {
            record,
            case_id,
            parent_block,
        }
    }
}
