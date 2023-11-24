use std::collections::HashMap;
use std::rc::Rc;

use crate::runtime::*;
use crate::{Compile, CompileError::*};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ty {
    kind: TyKind,
    ref_level: usize,
}

impl Ty {
    pub fn void() -> Self {
        Self {
            kind: TyKind::Void,
            ref_level: 0,
        }
    }
    pub fn int() -> Self {
        Self {
            kind: TyKind::Int,
            ref_level: 0,
        }
    }
    pub fn bool() -> Self {
        Self {
            kind: TyKind::Bool,
            ref_level: 0,
        }
    }
    pub fn record(data: TyRecord) -> Self {
        Self {
            kind: TyKind::Struct(Rc::new(data)),
            ref_level: 0,
        }
    }
    pub fn oneof(data: TyOneOf) -> Self {
        Self {
            kind: TyKind::OneOf(Rc::new(data)),
            ref_level: 0,
        }
    }
    pub fn array(item_ty: Ty, capacity: Word) -> Self {
        Self {
            kind: TyKind::Array(Rc::new(TyArray {
                ty: item_ty,
                capacity,
            })),
            ref_level: 0,
        }
    }
    pub fn sub(data: TySub) -> Self {
        Self {
            kind: TyKind::Sub(Rc::new(data)),
            ref_level: 0,
        }
    }
    pub fn add_ref(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            ref_level: self.ref_level + 1,
        }
    }
    pub fn deref(&self) -> Compile<Self> {
        if self.ref_level == 0 {
            Err(InvalidDeref)
        } else {
            Ok(Self {
                kind: self.kind.clone(),
                ref_level: self.ref_level - 1,
            })
        }
    }
    pub fn check(&self, other: &Self) -> Compile<()> {
        if self == other {
            Ok(())
        } else {
            Err(ExpectedType(self.clone(), other.clone()))
        }
    }
    pub fn cast(&self, other: Self) -> Compile<Self> {
        if self.size() != other.size() {
            Err(InvalidCast)
        } else {
            Ok(other)
        }
    }
    pub fn size(&self) -> Word {
        if self.ref_level > 0 {
            return 1;
        }
        match &self.kind {
            TyKind::Void => 0,
            TyKind::Int => 1,
            TyKind::Bool => 1,
            TyKind::Struct(s) => s.size,
            TyKind::OneOf(_) => 1,
            // TODO: large bitsets
            TyKind::BitSet(_) => 1,
            TyKind::Array(a) => a.ty.size() * a.capacity,
            TyKind::Sub(_) => 1,
        }
    }
    pub fn get_sub(&self) -> Compile<Rc<TySub>> {
        match &self.kind {
            TyKind::Sub(sub) => Ok(sub.clone()),
            _ => Err(Expected("subroutine")),
        }
    }
    pub fn index_ty(&self, index: &Ty) -> Compile<&Ty> {
        match &self.kind {
            TyKind::Array(a) => {
                Ty::int().check(index)?;
                Ok(&a.ty)
            }
            _ => Err(Expected("indexable type")),
        }
    }
    pub fn get_record(&self) -> Compile<Rc<TyRecord>> {
        match &self.kind {
            TyKind::Struct(fs) => Ok(fs.clone()),
            _ => Err(Expected("struct case")),
        }
    }
    pub fn oneof_member(&self, member_name: &str) -> Compile<&TyOneOfMember> {
        let fields = match &self.kind {
            TyKind::OneOf(fields) => fields,
            TyKind::BitSet(fields) => fields,
            _ => return Err(Expected("oneof")),
        };
        fields.get(member_name)
    }
    pub fn as_bitset(self) -> Compile<Self> {
        match self.kind {
            TyKind::OneOf(fields) => Ok(Self {
                ref_level: self.ref_level,
                kind: TyKind::BitSet(fields),
            }),
            TyKind::BitSet(_) => Ok(self),
            _ => Err(Expected("bitset-compatible")),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Void,
    Int,
    Bool,
    Struct(Rc<TyRecord>),
    OneOf(Rc<TyOneOf>),
    BitSet(Rc<TyOneOf>),
    Array(Rc<TyArray>),
    Sub(Rc<TySub>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyOneOf {
    id: usize,
    members: HashMap<String, TyOneOfMember>,
}

impl TyOneOf {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            members: HashMap::new(),
        }
    }
    pub fn insert(&mut self, key: String, index: Word) -> Compile<()> {
        match self.members.insert(key, TyOneOfMember { index }) {
            Some(_) => Err(DuplicateField),
            None => Ok(()),
        }
    }
    pub fn get(&self, key: &str) -> Compile<&TyOneOfMember> {
        self.members.get(key).ok_or(MissingField)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TyArray {
    ty: Ty,
    capacity: Word,
}

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TySub {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

impl TySub {
    pub fn new(params: Vec<Ty>, ret: Ty) -> Self {
        Self { params, ret }
    }
    pub fn args_size(&self) -> Word {
        self.params.iter().map(|p| p.size()).sum::<Word>()
    }
}
