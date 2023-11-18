use crate::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ty {
    kind: TyKind,
    ref_level: usize,
}

impl Ty {
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
    pub fn struct_(data: TyStruct) -> Self {
        Self {
            kind: TyKind::Struct(Box::new(data)),
            ref_level: 0,
        }
    }
    pub fn oneof(data: TyOneOf) -> Self {
        Self {
            kind: TyKind::OneOf(Box::new(data)),
            ref_level: 0,
        }
    }
    pub fn bitset(data: TyOneOf) -> Self {
        Self {
            kind: TyKind::BitSet(Box::new(data)),
            ref_level: 0,
        }
    }
    pub fn array(item_ty: Ty, capacity: Word) -> Self {
        Self {
            kind: TyKind::Array(Box::new(TyArray {
                ty: item_ty,
                capacity,
            })),
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
            TyKind::Int => 1,
            TyKind::Bool => 1,
            TyKind::Struct(s) => s.size,
            TyKind::OneOf(_) => 1,
            // TODO: large bitsets
            TyKind::BitSet(_) => 1,
            TyKind::Array(a) => a.ty.size() * a.capacity,
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
    pub fn struct_field(&self, field_name: &str) -> Compile<StructField> {
        let fields = match &self.kind {
            TyKind::Struct(fs) => fs,
            _ => return Err(Expected("struct")),
        };
        Ok(fields.get(field_name)?.clone())
    }
    pub fn oneof_member(&self, member_name: &str) -> Compile<TyOneOfMember> {
        let fields = match &self.kind {
            TyKind::OneOf(fields) => fields,
            _ => return Err(Expected("oneof")),
        };
        Ok(fields.get(&member_name)?.clone())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Int,
    Bool,
    Struct(Box<TyStruct>),
    OneOf(Box<TyOneOf>),
    BitSet(Box<TyOneOf>),
    Array(Box<TyArray>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyStruct {
    id: usize,
    size: Word,
    fields: HashMap<String, StructField>,
}

impl TyStruct {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            size: 0,
            fields: HashMap::new(),
        }
    }
    pub fn insert(&mut self, k: String, ty: Ty) -> Compile<()> {
        if self.fields.contains_key(&k) {
            return Err(DuplicateField);
        }
        let size = ty.size();
        self.fields.insert(
            k,
            StructField {
                ty,
                offset: self.size,
            },
        );
        self.size += size;
        return Ok(());
    }
    pub fn get(&self, k: &str) -> Compile<&StructField> {
        self.fields.get(k).ok_or(MissingField)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub ty: Ty,
    pub offset: Word,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyOneOfMember {
    pub index: Word,
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
