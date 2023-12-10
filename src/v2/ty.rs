use std::{collections::HashMap, mem};

use super::{vm::WORD_BYTES, Compile, CompileError::*, Word};

type OneOfId = Word;
type RecordId = Word;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Void,
    Int,
    Bool,
    Var(Word),
    Pointer(Box<Ty>),
    VarPointer(Box<Ty>),
    OneOf(OneOfId),
    BitSet(OneOfId),
    Array {
        ty: Box<Ty>,
        length: Word,
    },
    Record {
        id: RecordId,
        type_params: Vec<(Ty, Word)>,
        base_size: Word,
    },
    Sub {
        params: Vec<Ty>,
        ret: Box<Ty>,
    },
}

impl Default for Ty {
    fn default() -> Self {
        Ty::Void
    }
}

impl Ty {
    pub fn size(&self) -> Option<Word> {
        match self {
            Self::Void => Some(0),
            Self::Var(_) => None,
            Self::Array { ty, length } => ty.size().map(|size| size * length),
            Self::Record {
                type_params,
                base_size,
                ..
            } => {
                let mut sum = *base_size;
                for (ty, scale) in type_params {
                    if let Some(size) = ty.size() {
                        sum += size * scale
                    } else {
                        return None;
                    }
                }
                return Some(sum);
            }
            _ => Some(WORD_BYTES),
        }
    }
    pub fn use_address_register(&self) -> bool {
        match self {
            Ty::Pointer(_) | Ty::VarPointer(_) | Ty::Sub { .. } => true,
            _ => false,
        }
    }
    pub fn unify(&self, other: &Ty) -> Compile<Ty> {
        TypeUnifier::unify(self, other)
    }
    fn resolve_vars(self, vars: &HashMap<Word, Ty>) -> Ty {
        match self {
            Self::Var(id) => vars.get(&id).cloned().unwrap_or(Self::Var(id)),
            Self::Pointer(ty) => (*ty).resolve_vars(vars).pointer(),
            Self::VarPointer(ty) => (*ty).resolve_vars(vars).var_pointer(),
            Self::Array { ty, length } => Self::Array {
                ty: Box::new((*ty).resolve_vars(vars)),
                length,
            },
            Self::Record {
                id,
                type_params,
                base_size,
            } => Self::Record {
                id,
                type_params: type_params
                    .into_iter()
                    .map(|(ty, scale)| (ty.resolve_vars(vars), scale))
                    .collect(),
                base_size,
            },
            Self::Sub { params, ret } => Self::Sub {
                params: params.into_iter().map(|p| p.resolve_vars(vars)).collect(),
                ret: Box::new((*ret).resolve_vars(vars)),
            },
            ty => ty,
        }
    }
    pub fn pointer(self) -> Self {
        Self::Pointer(Box::new(self))
    }
    pub fn var_pointer(self) -> Self {
        Self::VarPointer(Box::new(self))
    }
    pub fn deref(self) -> Compile<Self> {
        match self {
            Self::Pointer(ty) => Ok(*ty),
            Self::VarPointer(ty) => Ok(*ty),
            ty => Err(ExpectedType {
                expected: ty.clone().pointer(),
                received: ty,
            }),
        }
    }
}

#[derive(Debug)]
struct TypeUnifier {
    left: Ty,
    right: Ty,
    left_vars: HashMap<Word, Ty>,
    right_vars: HashMap<Word, Ty>,
}

impl TypeUnifier {
    fn unify(left: &Ty, right: &Ty) -> Compile<Ty> {
        let mut u = Self {
            left: left.clone(),
            right: right.clone(),
            left_vars: HashMap::new(),
            right_vars: HashMap::new(),
        };

        u.run()?;
        Ok(u.left.resolve_vars(&u.left_vars))
    }
    fn unify_next(&mut self, left: Ty, right: Ty) -> Compile<()> {
        let mut next = Self {
            left,
            right,
            left_vars: mem::take(&mut self.left_vars),
            right_vars: mem::take(&mut self.right_vars),
        };
        next.run()?;
        self.left_vars = mem::take(&mut next.left_vars);
        self.right_vars = mem::take(&mut next.right_vars);
        Ok(())
    }
    fn run(&mut self) -> Compile<()> {
        let left = match &self.left {
            Ty::Var(id) => self.left_vars.get(id).cloned().unwrap_or(Ty::Var(*id)),
            ty => ty.clone(),
        };
        let right = match &self.right {
            Ty::Var(id) => self.right_vars.get(id).cloned().unwrap_or(Ty::Var(*id)),
            ty => ty.clone(),
        };

        match (left, right) {
            (Ty::Var(_), Ty::Var(_)) => {
                todo!("forget what I'm supposed to do here");
            }
            (Ty::Var(id), right) => {
                self.left_vars.insert(id, right);
                Ok(())
            }
            (left, Ty::Var(id)) => {
                self.right_vars.insert(id, left);
                Ok(())
            }
            (Ty::Void, Ty::Void) => Ok(()),
            (Ty::Int, Ty::Int) => Ok(()),
            (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::Pointer(left), Ty::Pointer(right)) => self.unify_next(*left, *right),
            (Ty::VarPointer(left), Ty::VarPointer(right)) => self.unify_next(*left, *right),
            (Ty::OneOf(l), Ty::OneOf(r)) => {
                if l != r {
                    self.fail()
                } else {
                    Ok(())
                }
            }
            (Ty::BitSet(l), Ty::BitSet(r)) => {
                if l != r {
                    self.fail()
                } else {
                    Ok(())
                }
            }
            (
                Ty::Array {
                    ty: left,
                    length: len_left,
                },
                Ty::Array {
                    ty: right,
                    length: len_right,
                },
            ) => {
                if len_left != len_right {
                    self.fail()?;
                }
                self.unify_next(*left, *right)
            }
            (
                Ty::Record {
                    id: id_l,
                    type_params: tp_l,
                    ..
                },
                Ty::Record {
                    id: id_r,
                    type_params: tp_r,
                    ..
                },
            ) => {
                if id_l != id_r {
                    self.fail()?;
                }
                for i in 0..tp_l.len() {
                    self.unify_next(tp_l[i].0.clone(), tp_r[i].0.clone())?;
                }
                Ok(())
            }
            (
                Ty::Sub {
                    params: p_l,
                    ret: r_l,
                    ..
                },
                Ty::Sub {
                    params: p_r,
                    ret: r_r,
                    ..
                },
            ) => {
                // TODO: I think this might not be quite right
                if p_l.len() != p_r.len() {
                    self.fail()?;
                }
                for i in 0..p_l.len() {
                    self.unify_next(p_l[i].clone(), p_r[i].clone())?;
                }
                self.unify_next(*r_l, *r_r)?;

                Ok(())
            }
            (_, _) => self.fail(),
        }
    }
    fn fail(&self) -> Compile<()> {
        return Err(ExpectedType {
            expected: self.left.clone(),
            received: self.right.clone(),
        });
    }
}

#[cfg(test)]
mod ty_test {
    use super::*;

    fn expect_ok(value: Compile<Ty>, ty: Ty) {
        assert_eq!(value, Ok(ty))
    }

    fn expect_err(value: Compile<Ty>) {
        value.expect_err("expected type error");
    }

    #[test]
    fn simple_ty() {
        expect_ok(Ty::Bool.unify(&Ty::Bool), Ty::Bool);
        expect_ok(Ty::Bool.unify(&Ty::Var(1)), Ty::Bool);
        expect_ok(Ty::Var(1).unify(&Ty::Bool), Ty::Bool);
        expect_err(Ty::Bool.unify(&Ty::Int));
    }

    #[test]

    fn records() {
        expect_ok(
            Ty::Record {
                id: 1,
                type_params: vec![(Ty::Int, 0)],
                base_size: 0,
            }
            .unify(&Ty::Record {
                id: 1,
                type_params: vec![(Ty::Var(1), 0)],
                base_size: 0,
            }),
            Ty::Record {
                id: 1,
                type_params: vec![(Ty::Int, 0)],
                base_size: 0,
            },
        );
    }

    #[test]
    fn subroutines() {
        expect_ok(
            Ty::Sub {
                params: vec![Ty::Var(1), Ty::Bool],
                ret: Box::new(Ty::Var(1)),
            }
            .unify(&Ty::Sub {
                params: vec![Ty::Int, Ty::Var(1)],
                ret: Box::new(Ty::Int),
            }),
            Ty::Sub {
                params: vec![Ty::Int, Ty::Bool],
                ret: Box::new(Ty::Int),
            },
        );
        expect_err(
            Ty::Sub {
                params: vec![Ty::Var(1), Ty::Bool],
                ret: Box::new(Ty::Var(1)),
            }
            .unify(&Ty::Sub {
                params: vec![Ty::Int, Ty::Var(1)],
                ret: Box::new(Ty::Bool),
            }),
        )
    }
}
