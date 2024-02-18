use crate::lang::{
    solver::{ModItemSet, TypeIdWithSpan},
    tokens::TkIdent,
};

use super::{
    NormalizationError, NormalizationQuery, TyType, TyTypeFlags, TyTypeLogic,
    TypeAssignabilityQuery, TypeDependencies, TypeSubsetQuery,
};

#[derive(Debug, Clone)]
pub struct TyStruct {
    pub literal: Option<TyStructLiteral>,
}

impl TyStruct {
    pub fn new() -> Self {
        Self { literal: None }
    }

    pub fn new_literal(fields: Vec<TyStructLiteralField>) -> Self {
        Self {
            literal: Some(TyStructLiteral::new(fields)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyStructLiteral {
    pub fields: Vec<TyStructLiteralField>,
}

impl TyStructLiteral {
    pub fn new(fields: Vec<TyStructLiteralField>) -> Self {
        Self { fields }
    }

    pub fn get_field(&self, name: &TkIdent) -> Option<&TyStructLiteralField> {
        self.fields
            .iter()
            .find(|field| field.name.ident == name.ident)
    }
}

#[derive(Debug, Clone)]
pub struct TyStructLiteralField {
    pub name: TkIdent,
    pub value: TypeIdWithSpan,
}

impl TyStructLiteralField {
    pub fn new(name: TkIdent, value: TypeIdWithSpan) -> Self {
        Self { name, value }
    }
}

impl TyTypeLogic for TyStruct {
    fn check_assignable_to(&self, other: &Self, query: &mut TypeAssignabilityQuery) -> bool {
        // If other is a generic struct type, then any other struct type can be assigned to it.
        let Some(other_literal) = other.literal.as_ref() else {
            return true;
        };

        // If self is a generic struct type, then it can't be assigned to a non generic struct type.
        let Some(self_literal) = self.literal.as_ref() else {
            return false;
        };

        // If both are non generic struct types, then self can be assigned to other if all of other's
        // fields are in self.
        let self_fields = &self_literal.fields;
        let other_fields = &other_literal.fields;

        other_fields.iter().all(|other_field| {
            let self_field = self_fields
                .iter()
                .find(|self_field| self_field.name.ident == other_field.name.ident);

            let Some(self_field) = self_field else {
                return false;
            };

            query.is_assignable_to(self_field.value.id, other_field.value.id)
        })
    }

    fn get_intersection(&self, other: &Self) -> Self {
        // If both are generic struct types, then the intersection is also generic.
        if self.literal.is_none() && other.literal.is_none() {
            return TyStruct { literal: None };
        }

        // If one is generic and the other is not, then the intersection is the non generic one.
        let Some(_left) = self.literal.as_ref() else {
            return other.clone();
        };

        let Some(_right) = other.literal.as_ref() else {
            return self.clone();
        };

        todo!();

        // If both are non generic struct types, then the intersection is the fields that are in
        // both (the field types being the intersection of both fields).
        // let mut fields = Vec::new();
        // for left_field in &left.fields {
        //     for right_field in &right.fields {
        //         if left_field.name.ident == right_field.name.ident {
        //             fields.push(TyStructLiteralField {
        //                 name: left_field.name.clone(),
        //                 value: left_field.value.get_intersection(&right_field.value),
        //             });
        //             break;
        //         }
        //     }
        // }

        // TyStruct {
        //     literal: Some(TyStructLiteral { fields }),
        // }
    }

    fn is_substate_of(&self, other: &Self, query: &mut TypeSubsetQuery) -> bool {
        // If other is a generic struct type, then any other struct is a subset of it.
        let Some(other_literal) = other.literal.as_ref() else {
            return true;
        };

        // If self is a generic struct type, then it it can't be a subset of a non generic one.
        let Some(self_literal) = self.literal.as_ref() else {
            return false;
        };

        // If both are non generic struct types, then self is a subset of other if all the fieds are
        // identical and field values are subsets
        let self_fields = &self_literal.fields;
        let other_fields = &other_literal.fields;

        let other_fields_are_substate = other_fields.iter().all(|other_field| {
            let self_field = self_fields
                .iter()
                .find(|self_field| self_field.name.ident == other_field.name.ident);

            let Some(self_field) = self_field else {
                return false;
            };

            query.is_substet_of(self_field.value.id, other_field.value.id)
        });

        if !other_fields_are_substate {
            return false;
        }

        let no_missing_fields = self_fields.iter().all(|self_field| {
            other_fields
                .iter()
                .any(|other_field| other_field.name.ident == self_field.name.ident)
        });

        if !no_missing_fields {
            return false;
        }

        true
    }

    fn get_normalized(
        &self,
        ctx: &mut NormalizationQuery,
    ) -> Result<Option<Self>, NormalizationError> {
        if let Some(literal) = &self.literal {
            for field in &literal.fields {
                ctx.ensure_non_required_type_normalized(&field.value)?;
            }

            Ok(None)
        } else {
            Ok(None)
        }
    }

    fn flags(&self, types: &ModItemSet<TyType>) -> TyTypeFlags {
        if let Some(literal) = &self.literal {
            let mut flags = TyTypeFlags::new_all();

            for field in &literal.fields {
                let ty = types.get(field.value.id);
                if let Some(ty) = ty {
                    flags = flags.join(ty.flags(types));
                } else {
                    flags = flags.join(TyTypeFlags::new_for_unknown());
                }
            }

            flags
        } else {
            TyTypeFlags::new_all()
        }
    }

    fn get_type_dependencies(&self, _types: &ModItemSet<TyType>) -> TypeDependencies {
        if let Some(literal) = &self.literal {
            let inner_types = literal.fields.iter().map(|field| field.value.id).collect();

            TypeDependencies {
                inner_types,
                ..Default::default()
            }
        } else {
            TypeDependencies::new_empty()
        }
    }
}
