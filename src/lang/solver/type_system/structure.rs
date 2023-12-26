use crate::lang::tokens::TkIdent;

use super::{TyType, TyTypeLogic};

#[derive(Debug, Clone)]
pub struct TyStruct {
    literal: Option<TyStructLiteral>,
}

#[derive(Debug, Clone)]
pub struct TyStructLiteral {
    fields: Vec<TyStructLiteralField>,
}

#[derive(Debug, Clone)]
pub struct TyStructLiteralField {
    name: TkIdent,
    value: TyType,
}

impl TyTypeLogic for TyStruct {
    fn is_assignable_to(&self, other: &Self) -> bool {
        // If other is a generic struct type, then any other struct type can be assigned to it.
        let Some(other_literal) = other.literal.as_ref() else {
            return true;
        };

        // If self is a generic struct type, then it can't be assigned to a non generic struct type.
        let Some(self_literal) = self.literal.as_ref() else {
            return false;
        };

        // If both are non generic struct types, then self can be assigned to other if all of its
        // fields are in other.
        let self_fields = &self_literal.fields;
        let other_fields = &other_literal.fields;
        self_fields.iter().all(|self_field| {
            other_fields
                .iter()
                .any(|other_field| self_field.name.ident == other_field.name.ident)
        })
    }

    fn get_intersection(&self, other: &Self) -> Self {
        // If both are generic struct types, then the intersection is also generic.
        if self.literal.is_none() && other.literal.is_none() {
            return TyStruct { literal: None };
        }

        // If one is generic and the other is not, then the intersection is the non generic one.
        let Some(left) = self.literal.as_ref() else {
            return other.clone();
        };

        let Some(right) = other.literal.as_ref() else {
            return self.clone();
        };

        // If both are non generic struct types, then the intersection is the fields that are in
        // both (the field types being the intersection of both fields).
        let mut fields = Vec::new();
        for left_field in &left.fields {
            for right_field in &right.fields {
                if left_field.name.ident == right_field.name.ident {
                    fields.push(TyStructLiteralField {
                        name: left_field.name.clone(),
                        value: left_field.value.get_intersection(&right_field.value),
                    });
                    break;
                }
            }
        }

        TyStruct {
            literal: Some(TyStructLiteral { fields }),
        }
    }
}
