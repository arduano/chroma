use crate::lang::solver::{MId, ModItemSet, TypeIdWithSpan};

use super::{TyType, TyTypeFlags, TyTypeKind, TyTypeLogic, TypeSubsetabilityCache};

#[derive(Debug, Clone)]
pub struct NormalizationSuccess;
#[derive(Debug, Clone)]
pub struct NormalizationError;

pub struct NormalizationQuery<'a> {
    pub types: &'a mut ModItemSet<TyType>,
    pub type_subsetability: &'a mut TypeSubsetabilityCache,

    /// The required normalizations in the current ctx. If a cycle happens here, it's an error.
    required_parent_normalizations: Vec<MId<TyType>>,
    /// The parent calls outside required normalizations. If a cycle happens here, we ignore that path.
    parent_calls: Vec<MId<TyType>>,
}

impl<'a> NormalizationQuery<'a> {
    pub fn new(
        types: &'a mut ModItemSet<TyType>,
        type_subsetability: &'a mut TypeSubsetabilityCache,
    ) -> Self {
        Self {
            types,
            type_subsetability,
            required_parent_normalizations: Vec::new(),
            parent_calls: Vec::new(),
        }
    }

    /// Try to normalize a required type. Return an error if it fails.
    pub fn ensure_required_type_normalized(
        &mut self,
        ty_ref: &TypeIdWithSpan,
    ) -> Result<NormalizationSuccess, NormalizationError> {
        macro_rules! bail {
            () => {
                let error_ty = TyType::new(
                    TyTypeKind::Unknown,
                    ty_ref.span.clone(),
                    TyTypeFlags::new_all(),
                );
                self.types.replace_value(ty_ref.id, error_ty);
                return Err(NormalizationError);
            };
        }

        let Some(ty) = self.types.get(ty_ref.id) else {
            bail!();
        };

        if ty.flags.is_normalized {
            return Ok(NormalizationSuccess);
        }

        if self.required_parent_normalizations.contains(&ty_ref.id) {
            bail!();
        }

        if self.parent_calls.contains(&ty_ref.id) {
            return Ok(NormalizationSuccess);
        }

        let Some(ty) = self.types.get(ty_ref.id) else {
            bail!();
        };

        self.required_parent_normalizations.push(ty_ref.id);

        // TODO: Keep the borrow checker happy. In the future, maybe wrap all
        // the types in Arc? And have a `get` and `get_arc` function on the type sets.
        let ty = ty.clone();

        if let Some(normalized) = ty.get_normalized(self)? {
            self.types.replace_value(ty_ref.id, normalized);
        }

        self.required_parent_normalizations.pop();

        Ok(NormalizationSuccess)
    }

    /// Try to normalize a non required type. Same as normalizing a required type, except a
    /// new query is created with all the required parent calls being moved to the parent_calls.
    pub fn ensure_non_required_type_normalized(
        &mut self,
        ty_ref: &TypeIdWithSpan,
    ) -> Result<NormalizationSuccess, NormalizationError> {
        let mut new_parent_calls = self.parent_calls.clone();
        new_parent_calls.extend_from_slice(&self.required_parent_normalizations);

        let mut new_query = NormalizationQuery {
            types: self.types,
            type_subsetability: self.type_subsetability,
            required_parent_normalizations: vec![],
            parent_calls: new_parent_calls,
        };

        new_query.ensure_required_type_normalized(ty_ref)
    }
}

#[derive(Debug, Clone)]
pub struct CantNormalize;
