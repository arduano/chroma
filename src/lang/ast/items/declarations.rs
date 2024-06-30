use std::sync::Arc;

use super::*;
use crate::lang::{ast::helpers::*, tokens::*, ErrorCollector};

mod type_fn;
pub use type_fn::*;
mod function;
pub use function::*;

#[derive(Debug, Clone)]
pub enum SyDeclaration {
    TypeDefine(Arc<SyTypeDefine>),
    TypeFn(Arc<SyTypeFn>),
    Function(Arc<SyFunction>),
}

impl SyDeclaration {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Self::TypeDefine(_) => true,
            Self::TypeFn(_) => false,
            Self::Function(_) => false,
        }
    }
}

impl AstItem for SyDeclaration {
    const NAME: &'static str = "declaration";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyDeclaration::TypeFn(Arc::new(expr)));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyDeclaration::TypeDefine(Arc::new(expr)));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyDeclaration::Function(Arc::new(expr)));
        }

        Err(ParseError::NoMatch)
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        match self {
            Self::TypeDefine(expr) => expr.check(env, errors),
            Self::TypeFn(expr) => expr.check(env, errors),
            Self::Function(expr) => expr.check(env, errors),
        }
    }
}

impl ItemWithSpan for SyDeclaration {
    fn span(&self) -> Span {
        match self {
            Self::TypeDefine(expr) => expr.span(),
            Self::TypeFn(expr) => expr.span(),
            Self::Function(expr) => expr.span(),
        }
    }
}

/// Represents a type declaration.
///
/// # Example
///
/// ```no_run
/// type Foo = { a: string }
/// ```
#[derive(Debug, Clone)]
pub struct SyTypeDefine {
    pub type_token: TkType,
    pub name: TkIdent,
    pub eq_token: TkAssign,
    pub value: Arc<Attempted<SyExpression>>,
}

impl AstItem for SyTypeDefine {
    const NAME: &'static str = "type define";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let type_token = reader.parse_optional_token()?;
        let name = reader.parse_optional_token()?;
        let eq_token = reader.parse_optional_token()?;
        let value = reader.parse_required(env.inside_nested_expr());

        Ok(Self {
            type_token,
            name,
            eq_token,
            value: Arc::new(value),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(value) = &*self.value {
            value.check(env.inside_type_only().inside_nested_expr(), errors);
        }
    }
}

impl ItemWithSpan for SyTypeDefine {
    fn span(&self) -> Span {
        self.type_token
            .span()
            .join(&self.eq_token.span())
            .join(&self.value.span())
    }
}
