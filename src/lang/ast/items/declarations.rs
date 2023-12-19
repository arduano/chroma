use super::*;
use crate::lang::{ast::helpers::*, tokens::*};

mod type_fn;
pub use type_fn::*;
mod module;
pub use module::*;

#[derive(Debug, Clone, PartialEq)]
pub enum SDeclaration {
    TypeDefine(STypeDefine),
    TypeFn(STypeFn),
    Module(SModule),
}

impl AstItem for SDeclaration {
    const NAME: &'static str = "declaration";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SDeclaration::TypeFn(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SDeclaration::TypeDefine(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SDeclaration::Module(expr));
        }

        Err(ParseError::NoMatch)
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        match self {
            Self::TypeDefine(expr) => expr.check(env, errors),
            Self::TypeFn(expr) => expr.check(env, errors),
            Self::Module(expr) => expr.check(env, errors),
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
#[derive(Debug, Clone, PartialEq)]
pub struct STypeDefine {
    pub type_token: TType,
    pub name: TIdent,
    pub eq_token: TAssign,
    pub value: Box<Attempted<SExpression>>,
}

impl AstItem for STypeDefine {
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
            value: Box::new(value),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(value) = &*self.value {
            value.check(env.inside_type_only().inside_nested_expr(), errors);
        }
    }
}
