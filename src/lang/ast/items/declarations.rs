use std::sync::Arc;

use super::*;
use crate::lang::{
    ast::{
        helpers::*,
        linked_items::StatementId,
        linking::{
            ident_finder::LinkingIdentFinder, FunctionBuilder, FunctionExpression,
            FunctionLinkingCompilation,
        },
    },
    tokens::*,
};

mod type_fn;
pub use type_fn::*;
mod function;
pub use function::*;

#[derive(Debug, Clone)]
pub enum SyDeclaration {
    TypeDefine(Arc<SyTypeDefine>),
    Function(Arc<SyFunction>),
}

impl SyDeclaration {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Self::TypeDefine(_) => true,
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
            return Ok(SyDeclaration::TypeDefine(Arc::new(expr)));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyDeclaration::Function(Arc::new(expr)));
        }

        Err(ParseError::NoMatch)
    }
}

impl ItemWithSpan for SyDeclaration {
    fn span(&self) -> Span {
        match self {
            Self::TypeDefine(expr) => expr.span(),
            Self::Function(expr) => expr.span(),
        }
    }
}

impl FunctionExpression for SyDeclaration {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        match self {
            Self::TypeDefine(expr) => todo!(),
            Self::Function(expr) => todo!(),
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
}

impl ItemWithSpan for SyTypeDefine {
    fn span(&self) -> Span {
        self.type_token
            .span()
            .join(&self.eq_token.span())
            .join(&self.value.span())
    }
}
