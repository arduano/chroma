use crate::lang::{
    ast::{helpers::*, items::SDeclarationBody},
    tokens::*,
    CompilerError,
};

use super::SExpression;

/// Represents a module.
///
/// # Example
///
/// ```no_run
/// mod: {
///     pub fn test() {
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SModule {
    pub mod_: TMod,
    pub colon: TColon,
    pub body: Attempted<(TBraces, SDeclarationBody)>,
}

impl AstItem for SModule {
    const NAME: &'static str = "object literal";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mod_ = reader.parse_optional_token::<TMod>()?;
        let colon = reader.parse_optional_token::<TColon>()?;
        let body =
            reader.parse_required_group::<TBraces, SDeclarationBody>(env.outside_nested_expr());

        Ok(Self { mod_, colon, body })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok((_, body)) = &self.body {
            body.check(env, errors);
        }
    }
}
