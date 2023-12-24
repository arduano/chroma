use crate::lang::{
    ast::{helpers::*, items::SyDeclarationBody},
    tokens::*,
};

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
pub struct SyModule {
    pub mod_: TkMod,
    pub colon: TkColon,
    pub body: Attempted<(TkBraces, SyDeclarationBody)>,
}

impl AstItem for SyModule {
    const NAME: &'static str = "object literal";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mod_ = reader.parse_optional_token::<TkMod>()?;
        let colon = reader.parse_optional_token::<TkColon>()?;
        let body =
            reader.parse_required_group::<TkBraces, SyDeclarationBody>(env.outside_nested_expr());

        Ok(Self { mod_, colon, body })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok((_, body)) = &self.body {
            body.check(env, errors);
        }
    }
}
