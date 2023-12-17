mod type_fn_sig;
pub use type_fn_sig::*;

use crate::syntax::tokens::*;

use super::{expression::SExpression, *};

/// Represents a type fn.
///
/// # Example
///
/// ```no_run
/// type fn AddField<Name: const ident, Val: Value> {
///     type Result = {
///         ...Val,
///         [Name]: string,
///     };
///
///     Result
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct STypeFn {
    signature: STypeFnSignature,
    body: Attempted<(TBraces, SBody)>,
}

impl AstItem for STypeFn {
    const NAME: &'static str = "type function";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let signature = reader.parse_optional(env)?;
        let body = reader.parse_required_group::<TBraces, SBody>(env.outside_nested_expr());

        Ok(Self { signature, body })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.signature.check(env, errors);

        if let Ok((_, body)) = &self.body {
            body.check(env, errors);
        }
    }
}
