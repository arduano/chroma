use crate::syntax::tokens::*;

use super::{expression::SExpression, *};

/// Represents the contents of a body (inside {} braces), basically a list of statements.
///
/// # Example
///
/// ```no_run
/// {
///     type Result = {
///         ...Val,
///         [Name]: string,
///     };
///
///     let a = 1;
///
///     Result
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SBody {
    pub statements: Vec<Attempted<SExpression>>,
}

impl AstItem for SBody {
    const NAME: &'static str = "body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TSemicolon>());

        while !reader.is_empty() {
            let statement = reader.parse_required(env.outside_nested_expr());
            statements.push(statement);
        }

        Ok(Self { statements })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for statement in &self.statements {
            if let Ok(statement) = statement {
                statement.check(env, errors);
            }
        }
    }
}
