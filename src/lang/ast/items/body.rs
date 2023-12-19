use crate::lang::{ast::helpers::*, tokens::*, CompilerError};

use super::*;

/// Represents the contents of a body (inside {} braces), basically a list of statements.
///
/// # Example
///
/// ```no_run
///     type Result = {
///         ...Val,
///         [Name]: string,
///     };
///
///     let a = 1;
///
///     Result
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SBody {
    pub statements: Vec<Attempted<SBodyStatement>>,
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
            let statement = reader.parse_required::<SBodyStatement>(env.outside_nested_expr());

            if let Ok(statement) = &statement {
                let semi_requirement = statement.statement.needs_semicolon();
                match semi_requirement {
                    SemiRequirement::Never => {
                        if let Ok(semicolon) = &statement.semicolon {
                            reader.add_error(CompilerError::new(
                                "Unexpected ;",
                                semicolon.span().clone(),
                            ));
                        }
                    }
                    SemiRequirement::Expression => {
                        if !reader.is_empty() {
                            if let Err(span) = &statement.semicolon {
                                reader.add_error(CompilerError::new("Expected ;", span.clone()));
                            }
                        }
                    }
                    SemiRequirement::Always => {
                        if let Err(span) = &statement.semicolon {
                            reader.add_error(CompilerError::new("Expected ;", span.clone()));
                        }
                    }
                }
            }

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

/// Represents a statement with a semicolon at the end.
///
/// # Example
///
/// ```no_run
/// let a = 1;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SBodyStatement {
    pub statement: Box<SStatement>,

    // Result of the semicolon or the span where the semicolon should be
    pub semicolon: Result<TSemicolon, Span>,
}

impl AstItem for SBodyStatement {
    const NAME: &'static str = "body statement";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let statement = reader.parse_required(env.outside_nested_expr())?;
        let semicolon = reader.parse_optional_token::<TSemicolon>();

        let semicolon = match semicolon {
            Ok(semicolon) => Ok(semicolon),
            Err(_) => {
                let span = reader.input().span().clone();
                Err(span)
            }
        };

        Ok(Self {
            statement: Box::new(statement),
            semicolon,
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.statement.check(env.outside_nested_expr(), errors);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SDeclarationBody {
    pub statements: Vec<Attempted<SDeclarationBodyItem>>,
}

impl AstItem for SDeclarationBody {
    const NAME: &'static str = "declaration body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_end());

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

#[derive(Debug, Clone, PartialEq)]
pub struct SDeclarationPubVisibility {
    pub public: TPub,
}

impl AstItem for SDeclarationPubVisibility {
    const NAME: &'static str = "declaration visibility";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let public = reader.parse_optional_token::<TPub>()?;
        Ok(Self { public })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SDeclarationBodyItem {
    pub visibility: Option<SDeclarationPubVisibility>,
    pub item: Attempted<SDeclaration>,
}

impl AstItem for SDeclarationBodyItem {
    const NAME: &'static str = "declaration";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let visibility = reader.parse_optional::<SDeclarationPubVisibility>(env);
        let visibility = match visibility {
            Ok(visibility) => Some(visibility),
            Err(ParseError::NoMatch) => None,
            Err(ParseError::Error) => return Err(ParseError::Error),
        };

        let item = reader.parse_required(env);

        Ok(Self { visibility, item })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(item) = &self.item {
            item.check(env, errors);
        }
    }
}
