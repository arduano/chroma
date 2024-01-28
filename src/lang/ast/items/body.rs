use std::{ops::Deref, sync::Arc};

use crate::lang::{ast::helpers::*, tokens::*, CompilerError, ErrorCollector};

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
pub struct SyBody {
    pub statements: Vec<Attempted<SyBodyStatement>>,
}

impl AstItem for SyBody {
    const NAME: &'static str = "body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkSemicolon>());

        while !reader.is_empty() {
            let statement = reader.parse_required::<SyBodyStatement>(env.outside_nested_expr());

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
pub struct SyBodyStatement {
    pub statement: Box<SyStatement>,

    // Result of the semicolon or the span where the semicolon should be
    pub semicolon: Result<TkSemicolon, Span>,
}

impl AstItem for SyBodyStatement {
    const NAME: &'static str = "body statement";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let statement = reader.parse_required(env.outside_nested_expr())?;
        let semicolon = reader.parse_optional_token::<TkSemicolon>();

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
pub struct SyDeclarationBody {
    pub statements: Vec<Attempted<Arc<SyDeclarationBodyItem>>>,
}

impl AstItem for SyDeclarationBody {
    const NAME: &'static str = "declaration body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkBlockLineEndSearch>());

        while !reader.is_empty() {
            let statement =
                reader.parse_required::<SyDeclarationBodyItem>(env.outside_nested_expr());

            if let Ok(statement) = &statement {
                let needs_semi = statement.item.needs_semicolon();
                match needs_semi {
                    false => {
                        if let Ok(semicolon) = &statement.semicolon {
                            reader.add_error(CompilerError::new(
                                "Unexpected ;",
                                semicolon.span().clone(),
                            ));
                        }
                    }
                    true => {
                        if let Err(span) = &statement.semicolon {
                            reader.add_error(CompilerError::new("Expected ;", span.clone()));
                        }
                    }
                }
            }

            statements.push(statement.map(Arc::new));
        }

        Ok(Self { statements })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for statement in &self.statements {
            if let Ok(statement) = statement.deref() {
                statement.check(env, errors);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyDeclarationPubVisibility {
    pub public: TkPub,
}

impl AstItem for SyDeclarationPubVisibility {
    const NAME: &'static str = "declaration visibility";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let public = reader.parse_optional_token::<TkPub>()?;
        Ok(Self { public })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyDeclarationBodyItem {
    pub visibility: Option<SyDeclarationPubVisibility>,
    pub item: SyDeclaration,

    // Result of the semicolon or the span where the semicolon should be
    pub semicolon: Result<TkSemicolon, Span>,
}

impl AstItem for SyDeclarationBodyItem {
    const NAME: &'static str = "body declaration item";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let visibility = reader.parse_optional::<SyDeclarationPubVisibility>(env);
        let visibility = match visibility {
            Ok(visibility) => Some(visibility),
            Err(ParseError::NoMatch) => None,
            Err(ParseError::Error) => return Err(ParseError::Error),
        };

        let item = reader.parse_optional(env)?;

        let semicolon = reader.parse_optional_token::<TkSemicolon>();

        let semicolon = match semicolon {
            Ok(semicolon) => Ok(semicolon),
            Err(_) => {
                let span = reader.input().span().clone();
                Err(span)
            }
        };

        Ok(Self {
            visibility,
            item,
            semicolon,
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Some(visibility) = &self.visibility {
            visibility.check(env, errors);
        }

        self.item.check(env, errors);
    }
}
