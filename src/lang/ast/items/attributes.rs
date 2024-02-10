use crate::lang::{ast::helpers::*, tokens::*, CompilerError, ErrorCollector};

use super::SyExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct SyAttribute {
    at: TkAt,
    name: TkIdent,
    contents: Option<SyAttributeParams>,
}

impl AstItem for SyAttribute {
    const NAME: &'static str = "attribute";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let at = reader.parse_optional_token()?;
        let name = reader.parse_optional_token()?;
        let contents = reader.parse_optional(env.inside_nested_expr()).ok();

        Ok(Self { at, name, contents })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Some(params) = &self.contents {
            params.check(env.inside_nested_expr(), errors);
        }
    }
}

impl ItemWithSpan for SyAttribute {
    fn span(&self) -> Span {
        self.at
            .span()
            .join(&self.name.span())
            .join(&self.contents.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyAttributeParams {
    pub parens: TkParens,
    pub fields: SAttributeFields,
}

impl AstItem for SyAttributeParams {
    const NAME: &'static str = "attribute params";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let (parens, fields) = reader.parse_optional_group(env.inside_nested_expr())?;
        Ok(Self { parens, fields })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.fields.check(env.inside_nested_expr(), errors);
    }
}

impl ItemWithSpan for SyAttributeParams {
    fn span(&self) -> Span {
        self.parens.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SAttributeFields {
    pub fields: Vec<Attempted<SyExpression>>,
}

impl AstItem for SAttributeFields {
    const NAME: &'static str = "attribute fields";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut fields = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkComma>());

        while !reader.is_empty() {
            let field = reader.parse_required(env.inside_nested_expr());

            let comma = reader.parse_optional_token::<TkComma>();

            if comma.is_err() && !reader.is_empty() {
                let span = reader.search_until_token::<TkDataLineEndSearch>();
                reader.add_error(CompilerError::new("Expected ,", span));
            }

            fields.push(field);
        }

        Ok(Self { fields })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for field in &self.fields {
            if let Ok(field) = field {
                field.check(env, errors);
            }
        }
    }
}

impl ItemWithSpan for SAttributeFields {
    fn span(&self) -> Span {
        self.fields.span()
    }
}
