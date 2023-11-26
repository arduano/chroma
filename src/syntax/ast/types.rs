use crate::syntax::tokens::*;

use super::{AstItem, Attempted};

#[derive(Debug, Clone, PartialEq)]
pub struct STypeFn {
    pub name: Attempted<TIdent>,
    pub args: Attempted<STypeArgs>,
}

impl AstItem for STypeFn {
    const NAME: &'static str = "type function";

    fn parse<'a>(reader: &mut super::AstParser<'a>) -> super::ParseResult<Self>
    where
        Self: Sized,
    {
        reader.parse_optional_token::<TType>()?;
        reader.parse_optional_token::<TFn>()?;

        let name = reader.parse_required_token();
        let args = reader.parse_required();

        Ok(Self { name, args })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct STypeArgs {
    pub args: Vec<Attempted<STypeArg>>,
}

impl AstItem for STypeArgs {
    const NAME: &'static str = "type arguments";

    fn parse<'a>(reader: &mut super::AstParser<'a>) -> super::ParseResult<Self>
    where
        Self: Sized,
    {
        reader.parse_optional_token::<TLessThan>()?;

        let mut args = Vec::new();

        loop {
            let arg = reader.parse_required();
            let errored = arg.is_err();
            args.push(arg);

            let had_comma = reader.parse_optional_token::<TComma>().is_ok();

            if errored || !had_comma {
                reader.parse_required_token::<TGreaterThan>().ok();
                break;
            } else {
                if reader.parse_optional_token::<TGreaterThan>().is_ok() {
                    break;
                }
            }
        }

        Ok(Self { args })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct STypeArg {
    pub name: TIdent,
    pub constraint: Option<STypeConstraint>,
}

impl AstItem for STypeArg {
    const NAME: &'static str = "type argument";

    fn parse<'a>(reader: &mut super::AstParser<'a>) -> super::ParseResult<Self>
    where
        Self: Sized,
    {
        let name = reader.parse_required_token()?;

        let has_constraint = reader.parse_optional_token::<TColon>().is_ok();

        let constraint = if has_constraint {
            Some(reader.parse_required()?)
        } else {
            None
        };

        Ok(Self { name, constraint })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct STypeConstraint {
    pub is_const: bool,
    pub name: Attempted<TIdent>, //TODO: Fix
}

impl AstItem for STypeConstraint {
    const NAME: &'static str = "type constraint";

    fn parse<'a>(reader: &mut super::AstParser<'a>) -> super::ParseResult<Self>
    where
        Self: Sized,
    {
        let is_const = reader.parse_optional_token::<TConst>().is_ok();

        Ok(Self {
            is_const,
            name: reader.parse_required_token(),
        })
    }
}
