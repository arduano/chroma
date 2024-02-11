use super::*;

pub struct SyUnaryNegate {
    pub operator: TkMinus,
    pub operand: Attempted<SyExpression>,
}

impl AstItem for SyUnaryNegate {
    const NAME: &'static str = "unary negate";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let operator = reader.parse_optional_token()?;
        let operand = reader.parse_required(env);
        Ok(Self { operator, operand })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(operand) = &self.operand {
            operand.check(env, errors);
        }
    }
}

impl ItemWithSpan for SyUnaryNegate {
    fn span(&self) -> Span {
        self.operator.span().join(&self.operand.span())
    }
}

pub struct SyUnaryNot {
    pub operator: TkNot,
    pub operand: Attempted<SyExpression>,
}

impl AstItem for SyUnaryNot {
    const NAME: &'static str = "unary not";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let operator = reader.parse_optional_token()?;
        let operand = reader.parse_required(env);
        Ok(Self { operator, operand })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(operand) = &self.operand {
            operand.check(env, errors);
        }
    }
}

impl ItemWithSpan for SyUnaryNot {
    fn span(&self) -> Span {
        self.operator.span().join(&self.operand.span())
    }
}
