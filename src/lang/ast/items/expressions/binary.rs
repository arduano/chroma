use super::*;

macro_rules! try_parse_token {
    ($reader: expr, $token:ident) => {
        if let Ok(token) = $reader.parse_optional_token() {
            return Ok(Self::$token(token));
        }
    };
}

macro_rules! impl_binary_op {
    ($ty:ident, $name:expr, {$($variant:ident($tk:ident));+;}) => {
        #[derive(Debug, Clone)]
        pub enum $ty {
            $($variant($tk),)+
        }

        impl AstItem for $ty {
            const NAME: &'static str = $name;

            fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
            where
                Self: Sized,
            {
                $(
                    try_parse_token!(reader, $variant);
                )+
                return Err(ParseError::NoMatch);
            }
        }

        impl ItemWithSpan for $ty {
            fn span(&self) -> Span {
                match self {
                    $(
                        Self::$variant(op) => op.span(),
                    )+
                }
            }
        }

        impl $ty {
            pub fn as_op_symbol_str(&self) -> &'static str {
                match self {
                    $(
                        Self::$variant(_) => $tk::STATIC_NAME,
                    )+
                }
            }
        }

        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.as_op_symbol_str())
            }
        }
    };
}

impl_binary_op!(SyArithmeticBinaryOp, "arithmetic binary operator", {
    Add(TkPlus);
    Minus(TkMinus);
    Mult(TkMult);
    Div(TkDiv);
    Mod(TkModulo);
});

impl_binary_op!(SyComparativeBinaryOp, "comparative binary operator", {
    Eq(TkEquals);
    Neq(TkNotEquals);
    Gt(TkGreaterThan);
    Gte(TkGreaterThanEq);
    Lt(TkLessThan);
    Lte(TkLessThanEq);
});

impl_binary_op!(SyBooleanLogicBinaryOp, "boolean logic binary operator", {
    And(TkAnd);
    Or(TkOr);
});

impl_binary_op!(SyBitwiseBinaryOp, "bitwise binary operator", {
    BitAnd(TkBitAnd);
    BitOr(TkBitOr);
    BitXor(TkBitXor);
    BitShiftLeft(TkBitShiftLeft);
    BitShiftRight(TkBitShiftRight);
});

impl_binary_op!(SyMetaTypeBinaryOp, "meta type binary operator", {
    Extends(TkExtends);
    Union(TkTypeUnion);
});

#[derive(Debug, Clone)]
pub enum SyBinaryOp {
    Arithmetic(SyArithmeticBinaryOp),
    Comparative(SyComparativeBinaryOp),
    BooleanLogic(SyBooleanLogicBinaryOp),
    Bitwise(SyBitwiseBinaryOp),
    MetaType(SyMetaTypeBinaryOp),
}

impl AstItem for SyBinaryOp {
    const NAME: &'static str = "binary operator";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        macro_rules! try_parse {
            ($token:ident) => {
                if let Ok(token) = reader.parse_optional(env) {
                    return Ok(Self::$token(token));
                }
            };
        }

        try_parse!(Arithmetic);
        try_parse!(Comparative);
        try_parse!(BooleanLogic);
        try_parse!(Bitwise);
        try_parse!(MetaType);

        return Err(ParseError::NoMatch);
    }
}

impl ItemWithSpan for SyBinaryOp {
    fn span(&self) -> Span {
        match self {
            Self::Arithmetic(op) => op.span(),
            Self::Comparative(op) => op.span(),
            Self::BooleanLogic(op) => op.span(),
            Self::Bitwise(op) => op.span(),
            Self::MetaType(op) => op.span(),
        }
    }
}

impl SyBinaryOp {
    pub fn as_op_symbol_str(&self) -> &'static str {
        match self {
            Self::Arithmetic(op) => op.as_op_symbol_str(),
            Self::Comparative(op) => op.as_op_symbol_str(),
            Self::BooleanLogic(op) => op.as_op_symbol_str(),
            Self::Bitwise(op) => op.as_op_symbol_str(),
            Self::MetaType(op) => op.as_op_symbol_str(),
        }
    }
}

impl std::fmt::Display for SyBinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_op_symbol_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOpPrecedence {
    /// Signifies that `A x B x C` should be `(A x B) x C`
    Left,
    /// Signifies that `A x B x C` should be `A x (B x C)`
    Right,
    /// Signifies that the operation is ambiguous and be parenthesized. Treated as left-associative when compiling.
    Illegal,
}

fn arithmetic_op_precedence_num(op: &SyArithmeticBinaryOp) -> Option<u8> {
    use SyArithmeticBinaryOp::*;

    match op {
        Add(_) | Minus(_) => Some(1),
        Mult(_) | Div(_) => Some(2),
        Mod(_) => None,
    }
}

fn arithmetic_op_precedence(
    left: &SyArithmeticBinaryOp,
    right: &SyArithmeticBinaryOp,
) -> BinaryOpPrecedence {
    let left_prec = arithmetic_op_precedence_num(left);
    let right_prec = arithmetic_op_precedence_num(right);

    match (left_prec, right_prec) {
        (Some(left), Some(right)) => {
            if left >= right {
                BinaryOpPrecedence::Left
            } else {
                BinaryOpPrecedence::Right
            }
        }
        _ => BinaryOpPrecedence::Illegal,
    }
}

fn comparative_op_precedence(
    _left: &SyComparativeBinaryOp,
    _right: &SyComparativeBinaryOp,
) -> BinaryOpPrecedence {
    // Always return Illegal. Chaining comparative operators is very confusing.
    BinaryOpPrecedence::Illegal
}

fn boolean_logic_op_precedence(
    left: &SyBooleanLogicBinaryOp,
    right: &SyBooleanLogicBinaryOp,
) -> BinaryOpPrecedence {
    use SyBooleanLogicBinaryOp::*;

    // Illegal if they're not equal. Otherwise, it's left-associative.
    match (left, right) {
        (And(_), And(_)) | (Or(_), Or(_)) => BinaryOpPrecedence::Left,
        _ => BinaryOpPrecedence::Illegal,
    }
}

fn bitwise_op_precedence(
    _left: &SyBitwiseBinaryOp,
    _right: &SyBitwiseBinaryOp,
) -> BinaryOpPrecedence {
    use SyBitwiseBinaryOp::*;

    // Illegal if they're not the same logical operator
    match (_left, _right) {
        (BitAnd(_), BitAnd(_)) | (BitOr(_), BitOr(_)) | (BitXor(_), BitXor(_)) => {
            BinaryOpPrecedence::Left
        }
        _ => BinaryOpPrecedence::Illegal,
    }
}

fn op_precedence(left: &SyBinaryOp, right: &SyBinaryOp) -> BinaryOpPrecedence {
    match (left, right) {
        // Equal cateory of operators
        (SyBinaryOp::Arithmetic(left), SyBinaryOp::Arithmetic(right)) => {
            arithmetic_op_precedence(left, right)
        }
        (SyBinaryOp::Comparative(left), SyBinaryOp::Comparative(right)) => {
            comparative_op_precedence(left, right)
        }
        (SyBinaryOp::BooleanLogic(left), SyBinaryOp::BooleanLogic(right)) => {
            boolean_logic_op_precedence(left, right)
        }
        (SyBinaryOp::Bitwise(left), SyBinaryOp::Bitwise(right)) => {
            bitwise_op_precedence(left, right)
        }

        // Arithmetic to comparative (e.g. `a + b < c`)
        (SyBinaryOp::Arithmetic(_), SyBinaryOp::Comparative(_)) => BinaryOpPrecedence::Left,
        (SyBinaryOp::Comparative(_), SyBinaryOp::Arithmetic(_)) => BinaryOpPrecedence::Right,

        // Arithmetic to boolean logic (e.g. `a + b && c`)
        (SyBinaryOp::Arithmetic(_), SyBinaryOp::BooleanLogic(_)) => BinaryOpPrecedence::Left,
        (SyBinaryOp::BooleanLogic(_), SyBinaryOp::Arithmetic(_)) => BinaryOpPrecedence::Right,

        // Arithmetic to bitwise (e.g. `a + b & c`)
        (SyBinaryOp::Arithmetic(_), SyBinaryOp::Bitwise(_)) => BinaryOpPrecedence::Illegal,
        (SyBinaryOp::Bitwise(_), SyBinaryOp::Arithmetic(_)) => BinaryOpPrecedence::Illegal,

        // Comparative to boolean logic (e.g. `a < b && c`)
        (SyBinaryOp::Comparative(_), SyBinaryOp::BooleanLogic(_)) => BinaryOpPrecedence::Left,
        (SyBinaryOp::BooleanLogic(_), SyBinaryOp::Comparative(_)) => BinaryOpPrecedence::Right,

        // Comparative to bitwise (e.g. `a < b & c`)
        (SyBinaryOp::Comparative(_), SyBinaryOp::Bitwise(_)) => BinaryOpPrecedence::Right,
        (SyBinaryOp::Bitwise(_), SyBinaryOp::Comparative(_)) => BinaryOpPrecedence::Left,

        // Boolean logic to bitwise (e.g. `a && b & c`)
        (SyBinaryOp::BooleanLogic(_), SyBinaryOp::Bitwise(_)) => BinaryOpPrecedence::Right,
        (SyBinaryOp::Bitwise(_), SyBinaryOp::BooleanLogic(_)) => BinaryOpPrecedence::Left,

        // Meta type edge cases
        (
            SyBinaryOp::MetaType(SyMetaTypeBinaryOp::Union(_)),
            SyBinaryOp::MetaType(SyMetaTypeBinaryOp::Union(_)),
        ) => BinaryOpPrecedence::Left,

        // Meta type to anything (e.g. `a extends b + c`)
        (SyBinaryOp::MetaType(_), _) => BinaryOpPrecedence::Illegal,
        (_, SyBinaryOp::MetaType(_)) => BinaryOpPrecedence::Illegal,
    }
}

#[derive(Debug, Clone)]
pub struct SyBinary {
    pub left: Box<Attempted<SyExpression>>,
    pub operator: SyBinaryOp,
    pub right: Box<Attempted<SyExpression>>,
}

impl AstItem for SyBinary {
    const NAME: &'static str = "binary expression";

    fn parse<'a>(_reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        // This function shouldn't be reachable. Instead, parsing is done manually.
        unreachable!()
    }
}

impl ItemWithSpan for SyBinary {
    fn span(&self) -> Span {
        self.left
            .span()
            .join(&self.operator.span())
            .join(&self.right.span())
    }
}

impl FunctionExpression for SyBinary {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        let left = self.left.link_expression(builder, ctx);
        let right = self.right.link_expression(builder, ctx);
        builder.add_statement(Li2ExpressionStatement {
            kind: Li2ExpressionStatementKind::BinaryOp {
                left: left,
                right: right,
                op: self.operator.clone(),
            },
            span: Some(self.span()),
        })
    }
}

#[derive(Debug, Clone)]
struct ExprWithOp {
    // Option is used for temporarily moving data around because std::replace isn't feasible.
    expression: Option<Box<Attempted<SyExpression>>>,
    op: SyBinaryOp,
}

#[derive(Debug, Clone)]
struct BinaryOpChain {
    exprs: Vec<ExprWithOp>,
    // Option is used for temporarily moving data around because std::replace isn't feasible.
    last: Option<Box<Attempted<SyExpression>>>,
}

impl BinaryOpChain {
    fn new(left: Attempted<SyExpression>, op: SyBinaryOp, right: Attempted<SyExpression>) -> Self {
        Self {
            exprs: vec![ExprWithOp {
                expression: Some(Box::new(left)),
                op,
            }],
            last: Some(Box::new(right)),
        }
    }

    fn add(&mut self, op: SyBinaryOp, right: Attempted<SyExpression>) {
        self.exprs.push(ExprWithOp {
            expression: self.last.take(),
            op,
        });
        self.last = Some(Box::new(right));
    }

    fn group_op(&mut self, index: usize) {
        let left_cell = self.exprs.remove(index);

        let left = left_cell.expression.unwrap();
        let op = left_cell.op.clone();

        let right_cell = if index < self.exprs.len() {
            &mut self.exprs[index].expression
        } else {
            &mut self.last
        };
        let right = right_cell.take().unwrap();

        let binary_expr = SyBinary {
            left,
            operator: op,
            right,
        };

        *right_cell = Some(Box::new(Ok(SyExpression::Binary(binary_expr))));
    }

    fn ops_len(&self) -> usize {
        self.exprs.len()
    }

    fn op_at(&self, index: usize) -> &SyBinaryOp {
        &self.exprs[index].op
    }
}

impl ExpressionBottomUpParse for SyBinary {
    fn parse_bottom_up<'a>(
        expression: SyExpression,
        reader: &mut AstParser<'a>,
        env: ParsingPhaseEnv,
    ) -> Result<SyExpression, SyExpression>
    where
        Self: Sized,
    {
        // First, make sure a binary can be parsed in the first place.
        let Ok(binary) = reader.parse_optional(env) else {
            return Ok(expression);
        };

        // Then, parse the right-hand side of the binary expression.
        let right = reader.parse_required(env.inside_binary_expr());

        let mut chain = BinaryOpChain::new(Ok(expression), binary, right);

        // Parse the remaining chain expression
        loop {
            let Ok(binary) = reader.parse_optional(env) else {
                break;
            };

            let right = reader.parse_required(env.inside_binary_expr());

            chain.add(binary, right);
        }

        // Group the binary expressions based on precedence
        loop {
            let length = chain.ops_len();

            if length == 0 {
                break;
            }

            if length == 1 {
                chain.group_op(0);
                break;
            }

            // Repeatedly group the right-most operator precedence, starting from the right-most operator.
            let mut grouped_right = false;
            for index in (1..length).rev() {
                let left = chain.op_at(index - 1);
                let right = chain.op_at(index);

                let precedence = op_precedence(left, right);

                match precedence {
                    BinaryOpPrecedence::Left => {
                        // Continue
                    }
                    BinaryOpPrecedence::Right => {
                        // Group
                        chain.group_op(index);
                        grouped_right = true;
                    }
                    BinaryOpPrecedence::Illegal => {
                        // Return invalid expression
                        reader.add_error(CompilerError::new(
                            "Ambiguous binary operator precedence",
                            chain.op_at(index).span(),
                        ));

                        return Err(SyExpression::Invalid);
                    }
                }
            }

            // If no right precedences were grouped, then only left remain, so left-group them all.
            if !grouped_right {
                while chain.ops_len() > 0 {
                    chain.group_op(0);
                }
                break;
            }
        }

        // Return the final expression
        let last = chain.last.take().unwrap().unwrap();
        Err(last)
    }
}
