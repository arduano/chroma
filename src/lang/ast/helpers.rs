use crate::lang::{
    tokens::{ParseGroupToken, ParseSimpleToken, Span, TokenItem, TokenReader},
    CompilerError,
};

const DEBUG: bool = true;
macro_rules! debug {
    ($($arg:tt)*) => {
        if DEBUG {
            println!($($arg)*);
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    NoMatch,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseErrorNoMatch;
#[derive(Debug, Clone, PartialEq)]
pub struct ParseErrorError;

impl From<ParseErrorNoMatch> for ParseError {
    fn from(_: ParseErrorNoMatch) -> Self {
        ParseError::NoMatch
    }
}

impl From<ParseErrorError> for ParseError {
    fn from(_: ParseErrorError) -> Self {
        ParseError::Error
    }
}

pub type ParseResult<T> = Result<T, ParseError>;
pub type ParsedOptional<T> = Result<T, ParseErrorNoMatch>;
pub type Attempted<T> = Result<T, ParseErrorError>;

pub trait AstItem {
    const NAME: &'static str;

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized;

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector);
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CheckingPhaseEnv {
    pub inside_type_only: bool,
    pub inside_nested_expr: bool,
}

impl CheckingPhaseEnv {
    pub fn new() -> Self {
        Self {
            inside_type_only: false,
            inside_nested_expr: false,
        }
    }

    pub fn inside_type_only(self) -> Self {
        Self {
            inside_type_only: true,
            ..self
        }
    }

    pub fn inside_nested_expr(self) -> Self {
        Self {
            inside_nested_expr: true,
            ..self
        }
    }

    pub fn outside_type_only(self) -> Self {
        Self {
            inside_type_only: false,
            ..self
        }
    }

    pub fn outside_nested_expr(self) -> Self {
        Self {
            inside_nested_expr: false,
            ..self
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParsingPhaseEnv {
    pub inside_nested_expr: bool,
}

impl ParsingPhaseEnv {
    pub fn new() -> Self {
        Self {
            inside_nested_expr: false,
        }
    }

    pub fn inside_nested_expr(self) -> Self {
        Self {
            inside_nested_expr: true,
            ..self
        }
    }

    pub fn outside_nested_expr(self) -> Self {
        Self {
            inside_nested_expr: false,
            ..self
        }
    }
}

pub struct ErrorCollector {
    errors: Vec<CompilerError>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn push(&mut self, error: CompilerError) {
        self.errors.push(error);
    }

    pub fn extend(&mut self, errors: Vec<CompilerError>) {
        self.errors.extend(errors);
    }

    pub fn errors(&self) -> &[CompilerError] {
        &self.errors
    }
}

struct ErrorRecoveryTokenMatcher<T: ParseSimpleToken>(std::marker::PhantomData<T>);

impl<T: ParseSimpleToken> std::fmt::Debug for ErrorRecoveryTokenMatcher<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ErrorRecoveryTokenMatcher")
            .field("token", &T::displayed())
            .finish()
    }
}

pub trait ErrorRecoveryTokenMatch: std::fmt::Debug {
    fn matches(&self, reader: &mut TokenReader) -> bool;
}

impl<T: ParseSimpleToken> ErrorRecoveryTokenMatch for ErrorRecoveryTokenMatcher<T> {
    fn matches(&self, reader: &mut TokenReader) -> bool {
        reader.peek::<T>()
    }
}

#[derive(Debug)]
pub enum ErrorRecoveryMode {
    UntilEnd,
    UntilN(usize),
    UntilToken(Box<dyn ErrorRecoveryTokenMatch>),
}

impl ErrorRecoveryMode {
    pub fn until_end() -> Self {
        Self::UntilEnd
    }

    pub fn until_n(n: usize) -> Self {
        Self::UntilN(n)
    }

    pub fn until_token<T: 'static + ParseSimpleToken>() -> Self {
        Self::UntilToken(Box::new(ErrorRecoveryTokenMatcher(
            std::marker::PhantomData::<T>,
        )))
    }

    fn should_stop(&self, reader: &mut TokenReader, passed: usize) -> bool {
        match self {
            ErrorRecoveryMode::UntilEnd => reader.is_ended(),
            ErrorRecoveryMode::UntilN(n) => passed > *n,
            ErrorRecoveryMode::UntilToken(matcher) => {
                let mut reader_clone = reader.clone();
                let matches = matcher.matches(&mut reader_clone);
                if matches {
                    reader.skip(1);
                }
                matches
            }
        }
    }

    fn should_rollback(&self) -> bool {
        match self {
            ErrorRecoveryMode::UntilEnd => false,
            ErrorRecoveryMode::UntilToken(_) => false,
            ErrorRecoveryMode::UntilN(_) => true,
        }
    }
}

#[derive(Debug)]
struct AstParserFrame {
    current_error_recovery_mode: ErrorRecoveryMode,
}

impl Default for AstParserFrame {
    fn default() -> Self {
        Self {
            current_error_recovery_mode: ErrorRecoveryMode::until_n(3),
        }
    }
}

pub struct AstParser<'a> {
    curr_frame: AstParserFrame,
    input: TokenReader<'a>,
    errors: ErrorCollector,
}

impl<'a> AstParser<'a> {
    pub fn new(input: TokenReader<'a>) -> Self {
        Self {
            curr_frame: AstParserFrame::default(),
            input,
            errors: ErrorCollector::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.input.is_ended()
    }

    pub fn errors(&self) -> &[CompilerError] {
        self.errors.errors()
    }

    pub fn input(&self) -> &TokenReader<'a> {
        &self.input
    }

    pub fn add_error(&mut self, error: CompilerError) {
        self.errors.push(error);
    }

    pub fn set_error_recovery_mode(&mut self, lookahead: ErrorRecoveryMode) {
        self.curr_frame.current_error_recovery_mode = lookahead;
    }

    fn save_frame(&mut self) -> AstParserFrame {
        let frame = std::mem::replace(&mut self.curr_frame, AstParserFrame::default());
        frame
    }

    fn restore_frame(&mut self, frame: AstParserFrame) {
        self.curr_frame = frame;
    }

    pub fn parse_optional_token<T: ParseSimpleToken>(&mut self) -> ParsedOptional<T> {
        let mut reader = self.input.clone();
        let token = T::parse(&mut reader);
        if token.is_some() {
            self.input = reader;
            debug!("Parsed optional token: {}", T::displayed());
            ParsedOptional::Ok(token.unwrap())
        } else {
            debug!("Skipped optional token: {}", T::displayed());
            Err(ParseErrorNoMatch)
        }
    }

    pub fn search_until_token<T: ParseSimpleToken>(&mut self) -> Span {
        let error_start = self.input.span().clone();
        let mut error_end = error_start.clone();

        let mut i = 1;
        while !self.input.is_ended() && !self.input.peek::<T>() {
            let lookahead = &self.curr_frame.current_error_recovery_mode;
            if lookahead.should_stop(&mut self.input, i) {
                break;
            }

            self.input.skip(1);
            error_end = self.input.span().clone();
            i += 1;
        }

        error_start.join(&error_end)
    }

    pub fn parse_required_token<T: ParseSimpleToken>(&mut self) -> Attempted<T> {
        let reader_unchanged = self.input.clone();

        let token = T::parse(&mut self.input);

        if let Some(token) = token {
            debug!("Parsed token: {}", T::displayed());
            return Attempted::Ok(token);
        };

        let error_start = reader_unchanged.span().clone();
        let mut error_end = error_start.clone();

        if !self.input.is_ended() {
            self.input.skip(1);
        }

        let mut i = 1;
        while !self.input.is_ended() && !self.input.peek::<T>() {
            let lookahead = &self.curr_frame.current_error_recovery_mode;
            if lookahead.should_stop(&mut self.input, i) {
                error_end = self.input.span().clone();
                break;
            }

            error_end = self.input.span().clone();
            self.input.skip(1);
            i += 1;
        }

        debug!(
            "Error parsing required token: {} span {:?}",
            T::displayed(),
            error_start.join(&error_end)
        );
        self.errors.push(CompilerError::new(
            format!("Expected {}", T::displayed(),),
            error_start.join(&error_end),
        ));

        let token = T::parse(&mut self.input);

        if let Some(token) = token {
            // Found value, update reader
            Attempted::Ok(token)
        } else {
            // Didn't find the value, update reader if needed
            if self
                .curr_frame
                .current_error_recovery_mode
                .should_rollback()
            {
                self.input = reader_unchanged;
            }
            Attempted::Err(ParseErrorError)
        }
    }

    pub fn parse_optional_group<T: ParseGroupToken, I: AstItem>(
        &mut self,
        env: ParsingPhaseEnv,
    ) -> ParseResult<(T, I)> {
        let outer_unchanged_reader = self.input.clone();
        let mut outer_changed_reader = self.input.clone();

        let token = T::parse(&mut outer_changed_reader);
        if let Some((token, inner_reader)) = token {
            self.input = inner_reader;

            let result = self.parse_required::<I>(env);

            self.input = outer_changed_reader;
            match result {
                Attempted::Ok(item) => ParseResult::Ok((token, item)),
                Attempted::Err(ParseErrorError) => ParseResult::Err(ParseError::Error),
            }
        } else {
            self.input = outer_unchanged_reader;
            ParseResult::Err(ParseError::NoMatch)
        }
    }

    pub fn parse_required_group<T: ParseGroupToken, I: AstItem>(
        &mut self,
        env: ParsingPhaseEnv,
    ) -> Attempted<(T, I)> {
        let reader_unchanged = self.input.clone();

        let result = self.parse_optional_group::<T, I>(env);

        if let Ok(result) = result {
            return Attempted::Ok(result);
        };

        if self.input.is_ended() {
            self.errors.push(CompilerError::new(
                format!("Expected {}", I::NAME),
                self.input.span().clone(),
            ));
        }

        let error_start = self.input.span().clone();
        let mut error_end = error_start.clone();

        if !self.input.is_ended() {
            self.input.skip(1);
        }

        let mut found_item = None;

        let mut i = 1;
        while !self.input.is_ended() {
            let lookahead = &self.curr_frame.current_error_recovery_mode;
            if lookahead.should_stop(&mut self.input, i) {
                error_end = self.input.span().clone();
                break;
            }

            let item = self.parse_optional_group::<T, I>(env);
            match item {
                Ok(item) => {
                    found_item = Some(item);
                    break;
                }
                Err(ParseError::Error) => {
                    error_end = self.input.span().clone();
                    break;
                }
                Err(ParseError::NoMatch) => {
                    error_end = self.input.span().clone();
                    self.input.skip(1);
                    i += 1;
                }
            }
        }

        self.errors.push(CompilerError::new(
            format!("Expected {}", T::displayed()),
            error_start.join(&error_end),
        ));

        if let Some(item) = found_item {
            // Found value
            return Attempted::Ok(item);
        } else {
            // Didn't find value, restore if needed
            if self
                .curr_frame
                .current_error_recovery_mode
                .should_rollback()
            {
                self.input = reader_unchanged;
            }
            return Attempted::Err(ParseErrorError);
        }
    }

    pub fn parse_optional<T: AstItem>(&mut self, env: ParsingPhaseEnv) -> ParseResult<T> {
        debug!("Entering optional: {}", T::NAME);

        let frame = self.save_frame();
        let prev_reader = self.input.clone();

        let item = T::parse(self, env);

        if let ParseResult::Err(err) = &item {
            match err {
                ParseError::NoMatch => {
                    self.input = prev_reader;
                }
                ParseError::Error => {
                    // Don't restore
                }
            }
        }

        self.restore_frame(frame);

        if item.is_ok() {
            debug!("Parsed optional: {}", T::NAME);
        } else {
            debug!("Skipped optional: {}", T::NAME);
        }

        item
    }

    pub fn parse_required<T: AstItem>(&mut self, env: ParsingPhaseEnv) -> Attempted<T> {
        debug!("Entering required: {}", T::NAME);
        debug!("MODE: {:?}", &self.curr_frame.current_error_recovery_mode);

        let item = self.parse_optional::<T>(env);

        match item {
            ParseResult::Ok(item) => {
                debug!("Parsed required: {}", T::NAME);
                return Attempted::Ok(item);
            }
            ParseResult::Err(ParseError::Error) => {
                debug!("Error parsing required item: {}", T::NAME);
                return Attempted::Err(ParseErrorError);
            }
            ParseResult::Err(ParseError::NoMatch) => {
                let error_start = self.input.span().clone();
                let mut error_end = error_start.clone();

                let prev_reader = self.input.clone();
                if !self.input.is_ended() {
                    self.input.skip(1);
                }

                let mut error = false;
                let mut item = None;

                let mut i = 1;
                while !self.input.is_ended() {
                    let lookahead = &self.curr_frame.current_error_recovery_mode;
                    if lookahead.should_stop(&mut self.input, i) {
                        error_end = self.input.span().clone();
                        break;
                    }

                    let token = self.parse_optional::<T>(env);
                    match token {
                        ParseResult::Ok(found) => {
                            item = Some(found);
                            break;
                        }
                        ParseResult::Err(ParseError::Error) => {
                            error_end = self.input.span().clone();
                            error = true;
                            break;
                        }
                        ParseResult::Err(ParseError::NoMatch) => {
                            error_end = self.input.span().clone();
                            self.input.skip(1);
                            i += 1;
                        }
                    }
                }

                debug!(
                    "Error parsing required item: {} span {:?}",
                    T::NAME,
                    error_start.join(&error_end)
                );
                self.errors.push(CompilerError::new(
                    format!("Expected {}", T::NAME,),
                    error_start.join(&error_end),
                ));

                if let Some(item) = item {
                    // Found the value, return it
                    return Attempted::Ok(item);
                } else if error {
                    // Got an error, don't restore
                    return Attempted::Err(ParseErrorError);
                } else {
                    // Didn't find the value, restore if needed
                    if self
                        .curr_frame
                        .current_error_recovery_mode
                        .should_rollback()
                    {
                        self.input = prev_reader;
                    }
                    return Attempted::Err(ParseErrorError);
                }
            }
        }
    }
}
