use super::{
    tokens::{ParseGroupToken, ParseSimpleToken, Span, TokenReader},
    CompilerError,
};

pub mod types;

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

    fn parse<'a>(reader: &mut AstParser<'a>) -> ParseResult<Self>
    where
        Self: Sized;
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct AstParserFrame {
    current_error_lookahead: Option<usize>,
}

impl Default for AstParserFrame {
    fn default() -> Self {
        Self {
            current_error_lookahead: Some(3),
        }
    }
}

pub struct AstParser<'a> {
    curr_frame: AstParserFrame,
    input: TokenReader<'a>,
    errors: Vec<CompilerError>,
}

impl<'a> AstParser<'a> {
    pub fn new(input: TokenReader<'a>) -> Self {
        Self {
            curr_frame: AstParserFrame::default(),
            input,
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &[CompilerError] {
        &self.errors
    }

    fn set_error_lookahead(&mut self, lookahead: Option<usize>) {
        self.curr_frame.current_error_lookahead = lookahead;
    }

    fn save_frame(&mut self) -> AstParserFrame {
        let frame = self.curr_frame.clone();
        self.curr_frame = AstParserFrame::default();
        frame
    }

    fn restore_frame(&mut self, frame: AstParserFrame) {
        self.curr_frame = frame;
    }

    fn parse_optional_token<T: ParseSimpleToken>(&mut self) -> ParsedOptional<T> {
        let mut reader = self.input.clone();
        let token = T::parse(&mut reader);
        if token.is_some() {
            self.input = reader;
            ParsedOptional::Ok(token.unwrap())
        } else {
            Err(ParseErrorNoMatch)
        }
    }

    fn parse_required_token<T: ParseSimpleToken>(&mut self) -> Attempted<T> {
        let mut reader = self.input.clone();
        let token = reader.parse_simple::<T>();

        if let Some(token) = token {
            self.input = reader;
            return Attempted::Ok(token);
        };

        if reader.remaining_len() == 0 {
            self.errors.push(CompilerError::new(
                format!("Expected {}", T::displayed(),),
                reader.span().clone(),
            ));
        }

        let error_start = self.input.span();
        let mut error_end = error_start;

        reader.skip(1);

        let mut i = 1;
        while reader.remaining_len() > 0 && !reader.peek::<T>() {
            let lookahead = self.curr_frame.current_error_lookahead;
            if let Some(max) = lookahead {
                if i >= max {
                    break;
                }
            }

            reader.skip(1);
            error_end = reader.span();
            i += 1;
        }

        self.errors.push(CompilerError::new(
            format!("Expected {}", T::displayed(),),
            error_start.join(&error_end),
        ));

        let token = reader.parse_simple::<T>();

        if let Some(token) = token {
            self.input = reader;
            Attempted::Ok(token)
        } else {
            Attempted::Err(ParseErrorError)
        }
    }

    fn parse_optional_group<T: ParseGroupToken, I: AstItem>(&mut self) -> ParseResult<T> {
        let outer_unchanged_reader = self.input.clone();
        let mut outer_changed_reader = self.input.clone();

        let token = T::parse(&mut outer_changed_reader);
        if let Some((token, inner_reader)) = token {
            let outer_changed_reader = self.input.clone();

            self.input = inner_reader;

            let result = self.parse_required::<I>();

            self.input = outer_changed_reader;
            match result {
                Attempted::Ok(item) => ParseResult::Ok(token),
                Attempted::Err(ParseErrorError) => ParseResult::Err(ParseError::Error),
            }
        } else {
            self.input = outer_unchanged_reader;
            ParseResult::Err(ParseError::NoMatch)
        }
    }

    fn parse_required_group<T: ParseGroupToken, I: AstItem>(&mut self) -> Attempted<T> {
        let reader_unchanged = self.input.clone();

        let item = self.parse_optional_group::<T, I>();

        if let Ok(token) = item {
            return Attempted::Ok(token);
        };

        if self.input.remaining_len() == 0 {
            self.errors.push(CompilerError::new(
                format!("Expected {}", I::NAME,),
                self.input.span().clone(),
            ));
        }

        let error_start = self.input.span().clone();
        let mut error_end = error_start.clone();

        self.input.skip(1);

        let mut found_item = None;

        let mut i = 1;
        while self.input.remaining_len() > 0 {
            let lookahead = self.curr_frame.current_error_lookahead;
            if let Some(max) = lookahead {
                if i >= max {
                    break;
                }
            }

            let item = self.parse_optional_group::<T, I>();
            match item {
                Ok(item) => {
                    found_item = Some(item);
                    break;
                }
                Err(ParseError::Error) => {
                    break;
                }
                Err(ParseError::NoMatch) => {
                    self.input.skip(1);
                    error_end = self.input.span().clone();
                    i += 1;
                }
            }
        }

        self.errors.push(CompilerError::new(
            format!("Expected {}", T::displayed(),),
            error_start.join(&error_end),
        ));

        if let Some(item) = found_item {
            return Attempted::Ok(item);
        } else {
            self.input = reader_unchanged;
            return Attempted::Err(ParseErrorError);
        }
    }

    fn parse_optional<T: AstItem>(&mut self) -> ParseResult<T> {
        let frame = self.save_frame();
        let prev_reader = self.input.clone();

        let item = T::parse(self);

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

        item
    }

    fn parse_required<T: AstItem>(&mut self) -> Attempted<T> {
        let item = self.parse_optional::<T>();

        match item {
            ParseResult::Ok(item) => {
                return Attempted::Ok(item);
            }
            ParseResult::Err(ParseError::Error) => {
                return Attempted::Err(ParseErrorError);
            }
            ParseResult::Err(ParseError::NoMatch) => {
                let error_start = self.input.span().clone();
                let mut error_end = error_start.clone();

                let prev_reader = self.input.clone();
                self.input.skip(1);

                let mut error = false;
                let mut item = None;

                let mut i = 1;
                while self.input.remaining_len() > 0 {
                    if let Some(max) = self.curr_frame.current_error_lookahead {
                        if i >= max {
                            break;
                        }
                    }

                    let token = self.parse_optional::<T>();
                    match token {
                        ParseResult::Ok(found) => {
                            item = Some(found);
                            break;
                        }
                        ParseResult::Err(ParseError::Error) => {
                            error = true;
                            break;
                        }
                        ParseResult::Err(ParseError::NoMatch) => {
                            self.input.skip(1);
                            error_end = self.input.span().clone();
                            i += 1;
                        }
                    }
                }

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
                    // Didn't find the value, restore
                    self.input = prev_reader;
                    return Attempted::Err(ParseErrorError);
                }
            }
        }
    }
}
