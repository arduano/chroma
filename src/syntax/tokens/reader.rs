use crate::syntax::WithSpan;

use super::{
    GroupedTokenList, ParseGroupToken, ParseSimpleToken, Span, TestTokenValue, TokenList,
    TokenValue,
};

#[derive(Clone)]
pub struct TokenReader<'a> {
    tree: &'a TokenList,
    index: usize,
    end_cap: Span,
}

impl<'a> TokenReader<'a> {
    pub fn new(tokens: &'a TokenList) -> Self {
        Self {
            tree: tokens,
            index: 0,

            // Get the span of either the last token, or the whole token list (if empty).
            end_cap: tokens
                .value
                .last()
                .map(|t| &t.span)
                .unwrap_or(&tokens.span)
                .clone(),
        }
    }

    pub fn new_grpuped(tokens: &'a GroupedTokenList) -> Self {
        Self {
            tree: &tokens.tokens,
            index: 0,
            end_cap: tokens.right_cap.clone(),
        }
    }

    pub fn span(&self) -> &Span {
        self.next_token()
            .map(|t| &t.span)
            .unwrap_or_else(|| &self.end_cap)
    }

    pub fn peek<T: TestTokenValue>(&self) -> bool {
        T::test(self)
    }

    pub fn peek_n<T: TestTokenValue>(&self, ahead: usize) -> bool {
        if self.remaining_len() <= ahead {
            return false;
        }
        let mut other = self.clone();
        other.skip(ahead);
        T::test(&other)
    }

    pub fn peek_2<T: TestTokenValue, U: TestTokenValue>(&self) -> bool {
        self.peek::<T>() && self.peek_n::<U>(T::TOKEN_LEN)
    }

    pub fn peek_3<T: TestTokenValue, U: TestTokenValue, V: TestTokenValue>(&self) -> bool {
        self.peek::<T>()
            && self.peek_n::<U>(T::TOKEN_LEN)
            && self.peek_n::<V>(T::TOKEN_LEN + U::TOKEN_LEN)
    }

    pub fn peek_4<T: TestTokenValue, U: TestTokenValue, V: TestTokenValue, W: TestTokenValue>(
        &self,
    ) -> bool {
        self.peek::<T>()
            && self.peek_n::<U>(T::TOKEN_LEN)
            && self.peek_n::<V>(T::TOKEN_LEN + U::TOKEN_LEN)
            && self.peek_n::<W>(T::TOKEN_LEN + U::TOKEN_LEN + V::TOKEN_LEN)
    }

    pub fn remaining_tokens_slice(&self) -> &'a [WithSpan<TokenValue>] {
        &self.tree.value[self.index..]
    }

    pub fn next_token(&self) -> Option<&'a WithSpan<TokenValue>> {
        self.remaining_tokens_slice().first()
    }

    pub fn remaining_len(&self) -> usize {
        self.tree.value.len() - self.index
    }

    pub fn skip(&mut self, count: usize) {
        assert!(self.remaining_len() >= count);
        self.index += count;
    }

    pub fn parse_simple<T: ParseSimpleToken>(&mut self) -> Option<T> {
        T::parse(self)
    }

    pub fn parse_grouped<T: ParseGroupToken>(&mut self) -> Option<(T, TokenReader<'a>)> {
        T::parse(self)
    }
}
