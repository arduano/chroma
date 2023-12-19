use crate::lang::WithSpan;

use super::{GroupedTokenList, Span, TestTokenValue, TokenList, TokenValue};

#[derive(Clone)]
pub struct TokenReader<'a> {
    tree: &'a TokenList,
    index: usize,
    start_cap: Span,
    end_cap: Span,
}

impl<'a> TokenReader<'a> {
    pub fn new(tokens: &'a TokenList) -> Self {
        Self {
            tree: tokens,
            index: 0,

            // Get the span of either the last token, or the whole token list (if empty).
            start_cap: tokens
                .value
                .first()
                .map(|t| &t.span)
                .unwrap_or(&tokens.span)
                .clone(),
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
            start_cap: tokens.left_cap.clone(),
            end_cap: tokens.right_cap.clone(),
        }
    }

    pub fn span(&self) -> &Span {
        self.next_token()
            .map(|t| &t.span)
            .unwrap_or_else(|| &self.end_cap)
    }

    pub fn prev_span(&self) -> &Span {
        self.prev_token()
            .map(|t| &t.span)
            .unwrap_or_else(|| &self.start_cap)
    }

    pub fn prev_prev_span(&self) -> &Span {
        self.prev_prev_token()
            .map(|t| &t.span)
            .unwrap_or_else(|| &self.start_cap)
    }

    pub fn peek<T: TestTokenValue>(&self) -> bool {
        T::test(self)
    }

    pub fn remaining_tokens_slice(&self) -> &'a [WithSpan<TokenValue>] {
        &self.tree.value[self.index..]
    }

    pub fn next_token(&self) -> Option<&'a WithSpan<TokenValue>> {
        self.remaining_tokens_slice().first()
    }

    pub fn prev_token(&self) -> Option<&'a WithSpan<TokenValue>> {
        if self.index == 0 {
            return None;
        }

        self.tree.value.get(self.index - 1)
    }

    pub fn prev_prev_token(&self) -> Option<&'a WithSpan<TokenValue>> {
        if self.index <= 1 {
            return None;
        }

        self.tree.value.get(self.index - 2)
    }

    pub fn remaining_len(&self) -> usize {
        self.tree.value.len() - self.index
    }

    pub fn is_ended(&self) -> bool {
        let mut trimming = self.clone();
        trimming.trim_empty();
        trimming.remaining_len() == 0
    }

    pub fn skip(&mut self, count: usize) {
        assert!(self.remaining_len() >= count);
        self.index += count;
    }

    pub fn trim_empty(&mut self) {
        loop {
            let next = self.next_token().map(|s| &s.value);
            let Some(next) = next else {
                break;
            };

            if next != &TokenValue::Whitespace
                && next != &TokenValue::Newline
                && next != &TokenValue::Comment
            {
                break;
            }

            self.skip(1);
        }
    }
}
