use std::{ops::Range, str::FromStr};

use regex_lexer::{Lexer, LexerBuilder};

use crate::{ast::StringContext, parse::Token, verify::Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokenType {
    //Ops
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    //Cmp
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    //Types
    Int,
    Int32,
    String,
    Ident,
    MethodCallStart,
    /// A declaration which represents an explicit type
    TypeDec(Type),
    //Keywords
    Let, //`let`
    Fn,  //`fn`
    //Misc
    LParen,
    RParen,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Arrow, //`->`
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
    FailedToParse { ctx: StringContext },
}

fn try_parse<F>(val: &str, ctx: StringContext) -> Result<F, TokenizeError>
where
    F: FromStr,
{
    val.parse()
        .map_err(|e| TokenizeError::FailedToParse { ctx })
}

impl TokenType {
    fn into_token(self, ctx: StringContext, text: &str) -> Result<Token, TokenizeError> {
        Ok(match self {
            //Tokens with fields
            TokenType::Int => Token::Int((ctx.clone(), try_parse(text, ctx)?)),
            TokenType::Int32 => {
                //Get rid of the `i32`
                Token::Int32((
                    ctx.clone(),
                    try_parse(text.rsplit_once("i").unwrap().0, ctx)?,
                ))
            }
            TokenType::String => Token::String((ctx, text.to_string())),
            TokenType::Ident => Token::Ident((ctx, text.to_string())),

            //Trivial tokens (only needing a context)

            //Keywords
            TokenType::Let => Token::Let(ctx),
            TokenType::Fn => Token::Fn(ctx),
            TokenType::TypeDec(t) => Token::TypeDec((ctx, t)),
            //Ops
            TokenType::Add => Token::Add(ctx),
            TokenType::Sub => Token::Sub(ctx),
            TokenType::Mul => Token::Mul(ctx),
            TokenType::Div => Token::Div(ctx),
            TokenType::Assign => Token::Assign(ctx),
            //Cmp
            TokenType::Eq => Token::Eq(ctx),
            TokenType::Lt => Token::Lt(ctx),
            TokenType::Gt => Token::Gt(ctx),
            TokenType::Le => Token::Le(ctx),
            TokenType::Ge => Token::Ge(ctx),
            //Other
            TokenType::LParen => Token::LParen(ctx),
            TokenType::RParen => Token::RParen(ctx),
            TokenType::LBracket => Token::LBracket(ctx),
            TokenType::RBracket => Token::RBracket(ctx),
            TokenType::Semicolon => Token::Semicolon(ctx),
            TokenType::Comma => Token::Comma(ctx),
            TokenType::Arrow => Token::Arrow(ctx),
            TokenType::MethodCallStart => {
                //The text is just the identifier without the `(` at the end
                // Token::MethodCallStart((ctx, text.to_string()[0..(text.len() - 2)].into()))
                Token::MethodCallStart((ctx, text.to_string()))
            }
        })
    }
}

lazy_static! {
    static ref LEXER: Lexer<TokenType> = {
        use TokenType::*;
        //Define the regex for all tokens
        //Note that conflicts are resolved by the latest getting priority
        LexerBuilder::new()
            //Identifiers
            .token(r"[[:alpha:]_]+", Ident)

            //Operators and such
            //Arithmetic
            .token(r"\+", Add)
            .token(r"-", Sub)
            .token(r"\*", Mul)
            .token(r"/", Div)
            //Cmp
            .token(r"==", Eq)
            .token(r"<", Lt)
            .token(r">", Gt)
            .token(r"<=", Le)
            .token(r">=", Ge)
            //Misc
            .token(r"\(", LParen)
            .token(r"\)", RParen)
            .token(r"[;]", Semicolon)
            .token(r"[{]", LBracket)
            .token(r"[}]", RBracket)
            .token(r"=", Assign)
            .token(r",", Comma)

            //Keywords
            .token(r"let", Let)
            .token(r"fn", Fn)
            .token(r"->", Arrow)
            .token(r"i32", TypeDec(Type::Int32))

            //Basic type literals
            .token(r"[-+]?[0-9]+", Int)
            .token(r"[-+]?[0-9]+i32", Int32)
            //.token(r"[-+]?[0-9]+\.[0-9]*", Float)

            //Strings
            .token(r#"".*""#, String)
            //Comments
            .ignore(r"//.*")

            //Finishing up
            .ignore(r"\s+")
            .build()
            .expect("Lexer failed to build")
    };
}

/// Does trivial parsing, such as collapsing "ident `(` `)`" into `MethodCall`
fn pre_parse(
    tokens: Vec<regex_lexer::Token<TokenType>>,
) -> Result<Vec<regex_lexer::Token<TokenType>>, TokenizeError> {
    use TokenType::*;
    let patterns = vec![
        (vec![Ident, LParen], MethodCallStart), //
    ];

    let mut out_tokens = Vec::with_capacity(tokens.len());

    let mut i = 0;
    while i < tokens.len() {
        //The token which has been matched from a pattern and the length of that pattern.
        let mut matched_tok = None;
        //See if each token matches a pattern.
        //If so, skip all tokens involved in the pattern and replace it
        for (pattern, out_tok) in patterns.clone() {
            //If the pattern is too long for the rest of the sequence, it won't match
            if i + pattern.len() > tokens.len() {
                continue;
            }
            //
            let mut is_match = true;
            for pattern_idx in 0..pattern.len() {
                let pattern_tok = pattern[pattern_idx];
                //If the token doesn't match the pattern, then the pattern doesn't match
                if pattern_tok != tokens[pattern_idx + i].kind {
                    is_match = false;
                    break;
                }
            }

            if is_match {
                matched_tok = Some((out_tok, pattern.len()));
                break;
            }
        }

        // If the pattern was a match, then ignore all other patterns and continue as such
        if let Some((tok, len)) = matched_tok {
            out_tokens.push(regex_lexer::Token {
                kind: tok,
                span: Range {
                    start: tokens[i].span.start,
                    end: tokens[i + len].span.end,
                },
                text: tokens[i].text,
            });
            i += len;
        } else {
            out_tokens.push(tokens[i].clone());
            i += 1;
        }
    }

    Ok(out_tokens)
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    let token_types: Vec<_> = LEXER.tokens(input).collect();

    let token_types = pre_parse(token_types)?;

    let tokens = {
        token_types
            .into_iter()
            .map(|tok| {
                let context = StringContext::new(tok.span, input);
                tok.kind.into_token(context, tok.text)
            })
            .collect::<Result<_, _>>()?
    };

    Ok(tokens)
}
