use std::str::FromStr;

use regex_lexer::{Lexer, LexerBuilder};

use crate::{ast::StringContext, parse::Token, verify::Type};

#[derive(Debug, Clone, Copy)]
enum TokenType {
    //Ops
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    //Types
    Int,
    Int32,
    String,
    Ident,
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
            //Other
            TokenType::LParen => Token::LParen(ctx),
            TokenType::RParen => Token::RParen(ctx),
            TokenType::LBracket => Token::LBracket(ctx),
            TokenType::RBracket => Token::RBracket(ctx),
            TokenType::Semicolon => Token::Semicolon(ctx),
            TokenType::Comma => Token::Comma(ctx),
            TokenType::Arrow => Token::Arrow(ctx),
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
            .token(r"\+", Add)
            .token(r"-", Sub)
            .token(r"\*", Mul)
            .token(r"/", Div)
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

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    let token_types: Vec<_> = LEXER.tokens(input).collect();

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
