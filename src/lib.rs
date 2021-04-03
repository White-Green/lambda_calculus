use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

use parser::{LR1Parser, Parse, ParseError, Rule, Syntax};
use parser::enum_index;
use parser::enum_index_derive::*;
use parser::Symbol::*;
use tokenizer::{DFATokenizer, Tokenize};

#[derive(Debug, EnumIndex)]
enum Token {
    Lambda(String),
    Identifier(String),
    Bracket,
    CloseBracket,
}

#[derive(Debug, EnumIndex, Clone)]
pub enum Expression {
    Expr(Box<Expression>),
    Item(Box<Expression>),
    Variable(String),
    Lambda(String, Box<Expression>),
    Apply(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SimpleExpression {
    Variable(String),
    Lambda(String, Box<SimpleExpression>),
    Apply(Box<SimpleExpression>, Box<SimpleExpression>),
}

impl SimpleExpression {
    pub fn reduction_first(self, variables: &mut BTreeMap<String, SimpleExpression>) -> Result<Self, Self> {
        match self {
            SimpleExpression::Variable(name) => if let Some(expr) = variables.get(&name) {
                Ok(expr.clone())
            } else {
                Err(SimpleExpression::Variable(name))
            }
            SimpleExpression::Lambda(name, body) => {
                let buffer = variables.remove(&name);
                let ret = match (*body).reduction_first(variables) {
                    Ok(body) => Ok(SimpleExpression::Lambda(name.clone(), Box::new(body))),
                    Err(body) => Err(SimpleExpression::Lambda(name.clone(), Box::new(body)))
                };
                if let Some(value) = buffer {
                    variables.insert(name, value);
                }
                ret
            }
            SimpleExpression::Apply(f, a) => {
                match ((*f).reduction_first(variables), (*a).reduction_first(variables)) {
                    | (Ok(f), Ok(a))
                    | (Ok(f), Err(a))
                    | (Err(f), Ok(a)) => Ok(SimpleExpression::Apply(Box::new(f), Box::new(a))),
                    (Err(f), Err(SimpleExpression::Variable(name))) => Err(SimpleExpression::Apply(Box::new(f), Box::new(SimpleExpression::Variable(name)))),
                    (Err(SimpleExpression::Lambda(name, body)), Err(a)) => {
                        let buffer = variables.insert(name.clone(), a);
                        let ret = match (*body).reduction_first(variables) {
                            Ok(b) => b,
                            Err(b) => b,
                        };
                        if let Some(value) = buffer {
                            variables.insert(name, value);
                        } else {
                            variables.remove(&name);
                        }
                        Ok(ret)
                    }
                    (Err(f), Err(a)) => Err(SimpleExpression::Apply(Box::new(f), Box::new(a))),
                }
            }
        }
    }
}

impl From<Expression> for SimpleExpression {
    fn from(expr: Expression) -> Self {
        match expr {
            Expression::Expr(expr) => (*expr).into(),
            Expression::Item(expr) => (*expr).into(),
            Expression::Variable(name) => SimpleExpression::Variable(name),
            Expression::Lambda(arg, body) => SimpleExpression::Lambda(arg, Box::new((*body).into())),
            Expression::Apply(a, b) => SimpleExpression::Apply(Box::new((*a).into()), Box::new((*b).into()))
        }
    }
}

impl Display for SimpleExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleExpression::Variable(name) => write!(f, "{}", name),
            SimpleExpression::Lambda(arg, body) => write!(f, "(λ{}. {})", arg, body),
            SimpleExpression::Apply(a, b) => write!(f, "({} {})", a, b),
        }
    }
}

impl Default for Expression {
    fn default() -> Self {
        Expression::Variable(String::default())
    }
}

pub struct LambdaCalculusEngine {
    tokenizer: DFATokenizer<Option<Token>, char>,
    parser: LR1Parser<Expression, Token>,
}

impl LambdaCalculusEngine {
    pub fn new() -> Self {
        let (tokenizer, _) = DFATokenizer::builder()
            .pattern("/\\w+\\.", |s, _| {
                Some(Token::Lambda(s[1..s.len() - 1].to_owned()))
            })
            .pattern("\\w+", |s, _| {
                Some(Token::Identifier(s.to_owned()))
            })
            .pattern("\\(", |_, _| Some(Token::Bracket))
            .pattern("\\)", |_, _| Some(Token::CloseBracket))
            .pattern("\\s+", |_, _| None)
            .build()
            .unwrap();
        // Shift-Reduce衝突起きてるけどまあ動いてるからヨシ
        let (parser, _) = LR1Parser::new(Syntax::builder()
            .rule(Rule::new(Expression::Expr(Box::default()),
                            &[NonTerminal(Expression::Item(Box::default()))],
                            |symbols| if let [NonTerminal(Expression::Item(expr))] = symbols {
                                Expression::Expr(std::mem::take(expr))
                            } else { unreachable!() }))
            .rule(Rule::new(Expression::Item(Box::default()),
                            &[NonTerminal(Expression::Variable(String::default()))],
                            |symbols| if let [NonTerminal(variable)] = symbols {
                                Expression::Item(Box::new(std::mem::replace(variable, Expression::Variable(String::default()))))
                            } else { unreachable!() }))
            .rule(Rule::new(Expression::Item(Box::default()),
                            &[Terminal(Token::Bracket), NonTerminal(Expression::Expr(Box::default())), Terminal(Token::CloseBracket)],
                            |symbols| if let [_, NonTerminal(Expression::Expr(expr)), _] = symbols {
                                Expression::Item(std::mem::take(expr))
                            } else { unreachable!() }))
            .rule(Rule::new(Expression::Expr(Box::default()),
                            &[NonTerminal(Expression::Apply(Box::default(), Box::default()))],
                            |symbols| if let [NonTerminal(apply)] = symbols {
                                Expression::Expr(Box::new(std::mem::replace(apply, Expression::Apply(Box::default(), Box::default()))))
                            } else { unreachable!() }))
            .rule(Rule::new(Expression::Item(Box::default()),
                            &[NonTerminal(Expression::Lambda(String::default(), Box::new(Expression::default())))],
                            |symbols| if let [NonTerminal(lambda)] = symbols {
                                Expression::Item(Box::new(std::mem::replace(lambda, Expression::Lambda(String::default(), Box::default()))))
                            } else { unreachable!() }))

            .rule(Rule::new(Expression::Variable(String::default()),
                            &[Terminal(Token::Identifier(String::default()))],
                            |symbols| if let [Terminal(Token::Identifier(s))] = symbols {
                                Expression::Variable(s.to_owned())
                            } else { unreachable!() }))
            .rule(Rule::new(Expression::Lambda(String::default(), Box::default()),
                            &[Terminal(Token::Lambda(String::default())), NonTerminal(Expression::Expr(Box::default()))],
                            |symbols| if let [Terminal(Token::Lambda(s)), NonTerminal(Expression::Expr(b))] = symbols {
                                Expression::Lambda(s.to_owned(), std::mem::take(b))
                            } else { unreachable!() }))
            .rule(Rule::new(Expression::Apply(Box::default(), Box::default()),
                            &[NonTerminal(Expression::Expr(Box::default())), NonTerminal(Expression::Item(Box::default()))],
                            |symbols| if let [NonTerminal(Expression::Expr(f)), NonTerminal(Expression::Item(a))] = symbols {
                                Expression::Apply(std::mem::take(f), std::mem::take(a))
                            } else { unreachable!() }))
            .build(Expression::Expr(Box::default())));
        Self { tokenizer, parser }
    }

    pub fn parse(&self, input: &str) -> Result<SimpleExpression, ParseError> {
        input
            .chars()
            .tokenize(&self.tokenizer)
            .flatten()
            .parse(&self.parser)
            .map(Into::into)
    }
}
