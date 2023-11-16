use std::str::FromStr;

use chrono::FixedOffset;
use rust_decimal::Decimal as RustDecimal;

/// The main entry point for the parsing model.
/// This is a recursive struct to account for the possible recursive filter specification.
#[derive(Debug, PartialEq)]
pub(crate) enum Expression<'a> {
    Attribute(AttributeExpression<'a>),
    Logical(LogicalExpression<'a>),
    Group(GroupExpression<'a>),
}

/// An attribute expression.
/// It can be either:
///   - Complex like `emails[type eq "work" and value co "@example.com"]`
///   - Simple like `userName eq "ringo"`
///   - Present like `userName pr"`
#[derive(Debug, PartialEq)]
pub(crate) enum AttributeExpression<'a> {
    Complex(ComplexData<'a>),
    Simple(SimpleData<'a>),
    Present(&'a str),
}

/// Parsed data for Complex Attribute Expression
#[derive(Debug, PartialEq)]
pub(crate) struct ComplexData<'a> {
    pub(crate) attribute: &'a str,
    pub(crate) expression: Box<Expression<'a>>,
}

/// Parsed data for Simple Attribute Expression
#[derive(Debug, PartialEq)]
pub(crate) struct SimpleData<'a> {
    pub(crate) attribute: &'a str,
    pub(crate) expression_operator: ExpressionOperatorComparison,
    pub(crate) value: Value<'a>,
}

/// A parsed Value.
/// This is an enum because the value can have many different types. Namely:
///   - String / `"test"`
///   - Boolean / `true` or `false`
///   - DateTime / `2011-05-13T04:42:34Z`
///   - Number / `42` or `3.14`
///   - Binary
#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    String(&'a str),
    Boolean(bool),
    DateTime(chrono::DateTime<FixedOffset>),
    Number(RustDecimal),
    Binary(&'a str),
}

/// A logical expression in the form of xxx (and|or) yyy
/// This is a recursion node, since xxx and yyy could also be expressions
#[derive(Debug, PartialEq)]
pub(crate) struct LogicalExpression<'a> {
    pub(crate) left: Box<Expression<'a>>,
    pub(crate) operator: LogicalOperator,
    pub(crate) right: Box<Expression<'a>>,
}

/// A group expression in the form of `(singer eq "john" and bassist = "paul")`
/// This is a recursion node, since everything inside parens could be an expression
#[derive(Debug, PartialEq)]
pub(crate) struct GroupExpression<'a> {
    pub(crate) not: bool,
    pub(crate) content: Box<Expression<'a>>,
    pub(crate) operator: Option<LogicalOperator>,
    pub(crate) rest: Option<Box<Expression<'a>>>,
}

/// The logical operator `And` or `Or`
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum LogicalOperator {
    And,
    Or,
}

impl LogicalOperator {
    pub fn is_or(&self) -> bool {
        matches!(self, LogicalOperator::Or)
    }

    pub fn is_and(&self) -> bool {
        matches!(self, LogicalOperator::And)
    }
}

impl FromStr for LogicalOperator {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "and" => Ok(Self::And),
            "or" => Ok(Self::Or),
            _ => Err(()),
        }
    }
}

/// An expression operator for an attribute.
/// It's an enum because it could be a comparison (that has a value after) or the present attribute which ends the attribute expression.
#[derive(Debug, PartialEq)]
pub enum ExpressionOperator {
    Comparison(ExpressionOperatorComparison),
    Present,
}

impl FromStr for ExpressionOperator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "pr" {
            return Ok(Self::Present);
        }

        Ok(Self::Comparison(ExpressionOperatorComparison::from_str(s)?))
    }
}

/// An expression operator for a comparison attribute expression.
#[derive(Debug, PartialEq)]
pub enum ExpressionOperatorComparison {
    Equal,
    NotEqual,
    Contains,
    StartsWith,
    EndsWith,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl FromStr for ExpressionOperatorComparison {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "eq" => Ok(Self::Equal),
            "ne" => Ok(Self::NotEqual),
            "co" => Ok(Self::Contains),
            "sw" => Ok(Self::StartsWith),
            "ew" => Ok(Self::EndsWith),
            "gt" => Ok(Self::GreaterThan),
            "ge" => Ok(Self::GreaterThanOrEqual),
            "lt" => Ok(Self::LessThan),
            "le" => Ok(Self::LessThanOrEqual),
            _ => Err(format!("{} is not a valid operator", s)),
        }
    }
}
