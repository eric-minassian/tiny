use crate::lexer::{Identifier, Number, RelOp, Token};

pub trait OptionFrom<T>: Sized {
    fn option_from(t: T) -> Option<Self>;
}

/* Functions */
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Computation {
    pub vars: Option<VarDecl>,
    pub funcs: Vec<FuncDecl>,
    pub body: Block,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct FuncBody {
    pub vars: Option<VarDecl>,
    pub body: Block,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct FormalParam {
    pub params: Vec<Identifier>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct FuncDecl {
    pub is_void: bool,
    pub ident: Identifier,
    pub params: FormalParam,
    pub body: FuncBody,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct VarDecl {
    pub vars: Vec<Identifier>,
}

/* Statements */
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Statement {
    Assignment(Assignment),
    FuncCall(FuncCall),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ReturnStatement(ReturnStatement),
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Assignment {
    pub ident: Identifier,
    pub expr: Expression,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct FuncCall {
    pub ident: Identifier,
    pub args: Vec<Expression>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct IfStatement {
    pub rel: Relation,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct WhileStatement {
    pub rel: Relation,
    pub block: Block,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct ReturnStatement {
    pub expr: Option<Expression>,
}

/* Expressions  */
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Relation {
    pub lhs: Expression,
    pub rel_op: RelOp,
    pub rhs: Expression,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Expression {
    pub term: Term,
    pub ops: Vec<(ExprOp, Term)>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct Term {
    pub factor: Factor,
    pub ops: Vec<(TermOp, Factor)>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Factor {
    Ident(Identifier),
    Number(Number),
    Expression(Box<Expression>),
    FuncCall(FuncCall),
}

/* Implementations */
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum ExprOp {
    Add,
    Sub,
}

impl OptionFrom<Token> for ExprOp {
    fn option_from(t: Token) -> Option<Self> {
        match t {
            Token::Add => Some(Self::Add),
            Token::Sub => Some(Self::Sub),
            _ => None,
        }
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum TermOp {
    Mul,
    Div,
}

impl OptionFrom<Token> for TermOp {
    fn option_from(t: Token) -> Option<Self> {
        match t {
            Token::Mul => Some(Self::Mul),
            Token::Div => Some(Self::Div),
            _ => None,
        }
    }
}
