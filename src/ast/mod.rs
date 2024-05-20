use crate::lexer::{Identifier, Number, RelOp};

/* Functions */
pub struct Computation {
    vars: Option<VarDecl>,
    funcs: Vec<FuncDecl>,
    body: Block,
}

pub struct FuncBody {
    vars: Option<VarDecl>,
    body: Block,
}

pub struct FormalParam {
    params: Vec<Identifier>,
}

pub struct FuncDecl {
    is_void: bool,
    ident: Identifier,
    params: FormalParam,
    body: Block,
}

pub struct VarDecl {}

/* Statements */
pub struct Block {
    pub body: Vec<Statement>,
}

pub enum Statement {
    Assignment(Assignment),
    FuncCall(FuncCall),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ReturnStatement(ReturnStatement),
}

pub struct Assignment {
    pub ident: Identifier,
    pub expr: Expression,
}

pub struct FuncCall {
    pub ident: Identifier,
    pub args: Vec<Expression>,
}

pub struct IfStatement {
    rel: Relation,
    then_block: Block,
    else_block: Option<Block>,
}

pub struct WhileStatement {
    rel: Relation,
    block: Block,
}

pub struct ReturnStatement {
    expr: Option<Expression>,
}

/* Expressions  */
pub struct Relation {
    pub lhs: Expression,
    pub rhs: Expression,
    pub op: RelOp,
}

pub struct Expression {
    pub term: Term,
    pub ops: Vec<(ExpressionOperator, Term)>,
}

pub enum ExpressionOperator {
    Add,
    Subtract,
}

pub struct Term {
    pub factor: Factor,
    pub ops: Vec<(TermOp, Factor)>,
}

pub enum TermOp {
    Multiply,
    Divide,
}

pub enum Factor {
    Ident(Identifier),
    Number(Number),
    Expression(Box<Expression>),
    FuncCall(FuncCall),
}
