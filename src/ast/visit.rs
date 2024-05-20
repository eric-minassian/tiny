use super::{
    Assignment, Block, Computation, Expression, Factor, FormalParam, FuncBody, FuncCall, FuncDecl,
    IfStatement, Relation, ReturnStatement, Statement, Term, VarDecl, WhileStatement,
};

pub trait AstVisitor {
    fn visit_computation(&mut self, computation: &Computation);
    fn visit_func_body(&mut self, func_body: &FuncBody);
    fn visit_formal_param(&mut self, formal_param: &FormalParam);
    fn visit_func_decl(&mut self, func_decl: &FuncDecl);
    fn visit_var_decl(&mut self, var_decl: &VarDecl);
    fn visit_block(&mut self, block: &Block);
    fn visit_statement(&mut self, statement: &Statement);
    fn visit_assignment(&mut self, assignment: &Assignment);
    fn visit_func_call(&mut self, func_call: &FuncCall);
    fn visit_if_statement(&mut self, if_statement: &IfStatement);
    fn visit_while_statement(&mut self, while_statement: &WhileStatement);
    fn visit_return_statement(&mut self, return_statement: &ReturnStatement);
    fn visit_relation(&mut self, relation: &Relation);
    fn visit_expression(&mut self, expression: &Expression);
    fn visit_term(&mut self, term: &Term);
    fn visit_factor(&mut self, factor: &Factor);
}
