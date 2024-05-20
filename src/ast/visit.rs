use super::{
    Assignment, Block, Computation, Expression, Factor, FuncBody, FuncCall, FuncDecl, IfStatement,
    Relation, ReturnStatement, Statement, Term, WhileStatement,
};

pub trait AstVisitor: Sized {
    fn visit_computation(&mut self, computation: &Computation) {
        walk_computation(self, computation);
    }
    fn visit_func_body(&mut self, func_body: &FuncBody) {
        walk_func_body(self, func_body);
    }
    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        walk_func_decl(self, func_decl);
    }
    fn visit_block(&mut self, block: &Block) {
        walk_block(self, block);
    }
    fn visit_statement(&mut self, statement: &Statement) {
        walk_statement(self, statement);
    }
    fn visit_assignment(&mut self, assignment: &Assignment) {
        walk_assignment(self, assignment);
    }
    fn visit_func_call(&mut self, func_call: &FuncCall) {
        walk_func_call(self, func_call);
    }
    fn visit_if_statement(&mut self, if_statement: &IfStatement) {
        walk_if_statement(self, if_statement);
    }
    fn visit_while_statement(&mut self, while_statement: &WhileStatement) {
        walk_while_statement(self, while_statement);
    }
    fn visit_return_statement(&mut self, return_statement: &ReturnStatement) {
        walk_return_statement(self, return_statement);
    }
    fn visit_relation(&mut self, relation: &Relation) {
        walk_relation(self, relation);
    }
    fn visit_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }
    fn visit_term(&mut self, term: &Term) {
        walk_term(self, term);
    }
    fn visit_factor(&mut self, factor: &Factor) {
        walk_factor(self, factor);
    }
}

fn walk_computation<T: AstVisitor>(visitor: &mut T, computation: &Computation) {
    for func_decl in &computation.funcs {
        visitor.visit_func_decl(func_decl);
    }

    visitor.visit_block(&computation.body);
}

fn walk_func_body<T: AstVisitor>(visitor: &mut T, func_body: &FuncBody) {
    visitor.visit_block(&func_body.body);
}

fn walk_func_decl<T: AstVisitor>(visitor: &mut T, func_decl: &FuncDecl) {
    visitor.visit_func_body(&func_decl.body);
}

fn walk_block<T: AstVisitor>(visitor: &mut T, block: &Block) {
    for statement in &block.statements {
        visitor.visit_statement(statement);
    }
}

fn walk_statement<T: AstVisitor>(visitor: &mut T, statement: &Statement) {
    match statement {
        Statement::Assignment(assignment) => visitor.visit_assignment(assignment),
        Statement::FuncCall(func_call) => visitor.visit_func_call(func_call),
        Statement::IfStatement(if_statement) => visitor.visit_if_statement(if_statement),
        Statement::WhileStatement(while_statement) => {
            visitor.visit_while_statement(while_statement)
        }
        Statement::ReturnStatement(return_statement) => {
            visitor.visit_return_statement(return_statement)
        }
    }
}

fn walk_assignment<T: AstVisitor>(visitor: &mut T, assignment: &Assignment) {
    visitor.visit_expression(&assignment.expr);
}

fn walk_func_call<T: AstVisitor>(visitor: &mut T, func_call: &FuncCall) {
    for arg in &func_call.args {
        visitor.visit_expression(arg);
    }
}

fn walk_if_statement<T: AstVisitor>(visitor: &mut T, if_statement: &IfStatement) {
    visitor.visit_relation(&if_statement.rel);
    visitor.visit_block(&if_statement.then_block);
    if let Some(else_block) = &if_statement.else_block {
        visitor.visit_block(else_block);
    }
}

fn walk_while_statement<T: AstVisitor>(visitor: &mut T, while_statement: &WhileStatement) {
    visitor.visit_relation(&while_statement.rel);
    visitor.visit_block(&while_statement.block);
}

fn walk_return_statement<T: AstVisitor>(visitor: &mut T, return_statement: &ReturnStatement) {
    if let Some(expr) = &return_statement.expr {
        visitor.visit_expression(expr);
    }
}

fn walk_relation<T: AstVisitor>(visitor: &mut T, relation: &Relation) {
    visitor.visit_expression(&relation.lhs);
    visitor.visit_expression(&relation.rhs);
}

fn walk_expression<T: AstVisitor>(visitor: &mut T, expression: &Expression) {
    visitor.visit_term(&expression.term);
    for (_, term) in &expression.ops {
        visitor.visit_term(term);
    }
}

fn walk_term<T: AstVisitor>(visitor: &mut T, term: &Term) {
    visitor.visit_factor(&term.factor);
    for (_, factor) in &term.ops {
        visitor.visit_factor(factor);
    }
}

fn walk_factor<T: AstVisitor>(visitor: &mut T, factor: &Factor) {
    match factor {
        Factor::Expression(expression) => visitor.visit_expression(expression),
        Factor::FuncCall(func_call) => visitor.visit_func_call(func_call),
        Factor::Ident(_) | Factor::Number(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{ExprOp, FormalParam, TermOp},
        lexer::RelOp,
    };

    use super::*;

    #[derive(Debug, PartialEq)]
    enum VisitFunc {
        Computation,
        FuncBody,
        FuncDecl,
        Block,
        Statement,
        Assignment,
        FuncCall,
        IfStatement,
        WhileStatement,
        ReturnStatement,
        Relation,
        Expression,
        Term,
        Factor,
    }

    struct VisitTester(pub Vec<VisitFunc>);

    impl VisitTester {
        fn new() -> Self {
            Self(Vec::new())
        }
    }

    impl AstVisitor for VisitTester {
        fn visit_computation(&mut self, computation: &Computation) {
            self.0.push(VisitFunc::Computation);
            walk_computation(self, computation);
        }

        fn visit_func_body(&mut self, func_body: &FuncBody) {
            self.0.push(VisitFunc::FuncBody);
            walk_func_body(self, func_body);
        }

        fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
            self.0.push(VisitFunc::FuncDecl);
            walk_func_decl(self, func_decl);
        }

        fn visit_block(&mut self, block: &Block) {
            self.0.push(VisitFunc::Block);
            walk_block(self, block);
        }

        fn visit_statement(&mut self, statement: &Statement) {
            self.0.push(VisitFunc::Statement);
            walk_statement(self, statement);
        }

        fn visit_assignment(&mut self, assignment: &Assignment) {
            self.0.push(VisitFunc::Assignment);
            walk_assignment(self, assignment);
        }

        fn visit_func_call(&mut self, func_call: &FuncCall) {
            self.0.push(VisitFunc::FuncCall);
            walk_func_call(self, func_call);
        }

        fn visit_if_statement(&mut self, if_statement: &IfStatement) {
            self.0.push(VisitFunc::IfStatement);
            walk_if_statement(self, if_statement);
        }

        fn visit_while_statement(&mut self, while_statement: &WhileStatement) {
            self.0.push(VisitFunc::WhileStatement);
            walk_while_statement(self, while_statement);
        }

        fn visit_return_statement(&mut self, return_statement: &ReturnStatement) {
            self.0.push(VisitFunc::ReturnStatement);
            walk_return_statement(self, return_statement);
        }

        fn visit_relation(&mut self, relation: &Relation) {
            self.0.push(VisitFunc::Relation);
            walk_relation(self, relation);
        }

        fn visit_expression(&mut self, expression: &Expression) {
            self.0.push(VisitFunc::Expression);
            walk_expression(self, expression);
        }

        fn visit_term(&mut self, term: &Term) {
            self.0.push(VisitFunc::Term);
            walk_term(self, term);
        }

        fn visit_factor(&mut self, factor: &Factor) {
            self.0.push(VisitFunc::Factor);
            walk_factor(self, factor);
        }
    }

    #[test]
    fn visit_factor() {
        let factor = Factor::Number(1);
        let mut visitor = VisitTester::new();
        visitor.visit_factor(&factor);

        assert_eq!(visitor.0, vec![VisitFunc::Factor]);

        let factor = Factor::Ident(1);
        let mut visitor = VisitTester::new();
        visitor.visit_factor(&factor);

        assert_eq!(visitor.0, vec![VisitFunc::Factor]);

        let factor = Factor::Expression(Box::new(Expression {
            term: Term {
                factor: Factor::Number(1),
                ops: vec![],
            },
            ops: vec![],
        }));
        let mut visitor = VisitTester::new();
        visitor.visit_factor(&factor);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Factor,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );

        let factor = Factor::FuncCall(FuncCall {
            ident: 1,
            args: vec![Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            }],
        });
        let mut visitor = VisitTester::new();
        visitor.visit_factor(&factor);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Factor,
                VisitFunc::FuncCall,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_term() {
        let term = Term {
            factor: Factor::Number(1),
            ops: vec![],
        };
        let mut visitor = VisitTester::new();
        visitor.visit_term(&term);

        assert_eq!(visitor.0, vec![VisitFunc::Term, VisitFunc::Factor]);

        let term = Term {
            factor: Factor::Number(1),
            ops: vec![(TermOp::Mul, Factor::Number(1))],
        };
        let mut visitor = VisitTester::new();
        visitor.visit_term(&term);

        assert_eq!(
            visitor.0,
            vec![VisitFunc::Term, VisitFunc::Factor, VisitFunc::Factor]
        );
    }

    #[test]
    fn visit_expression() {
        let expression = Expression {
            term: Term {
                factor: Factor::Number(1),
                ops: vec![],
            },
            ops: vec![],
        };
        let mut visitor = VisitTester::new();
        visitor.visit_expression(&expression);

        assert_eq!(
            visitor.0,
            vec![VisitFunc::Expression, VisitFunc::Term, VisitFunc::Factor]
        );

        let expression = Expression {
            term: Term {
                factor: Factor::Number(1),
                ops: vec![],
            },
            ops: vec![(
                ExprOp::Add,
                Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
            )],
        };
        let mut visitor = VisitTester::new();
        visitor.visit_expression(&expression);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_relation() {
        let relation = Relation {
            lhs: Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            },
            rel_op: RelOp::Eq,
            rhs: Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            },
        };
        let mut visitor = VisitTester::new();
        visitor.visit_relation(&relation);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Relation,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_assignment() {
        let assignment = Assignment {
            ident: 1,
            expr: Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            },
        };
        let mut visitor = VisitTester::new();
        visitor.visit_assignment(&assignment);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Assignment,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_return_statement() {
        let return_statement = ReturnStatement {
            expr: Some(Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            }),
        };
        let mut visitor = VisitTester::new();
        visitor.visit_return_statement(&return_statement);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::ReturnStatement,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_while_statement() {
        let while_statement = WhileStatement {
            rel: Relation {
                lhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rel_op: RelOp::Eq,
                rhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            },
            block: Block { statements: vec![] },
        };
        let mut visitor = VisitTester::new();
        visitor.visit_while_statement(&while_statement);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::WhileStatement,
                VisitFunc::Relation,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Block
            ]
        );
    }

    #[test]
    fn visit_if_statement() {
        let if_statement = IfStatement {
            rel: Relation {
                lhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rel_op: RelOp::Eq,
                rhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            },
            then_block: Block { statements: vec![] },
            else_block: Some(Block { statements: vec![] }),
        };
        let mut visitor = VisitTester::new();
        visitor.visit_if_statement(&if_statement);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::IfStatement,
                VisitFunc::Relation,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Block,
                VisitFunc::Block
            ]
        );

        let if_statement = IfStatement {
            rel: Relation {
                lhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
                rel_op: RelOp::Eq,
                rhs: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            },
            then_block: Block { statements: vec![] },
            else_block: None,
        };
        let mut visitor = VisitTester::new();
        visitor.visit_if_statement(&if_statement);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::IfStatement,
                VisitFunc::Relation,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Block
            ]
        );
    }

    #[test]
    fn visit_func_call() {
        let func_call = FuncCall {
            ident: 1,
            args: vec![Expression {
                term: Term {
                    factor: Factor::Number(1),
                    ops: vec![],
                },
                ops: vec![],
            }],
        };
        let mut visitor = VisitTester::new();
        visitor.visit_func_call(&func_call);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::FuncCall,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_block() {
        let block = Block {
            statements: vec![Statement::Assignment(Assignment {
                ident: 1,
                expr: Expression {
                    term: Term {
                        factor: Factor::Number(1),
                        ops: vec![],
                    },
                    ops: vec![],
                },
            })],
        };
        let mut visitor = VisitTester::new();
        visitor.visit_block(&block);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Block,
                VisitFunc::Statement,
                VisitFunc::Assignment,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_func_decl() {
        let func_decl = FuncDecl {
            name: "test".to_string(),
            is_void: false,
            params: FormalParam { params: vec![] },
            ident: 1,
            body: FuncBody {
                vars: None,
                body: Block {
                    statements: vec![Statement::Assignment(Assignment {
                        ident: 1,
                        expr: Expression {
                            term: Term {
                                factor: Factor::Number(1),
                                ops: vec![],
                            },
                            ops: vec![],
                        },
                    })],
                },
            },
        };
        let mut visitor = VisitTester::new();
        visitor.visit_func_decl(&func_decl);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::FuncDecl,
                VisitFunc::FuncBody,
                VisitFunc::Block,
                VisitFunc::Statement,
                VisitFunc::Assignment,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_func_body() {
        let func_body = FuncBody {
            vars: None,
            body: Block {
                statements: vec![Statement::Assignment(Assignment {
                    ident: 1,
                    expr: Expression {
                        term: Term {
                            factor: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },
        };
        let mut visitor = VisitTester::new();
        visitor.visit_func_body(&func_body);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::FuncBody,
                VisitFunc::Block,
                VisitFunc::Statement,
                VisitFunc::Assignment,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }

    #[test]
    fn visit_computation() {
        let computation = Computation {
            vars: None,
            funcs: vec![FuncDecl {
                name: "test".to_string(),
                is_void: false,
                params: FormalParam { params: vec![] },
                ident: 1,
                body: FuncBody {
                    vars: None,
                    body: Block {
                        statements: vec![Statement::Assignment(Assignment {
                            ident: 1,
                            expr: Expression {
                                term: Term {
                                    factor: Factor::Number(1),
                                    ops: vec![],
                                },
                                ops: vec![],
                            },
                        })],
                    },
                },
            }],
            body: Block {
                statements: vec![Statement::Assignment(Assignment {
                    ident: 1,
                    expr: Expression {
                        term: Term {
                            factor: Factor::Number(1),
                            ops: vec![],
                        },
                        ops: vec![],
                    },
                })],
            },
        };
        let mut visitor = VisitTester::new();
        visitor.visit_computation(&computation);

        assert_eq!(
            visitor.0,
            vec![
                VisitFunc::Computation,
                VisitFunc::FuncDecl,
                VisitFunc::FuncBody,
                VisitFunc::Block,
                VisitFunc::Statement,
                VisitFunc::Assignment,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor,
                VisitFunc::Block,
                VisitFunc::Statement,
                VisitFunc::Assignment,
                VisitFunc::Expression,
                VisitFunc::Term,
                VisitFunc::Factor
            ]
        );
    }
}
