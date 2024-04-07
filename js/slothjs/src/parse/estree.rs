use serde_json::json;

use crate::{ast::*, prelude::*, JSON};

/// ToESTree de-parses an AST struct into its Esprima representation.
pub trait ToESTree {
    fn to_estree(&self) -> JSON;
}

impl ToESTree for JSON {
    fn to_estree(&self) -> JSON {
        self.clone() // TODO: validate it's a valid ESTree?
    }
}

impl ToESTree for Program {
    fn to_estree(&self) -> JSON {
        let body: Vec<JSON> = self.body.body.iter().map(|stmt| stmt.to_estree()).collect();
        json!({"type": "Program", "sourceType": "script", "body": body})
    }
}

impl ToESTree for BlockStatement {
    fn to_estree(&self) -> JSON {
        let body: Vec<JSON> = self.body.iter().map(|stmt| stmt.to_estree()).collect();
        json!({"type": "BlockStatement", "body": body})
    }
}

impl ToESTree for VariableDeclaration {
    fn to_estree(&self) -> JSON {
        let declarations: Vec<JSON> = self
            .declarations
            .iter()
            .map(|decl| {
                let jid = decl.name.to_estree();
                let jinit = match decl.init.as_ref() {
                    Some(init) => init.to_estree(),
                    None => JSON::Null,
                };
                json!({"type": "VariableDeclarator", "id": jid, "init": jinit})
            })
            .collect();
        let kind = self.kind.to_estree();
        json!({"type": "VariableDeclaration", "kind": kind, "declarations": declarations })
    }
}

impl ToESTree for DeclarationKind {
    fn to_estree(&self) -> JSON {
        JSON::from(match self {
            DeclarationKind::Var => "var",
            DeclarationKind::Let => "let",
            DeclarationKind::Const => "const",
        })
    }
}

impl ToESTree for Statement {
    fn to_estree(&self) -> JSON {
        match &self.stmt {
            Stmt::Empty => json!({"type": "EmptyStatement"}),
            Stmt::Expr(stmt) => {
                let jexpr = stmt.expression.to_estree();
                json!({"type": "ExpressionStatement", "expression": jexpr})
            }
            Stmt::Variable(vardecl) => vardecl.to_estree(),
            _ => todo!(),
        }
    }
}

impl ToESTree for Expression {
    fn to_estree(&self) -> JSON {
        match &self.expr {
            Expr::Literal(lit) => lit.to_estree(),
            Expr::Identifier(id) => id.to_estree(),
            Expr::BinaryOp(binop) => {
                let BinaryExpression(left, op, right) = binop.as_ref();
                let left = left.to_estree();
                let right = right.to_estree();
                let op = op.to_estree();
                json!({"type": "BinaryExpression", "left": left, "operator": op, "right": right})
            }
            _ => todo!(),
        }
    }
}

impl ToESTree for Literal {
    fn to_estree(&self) -> JSON {
        json!({"type": "Literal", "value": self.to_json()})
    }
}

impl ToESTree for Identifier {
    fn to_estree(&self) -> JSON {
        json!({"type": "Identifier", "name": self.as_str()})
    }
}

impl ToESTree for BinOp {
    fn to_estree(&self) -> JSON {
        JSON::from(match self {
            BinOp::Plus => "+",
            _ => todo!(),
        })
    }
}
