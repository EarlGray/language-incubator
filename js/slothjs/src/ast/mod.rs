use crate::prelude::*;

mod display;
pub mod expr;
pub mod stmt;

pub use self::expr::*;
pub use self::stmt::*;

/// represents a complete top-level JS script.
#[derive(Debug)]
pub struct Program {
    pub body: BlockStatement,
    pub variables: HashSet<Identifier>, // The set of scope variables
    pub functions: Vec<FunctionDeclaration>,
}

impl Program {
    pub fn from_stmt<V>(val: V) -> Program
    where
        Statement: From<V>,
    {
        let stmt = Statement::from(val);
        match stmt.stmt {
            Stmt::Block(block) => Program::from(block),
            _ => {
                let body = stmt::block(vec![stmt]);
                Program::from(body)
            }
        }
    }

    pub fn from_stmts(stmts: Vec<Statement>) -> Program {
        Program::from(BlockStatement::from(stmts))
    }
}

impl PartialEq for Program {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
    }
}

impl Eq for Program {}

impl From<BlockStatement> for Program {
    fn from(blockstmt: BlockStatement) -> Program {
        Program {
            body: blockstmt,
            variables: HashSet::new(), // TODO: block analysis
            functions: vec![],         // TODO: block analysis
        }
    }
}
