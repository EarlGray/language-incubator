use serde_json::json;

use crate::parse::*;
use crate::prelude::*;

#[test]
fn test_parser_context() -> Result<(), ParseError> {
    // var a = 4; function a_plus(b) { return a + b; }
    let json_ast1 = json!({
        "type": "BlockStatement",
        "body": [
            {   "type": "VariableDeclaration",
                "kind": "let",
                "declarations": [
                    {   "type": "VariableDeclarator",
                        "id": {"type": "Identifier", "name": "a"},
                        "init": {"type": "Literal", "value": 4.0},
                    },
                ]
            },
            {   "type": "FunctionDeclaration",
                "id": {"type": "Identifier", "name": "a_plus"},
                "params": [{"type": "Identifier", "name": "b"}],
                "body": { "type": "BlockStatement",
                    "body": [{
                        "type": "ReturnStatement",
                        "argument": {
                            "type": "BinaryExpression",
                            "operator": "+",
                            "left": {"type": "Identifier", "name": "a"},
                            "right": {"type": "Identifier", "name": "b"},
                        }
                    }],
                },
            },
        ],
    });
    let mut ctx = ParserContext::new();
    let ast1 = BlockStatement::parse_from(&json_ast1, &mut ctx)?;

    assert_eq!(ctx.declared_functions[0].id, Identifier::from("a_plus"));

    let a_plus = match &ast1.body[1].stmt {
        Stmt::Function(funcdecl) => &funcdecl.function,
        _ => panic!("a_plus: want a Stmt::Function, got {:?}", ast1.body[1].stmt),
    };

    let want = HashSet::from([Identifier::from("a")]);
    assert_eq!(&ast1.bindings, &want, "let bindings");

    assert_eq!(
        a_plus.func.free_variables,
        HashSet::from([Identifier::from("a")]),
        "free variables of a_plus must be: ['a']",
    );

    Ok(())
}

#[test]
fn test_redeclared() -> Result<(), ParseError> {
    let json_ast = json!({
        "type": "BlockStatement",
        "body": [
            {
                "type": "VariableDeclaration",
                "kind": "let",
                "declarations": [
                    {
                        "type": "VariableDeclarator",
                        "id": { "type": "Identifier", "name": "foo" },
                        "init": null
                    }
                ]
            },
            {
                "type": "VariableDeclaration",
                "kind": "let",
                "declarations": [
                    {
                        "type": "VariableDeclarator",
                        "id": { "type": "Identifier", "name": "foo" },
                        "init": null
                    }
                ]
            }
        ],
    });

    let mut ctx = ParserContext::new();
    match BlockStatement::parse_from(&json_ast, &mut ctx) {
        Err(ParseError::BindingRedeclared {}) => Ok(()),
        other => panic!("want an error, got {:?}", other),
    }
}
