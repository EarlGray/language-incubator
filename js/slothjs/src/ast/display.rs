use core::fmt;

use crate::prelude::*;

use super::expr::*;
use super::stmt::*;


impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let source = self.to_source();
        if source.len() == 1 {
            write!(f, "{}", source[0])
        } else {
            for line in source.iter() {
                writeln!(f, "{}", line)?;
            }
            Ok(())
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let source = self.to_source();
        if source.len() == 1 {
            write!(f, "{}", source[0])
        } else {
            for line in source.iter() {
                writeln!(f, "{}", line)?;
            }
            Ok(())
        }
    }
}

trait ToSource {
    const INDENT: &'static str = "    ";

    fn to_source(&self) -> Vec<String>;
}

impl ToSource for Literal {
    fn to_source(&self) -> Vec<String> {
        vec![self.0.to_string()]
    }
}

impl ToSource for Identifier {
    fn to_source(&self) -> Vec<String> {
        vec![self.0.to_string()]
    }
}

impl ToSource for BinOp {
    fn to_source(&self) -> Vec<String> {
        use BinOp::*;
        let s = match self {
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Percent => "%",
            EqEq => "==",
            NotEq => "!=",
            EqEqEq => "===",
            NotEqEq => "!==",
            Less => "<",
            Greater => ">",
            LtEq => "<=",
            GtEq => ">=",
            Pipe => "|",
            Hat => "^",
            Ampersand => "&",
            LtLt => "<<",
            GtGt => ">>",
            GtGtGt => ">>>",
            In => "in",
            InstanceOf => "instanceof",
        };
        vec![s.to_string()]
    }
}

impl ToSource for MemberExpression {
    fn to_source(&self) -> Vec<String> {
        let MemberExpression(object, attr, computed) = self;
        let objsrc = object.to_source();
        let mut s = String::new();
        if objsrc.len() == 1 {
            s.push_str(objsrc[0].as_str());
        } else {
            todo!(
                "MemberExpression::to_source(): {} lines in object",
                objsrc.len()
            );
        }

        if *computed {
            let attrsrc = attr.to_source();
            s.push('[');
            if attrsrc.len() == 1 {
                s.push_str(attrsrc[0].as_str());
            } else {
                todo!(
                    "MemberExpression::to_source(): {} lines in attr",
                    attrsrc.len()
                );
            }
            s.push(']');
        } else if let Expr::Identifier(ident) = &attr.expr {
            s.push('.');
            s.push_str(ident.as_str());
        } else {
            panic!(
                "MemberExpression::to_source(): non-computed non-identifiter attr {:?}",
                attr.expr
            );
        }
        vec![s]
    }
}

impl ToSource for CallExpression {
    fn to_source(&self) -> Vec<String> {
        let CallExpression(callee, arguments) = self;

        let mut s = String::new();
        let calleesrc = callee.to_source();
        if calleesrc.len() != 1 {
            todo!(
                "CallExpression::to_source(): {} lines in callee",
                calleesrc.len()
            );
        }
        s.push_str(calleesrc[0].as_str());

        s.push('(');
        for arg in arguments.iter() {
            let argsrc = arg.to_source();
            if argsrc.len() != 1 {
                todo!(
                    "CallExpression::to_source(): {} lines in argument: {:?}",
                    argsrc.len(),
                    arg
                );
            }
            s.push_str(argsrc[0].as_str());
            s.push_str(", ");
        }
        if s.ends_with(", ") {
            s.pop();
            s.pop();
        }
        s.push(')');

        vec![s]
    }
}

impl ToSource for Expr {
    fn to_source(&self) -> Vec<String> {
        match self {
            Expr::Literal(lit) => lit.to_source(),
            Expr::Identifier(id) => id.to_source(),
            Expr::BinaryOp(binop) => {
                let BinaryExpression(left, op, right) = binop.as_ref();
                let left = left.to_source();
                let op = op.to_source();
                let right = right.to_source();
                let s = format!("{} {} {}", left[0], op[0], right[0]);
                vec![s]
            }
            Expr::Array(arrexpr) => {
                if arrexpr.0.is_empty() {
                    vec!["[]".to_string()]
                } else {
                    let mut s = String::new();
                    s.push('[');
                    for expr in arrexpr.0.iter() {
                        let e = expr.to_source();
                        s.push_str(e[0].as_str());
                        s.push_str(", ");
                    }
                    s.pop();
                    s.pop();
                    s.push(']');
                    vec![s]
                }
            }
            Expr::Member(membexpr) => membexpr.to_source(),
            Expr::Call(callexpr) => callexpr.to_source(),
            Expr::This => vec!["this".to_string()],
            /*
            Expr::Unary() => TODO
            Expr::Conditional() => TODO
            Expr::Update() => TODO
            Expr::Function() => TODO
            Expr::Sequence() => TODO
            Expr::Assign() => TODO
            Expr::New() => TODO
            Expr::LogicalOp() => TODO
            */
            other => todo!("{:?}.to_source()", other),
        }
    }
}

impl ToSource for Expression {
    fn to_source(&self) -> Vec<String> {
        // TODO: consult and check self.loc, if any
        self.expr.to_source()
    }
}

impl ToSource for IfStatement {
    fn to_source(&self) -> Vec<String> {
        let cond = self.test.to_source();
        let thensrc = self.consequent.to_source();

        let mut lines = vec![];

        // "if (<ifstmt.test>) "
        let mut s = String::from("if ");
        if cond.len() == 1 {
            let cond = cond[0].as_str();
            let cond_brackets = cond.chars().nth(0) == Some('(');
            if !cond_brackets {
                s.push('(');
            }
            s.push_str(cond);
            if !cond_brackets {
                s.push(')');
            }
        } else {
            todo!();
        }

        // then block
        let block_then = matches!(self.consequent.stmt, Stmt::Block(_));
        if block_then {
            s.push_str(" {");
            lines.push(s);
            lines.extend(thensrc.iter().skip(1).map(|s| s.to_string()));
        } else {
            lines.push(s);
            for line in thensrc.iter() {
                let mut s = String::from(Self::INDENT);
                s.push_str(line.as_str());
                lines.push(s);
            }
        }

        // else block
        if let Some(elsestmt) = self.alternate.as_ref() {
            let block_else = matches!(elsestmt.stmt, Stmt::Block(_));
            let elsesrc = elsestmt.to_source();

            let len = lines.len();
            match (block_then, block_else) {
                (true, true) => lines[len - 1].push_str(" else {"),
                (true, false) => lines[len - 1].push_str(" else"),
                (false, true) => lines.push("else {".to_string()),
                (false, false) => lines.push("else".to_string()),
            };

            if block_else {
                lines.extend(elsesrc.iter().skip(1).map(|s| s.to_string()));
            } else {
                for line in elsesrc.iter() {
                    let mut s = String::from(Self::INDENT);
                    s.push_str(line.as_str());
                    lines.push(s);
                }
            }
        }

        lines
    }
}

impl ToSource for BlockStatement {
    fn to_source(&self) -> Vec<String> {
        let mut lines = vec![];
        lines.push(String::from("{"));
        for stmt in self.body.iter() {
            let stmtsrc = stmt.to_source();
            for line in stmtsrc.iter() {
                let mut s = String::from(Self::INDENT);
                s.push_str(line.as_str());
                lines.push(s);
            }
        }
        lines.push(String::from("}"));
        lines
    }
}

impl ToSource for Stmt {
    fn to_source(&self) -> Vec<String> {
        use Stmt::*;
        match self {
            Empty => vec![";".to_string()],
            Block(blockstmt) => blockstmt.to_source(),
            Expr(exprstmt) => {
                let mut lines = exprstmt.expression.to_source();
                let len = lines.len();
                lines[len - 1].push(';');
                lines
            }
            If(ifstmt) => ifstmt.to_source(),
            //Switch() => TODO
            //For() => TODO
            //ForIn() => TODO
            Return(retstmt) => {
                let mut s = String::from("return");
                if let Some(retexpr) = &retstmt.0 {
                    s.push(' ');
                    let retsrc = retexpr.to_source();
                    if retsrc.len() != 1 {
                        todo!(
                            "Return::to_source(): {} lines in return expression",
                            retsrc.len()
                        );
                    }
                    s.push_str(retsrc[0].as_str());
                }
                s.push(';');
                vec![s]
            }
            //Break() => TODO
            //Continue() => TODO
            //Label() => TODO
            //Throw() => TODO
            //Try() => TODO
            //Variable() => TODO
            //Function() => TODO
            _ => todo!("{:?}.to_source()", self),
        }
    }
}

impl ToSource for Statement {
    fn to_source(&self) -> Vec<String> {
        // TODO: consult and check self.loc, if any
        self.stmt.to_source()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_print {
        ($want:expr, $expr:expr,) => {
            assert_print!($want, $expr);
        };
        ($want:literal, $expr:expr) => {
            let got = $expr.to_source().join("\n");
            if got.as_str() != $want {
                panic!("\nGOT:\n{}\n\nWANT:\n{}\n", got, $want);
            }
        };
        ($want:expr, $expr:expr) => {
            let want = $want;
            let got = $expr.to_source().join("\n");
            if got.as_str() != want.as_str() {
                panic!("\nGOT:\n{}\n\nWANT:\n{}\n", got, want);
            }
        };
    }

    #[test]
    fn display_expr() {
        assert_print!("0", Literal::from(0));
        assert_print!("-1", Literal::from(-1));

        assert_print!("true", Literal::from(true));
        assert_print!("\"string\"", Literal::from("string"));
        //assert_eq!( Literal::from(r#"\"quotes\""#) r#"\"\\\"quotes\\\"\""#]);

        assert_print!("test", id("test"));
        assert_print!("[]", empty_array());

        assert_print!("2 + 2", add(2, 2));
        //assert_print!("+1", plus(1));
        assert_print!(
            "x === undefined",
            binary(BinOp::EqEqEq, id("x"), id("undefined")),
        );
        {
            let expr = add(add("a", empty_array()), id("x"));
            assert_print!(r#""a" + [] + x"#, expr);
            assert_eq!(r#""a" + [] + x"#, format!("{}", expr));
        }
        assert_print!(
            r#"[2, "a", 2 + "a"]"#,
            array(vec![lit(2), lit("a"), add(2, "a")]),
        );

        assert_print!("sqr(12)", call(id("sqr"), vec![lit(12)]));
        assert_print!(
            r#"console.log("x=", x)"#,
            call(memb(id("console"), "log"), vec![lit("x="), id("x")]),
        );

        assert_print!("a.b.c", memb(memb(id("a"), "b"), "c"));
        assert_print!("a[b].c", memb(index(id("a"), id("b")), "c"));
        assert_print!("a[i + 1]", index(id("a"), add(id("i"), 1)));
    }

    #[test]
    fn display_stmt() {
        assert_print!(";", Statement::from(Stmt::Empty));

        assert_print!(
            r#"1 in ["a", "b"];"#,
            Statement::from(binary(BinOp::In, 1, array(vec!["a", "b"]))),
        );

        {
            let ifstmt = IfStatement {
                test: binary(BinOp::Percent, id("x"), lit(2)),
                consequent: Statement::from(call(memb(id("console"), "log"), vec![lit("odd")])),
                alternate: None,
            };

            #[rustfmt::skip]
            let want = [
                r#"if (x % 2)"#,
                r#"    console.log("odd");"#,
            ].join("\n");
            assert_print!(&want, ifstmt);
        }

        {
            let ifstmt = IfStatement {
                test: binary(BinOp::Less, id("n"), lit(2)),
                consequent: Statement::from(block(vec![
                    call(memb(id("console"), "log"), vec![lit("base")]).into(),
                    return_(lit(1)).into(),
                ])),
                alternate: Some(Statement::from(block(vec![return_(binary(
                    BinOp::Star,
                    id("n"),
                    call(id("fact"), vec![binary(BinOp::Minus, id("n"), 1)]),
                ))
                .into()]))),
            };

            #[rustfmt::skip]
            let want = [
                r#"if (n < 2) {"#,
                r#"    console.log("base");"#,
                r#"    return 1;"#,
                r#"} else {"#,
                r#"    return n * fact(n - 1);"#,
                r#"}"#,
            ].join("\n");

            assert_print!(&want, ifstmt);
        }
    }
}
