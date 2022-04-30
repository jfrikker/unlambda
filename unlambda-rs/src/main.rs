use std::{io::{stdin, Read}, fs::File, env::args, rc::Rc, fmt::Display};

use parser::AstNode;

use crate::parser::parse;

mod parser;

fn main() {
    let mut code = String::new();
    File::open(args().nth(1).unwrap()).unwrap().read_to_string(&mut code).unwrap();
    let parsed = parse(&code).unwrap().1;

    println!("{:?}", parsed);
    println!("{:?}", Unlambda::default().run(parsed));
}

#[derive(Debug, PartialEq)]
enum Value {
    Char(char),
    Constant0,
    Constant1(Rc<Value>),
    Continuation(Rc<Continuation>),
    CreateContinuation,
    Distribute0,
    Distribute1(Rc<Value>),
    Distribute2(Rc<Value>, Rc<Value>),
    Identity,
    Lazy0,
    Lazy1(MaybeEvaluated),
    Read,
    PrintCC,
    Term,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char('\n') => write!(f, "r"),
            Self::Char(c) => write!(f, ".{}", c),
            Self::Constant0 => write!(f, "k"),
            Self::Constant1(arg) => write!(f, "`k{}", arg),
            Self::Continuation(next) => write!(f, "<cont:{}>", next),
            Self::CreateContinuation => write!(f, "c"),
            Self::Distribute0 => write!(f, "s"),
            Self::Distribute1(arg) => write!(f, "`s{}", arg),
            Self::Distribute2(arg1, arg2) => write!(f, "``s{}{}", arg1, arg2),
            Self::Identity => write!(f, "i"),
            Self::Lazy0 => write!(f, "d"),
            Self::Lazy1(arg) => write!(f, "`d{}", arg),
            Self::Read => write!(f, "@"),
            Self::PrintCC => write!(f, "|"),
            Self::Term => write!(f, "v"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum MaybeEvaluated {
    Evaluated(Rc<Value>),
    Unevaluated(Rc<AstNode>),
}

impl Display for MaybeEvaluated {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Evaluated(e) => write!(f, "{}", e),
            Self::Unevaluated(e) => write!(f, "{}", e),
        }
    }
}

impl From<Rc<Value>> for MaybeEvaluated {
    fn from(val: Rc<Value>) -> Self {
        Self::Evaluated(val)
    }
}

impl <'a> From<&'a Rc<Value>> for MaybeEvaluated {
    fn from(val: &'a Rc<Value>) -> Self {
        Self::Evaluated(val.clone())
    }
}

impl From<Value> for MaybeEvaluated {
    fn from(val: Value) -> Self {
        Self::Evaluated(val.into())
    }
}

impl From<Rc<AstNode>> for MaybeEvaluated {
    fn from(val: Rc<AstNode>) -> Self {
        Self::Unevaluated(val)
    }
}

impl <'a> From<&'a Rc<AstNode>> for MaybeEvaluated {
    fn from(val: &'a Rc<AstNode>) -> Self {
        Self::Unevaluated(val.clone())
    }
}

impl From<AstNode> for MaybeEvaluated {
    fn from(val: AstNode) -> Self {
        Self::Unevaluated(val.into())
    }
}

#[derive(Debug, PartialEq)]
enum Continuation {
    Apply1(MaybeEvaluated, Rc<Continuation>),
    Apply2(Rc<Value>, Rc<Continuation>),
    Distribute(Rc<Value>, Rc<Value>, Rc<Continuation>),
    Lazy0(MaybeEvaluated, Rc<Continuation>),
    End,
}

impl Continuation {
    fn write_prefix(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Apply1(_, next) => {
                next.write_prefix(f)?;
                write!(f, "`")
            }
            Self::Apply2(fun, next) => {
                next.write_prefix(f)?;
                write!(f, "`{}", fun)
            },
            Self::Distribute(_, _, next) => {
                next.write_prefix(f)?;
                write!(f, "`d")
            }
            Self::Lazy0(fun, next) => {
                next.write_prefix(f)?;
                write!(f, "`d{}", fun)
            }
            Self::End => Ok(())
        }
    }

    fn write_suffix(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Apply1(arg, next) => {
                write!(f, "{}", arg)?;
                next.write_suffix(f)
            }
            Self::Apply2(_, next) => next.write_suffix(f),
            Self::Distribute(f2, arg, next) => {
                write!(f, "{}{}", f2, arg)?;
                next.write_suffix(f)
            }
            Self::Lazy0(_, next) => next.write_suffix(f),
            Self::End => Ok(())
        }
    }
}

impl Display for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_prefix(f)?;
        write!(f, "_")?;
        self.write_suffix(f)
    }
}

#[derive(Default)]
struct Unlambda {
    current_char: Option<char>,
}

impl Unlambda {
    fn run(&mut self, prog: Rc<AstNode>) -> MaybeEvaluated {
        let (mut prog, mut continuation) = self.step(prog.into(), Continuation::End.into());
        while *continuation != Continuation::End {
            // println!("{} -- {}", prog, continuation);
            (prog, continuation) = self.step(prog, continuation);
        }
        prog
    }

    fn step(&mut self, prog: MaybeEvaluated, continuation: Rc<Continuation>) -> (MaybeEvaluated, Rc<Continuation>) {
        match prog {
            MaybeEvaluated::Unevaluated(ast) => {
                match ast.as_ref() {
                    AstNode::Apply(f, a) => (f.into(),
                        Rc::new(Continuation::Apply1(a.into(), continuation))),
                    AstNode::Char(c) => (Value::Char(*c).into(), continuation),
                    AstNode::Constant => (Value::Constant0.into(), continuation),
                    AstNode::Continuation => (Value::CreateContinuation.into(), continuation),
                    AstNode::Identity => (Value::Identity.into(), continuation),
                    AstNode::Distribute => (Value::Distribute0.into(), continuation),
                    AstNode::Lazy => (Value::Lazy0.into(), continuation),
                    AstNode::Term => (Value::Term.into(), continuation),
                    AstNode::Read => (Value::Read.into(), continuation),
                    AstNode::PrintCC => (Value::PrintCC.into(), continuation),
                }
            }
            MaybeEvaluated::Evaluated(v) => self.continu(continuation, v)
        }
    }

    fn maybe_apply(&mut self, func: Rc<Value>, arg: MaybeEvaluated, continuation: Rc<Continuation>) -> (MaybeEvaluated, Rc<Continuation>) {
        match &*func {
            Value::Lazy0 => (Value::Lazy1(arg).into(), continuation),
            Value::Lazy1(f) => (arg, Continuation::Lazy0(f.clone(), continuation).into()),
            _ => (arg, Continuation::Apply2(func, continuation).into())
        }
    }

    fn apply_value(&mut self, func: Rc<Value>, arg: Rc<Value>, continuation: Rc<Continuation>) -> (MaybeEvaluated, Rc<Continuation>) {
        match func.as_ref() {
            Value::Char(c) => {
                print!("{}", c);
                (arg.into(), continuation)
            }
            Value::Constant0 => (Value::Constant1(arg).into(), continuation),
            Value::Constant1(k) => (k.clone().into(), continuation),
            Value::Continuation(c) => (arg.into(), c.clone()),
            Value::CreateContinuation => (Value::Continuation(continuation.clone()).into(), Continuation::Apply2(arg, continuation).into()),
            Value::Distribute0 => (Value::Distribute1(arg).into(), continuation),
            Value::Distribute1(f1) => (Value::Distribute2(f1.clone(), arg).into(), continuation),
            Value::Distribute2(f1, f2) => (arg.clone().into(),
                Rc::new(Continuation::Apply2(f1.clone(), Continuation::Distribute(f2.clone(), arg, continuation).into()))),
            Value::Identity => (arg.into(), continuation),
            Value::Lazy0 => (Value::Lazy1(arg.into()).into(), continuation),
            Value::Lazy1(f) => (f.clone(), Continuation::Apply1(arg.into(), continuation).into()),
            Value::PrintCC => {
                if let Some(c) = self.current_char {
                    print!("{}", c);
                    (arg.into(), continuation)
                } else {
                    (Value::Term.into(), continuation)
                }
            }
            Value::Term => (func.into(), continuation),
            Value::Read => {
                let mut buf = [0];
                if let Ok(s) = stdin().read(&mut buf) {
                    if s == 1 {
                        self.current_char = Some(buf[0] as char);
                        self.apply_value(arg, Value::Identity.into(), continuation)
                    } else {
                        self.current_char = None;
                        self.apply_value(arg, Value::Term.into(), continuation)
                    }
                } else {
                    self.current_char = None;
                    self.apply_value(arg, Value::Term.into(), continuation)
                }
            }
        }
    }
    
    fn continu(&mut self, continuation: Rc<Continuation>, value: Rc<Value>) -> (MaybeEvaluated, Rc<Continuation>) {
        match continuation.as_ref() {
            Continuation::End => panic!("Shouldn't have gotten here"),
            Continuation::Apply1(arg, next) => self.maybe_apply(value, arg.clone(), next.clone()),
            Continuation::Apply2(func, next) => self.apply_value(func.clone(), value, next.clone()),
            Continuation::Distribute(f2, arg, next) => (arg.into(),
                Rc::new(Continuation::Apply2(f2.clone(), Rc::new(Continuation::Apply2(value, next.clone()))))),
            Continuation::Lazy0(f, next) => (f.clone(), Rc::new(Continuation::Apply1(value.into(), next.clone()))),
        }
    }
}