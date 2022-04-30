use std::{io::{stdin, Read}, fs::File, env::args, rc::Rc};

use parser::AstNode;

use crate::parser::parse;

mod parser;

fn main() {
    let mut code = String::new();
    File::open(args().nth(1).unwrap()).unwrap().read_to_string(&mut code).unwrap();
    let parsed = parse(&code).unwrap().1;

    println!("{:?}", parsed);
    println!("{:?}", Unlambda::default().run(parsed.into()));
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

#[derive(Clone, Debug, PartialEq)]
enum MaybeEvaluated {
    Evaluated(Rc<Value>),
    Unevaluated(Rc<AstNode>),
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

#[derive(Default)]
struct Unlambda {
    current_char: Option<char>,
}

impl Unlambda {
    fn run(&mut self, prog: Rc<AstNode>) -> MaybeEvaluated {
        let (mut prog, mut continuation) = self.step(prog.into(), Continuation::End.into());
        while *continuation != Continuation::End {
            // println!("{:?} -- {:?}", prog, continuation);
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
            MaybeEvaluated::Evaluated(v) => self.continu(continuation, v.clone())
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
            Value::CreateContinuation => (Value::Continuation(continuation.clone()).into(), Continuation::Apply2(arg.into(), continuation).into()),
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
            Continuation::Distribute(f2, arg, next) => (MaybeEvaluated::Evaluated(arg.clone()),
                Rc::new(Continuation::Apply2(f2.clone(), Rc::new(Continuation::Apply2(value, next.clone()))))),
            Continuation::Lazy0(f, next) => (f.clone(), Rc::new(Continuation::Apply1(MaybeEvaluated::Evaluated(value), next.clone()))),
        }
    }
}