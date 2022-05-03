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
    Continuation(Vec<Continuation>),
    CreateContinuation,
    Distribute0,
    Distribute1(Rc<Value>),
    Distribute2(Rc<Value>, Rc<Value>),
    Identity,
    Lazy0,
    Lazy1(Rc<AstNode>),
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
            Self::Continuation(next) => write!(f, "<cont:{:?}>", next),
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

#[derive(Clone, Debug, PartialEq)]
enum Continuation {
    EvalApply(Rc<AstNode>),
    Apply1(Rc<Value>),
    Apply2(Rc<Value>),
    Distribute(Rc<Value>, Rc<Value>),
}

struct Unlambda {
    current_char: Option<char>,
    stack: Vec<Continuation>,
    constant0: Rc<Value>,
    create_continuation: Rc<Value>,
    distribute0: Rc<Value>,
    identity: Rc<Value>,
    lazy0: Rc<Value>,
    printcc: Rc<Value>,
    read: Rc<Value>,
    term: Rc<Value>,
}

impl Default for Unlambda {
    fn default() -> Self {
        Self {
            current_char: Default::default(),
            stack: Vec::new(),
            constant0: Rc::new(Value::Constant0),
            create_continuation: Rc::new(Value::CreateContinuation),
            distribute0: Rc::new(Value::Distribute0),
            identity: Rc::new(Value::Identity),
            lazy0: Rc::new(Value::Lazy0),
            printcc: Rc::new(Value::PrintCC),
            read: Rc::new(Value::Read),
            term: Rc::new(Value::Term),
        }
    }
}

impl Unlambda {
    fn run(&mut self, prog: Rc<AstNode>) -> MaybeEvaluated {
        let mut prog = self.step(prog.into());
        while !self.stack.is_empty() {
            // println!("{} -- {:?}", prog, self.stack);
            prog = self.step(prog);
        }
        prog
    }

    fn step(&mut self, prog: MaybeEvaluated) -> MaybeEvaluated {
        match prog {
            MaybeEvaluated::Unevaluated(ast) => {
                match ast.as_ref() {
                    AstNode::Apply(f, a) => {
                        self.stack.push(Continuation::EvalApply(a.clone()));
                        f.into()
                    }
                    AstNode::Char(c) => self.continu(Value::Char(*c).into()),
                    AstNode::Constant => self.continu(self.constant0.clone().into()),
                    AstNode::Continuation => self.continu(self.create_continuation.clone().into()),
                    AstNode::Identity => self.continu(self.identity.clone().into()),
                    AstNode::Distribute => self.continu(self.distribute0.clone().into()),
                    AstNode::Lazy => self.continu(self.lazy0.clone().into()),
                    AstNode::Term => self.continu(self.term.clone().into()),
                    AstNode::Read => self.continu(self.read.clone().into()),
                    AstNode::PrintCC => self.continu(self.printcc.clone().into()),
                }
            }
            MaybeEvaluated::Evaluated(v) => self.continu(v)
        }
    }

    fn apply(&mut self, func: &Value, arg: Rc<Value>) -> MaybeEvaluated {
        match func {
            Value::Char(c) => {
                print!("{}", c);
                arg.into()
            }
            Value::Constant0 => Value::Constant1(arg).into(),
            Value::Constant1(k) => k.clone().into(),
            Value::Continuation(c) => {
                self.stack.clear();
                self.stack.extend(c.iter().cloned());
                arg.into()
            }
            Value::CreateContinuation => {
                let continuation = self.stack.clone();
                self.stack.push(Continuation::Apply2(arg));
                Value::Continuation(continuation).into()
            }
            Value::Distribute0 => Value::Distribute1(arg).into(),
            Value::Distribute1(f1) => Value::Distribute2(f1.clone(), arg).into(),
            Value::Distribute2(f1, f2) => {
                self.stack.push(Continuation::Distribute(f2.clone(), arg.clone()));
                self.stack.push(Continuation::Apply2(f1.clone()));
                arg.into()
            }
            Value::Identity => arg.into(),
            Value::Lazy0 => arg.into(),
            Value::Lazy1(f) => {
                self.stack.push(Continuation::Apply1(arg.into()));
                f.into()
            }
            Value::PrintCC => {
                if let Some(c) = self.current_char {
                    print!("{}", c);
                    arg.into()
                } else {
                    self.term.clone().into()
                }
            }
            Value::Term => self.term.clone().into(),
            Value::Read => {
                let mut buf = [0];
                if let Ok(s) = stdin().read(&mut buf) {
                    if s == 1 {
                        self.current_char = Some(buf[0] as char);
                        self.apply(&arg, Value::Identity.into())
                    } else {
                        self.current_char = None;
                        self.apply(&arg, Value::Term.into())
                    }
                } else {
                    self.current_char = None;
                    self.apply(&arg, Value::Term.into())
                }
            }
        }
    }
    
    fn continu(&mut self, value: Rc<Value>) -> MaybeEvaluated {
        match self.stack.pop().unwrap() {
            Continuation::EvalApply(arg) => {
                match value.as_ref() {
                    Value::Lazy0 => {
                        Value::Lazy1(arg.clone()).into()
                    }
                    _ => {
                        self.stack.push(Continuation::Apply2(value.clone()));
                        arg.clone().into()
                    }
                }
            }
            Continuation::Apply1(arg) => {
                self.apply(&value, arg.clone())
            }
            Continuation::Apply2(func) => {
                self.apply(&func, value)
            }
            Continuation::Distribute(f2, arg) => {
                self.stack.push(Continuation::Apply2(value));
                self.stack.push(Continuation::Apply2(f2.clone()));
                arg.into()
            }
        }
    }
}