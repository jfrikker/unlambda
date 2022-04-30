use std::{rc::Rc, fmt::Display};

use nom::{character::complete::{anychar, none_of}, IResult, multi::many1_count};

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Apply(Rc<AstNode>, Rc<AstNode>),
    Char(char),
    Constant,
    Continuation,
    Distribute,
    Identity,
    Lazy,
    PrintCC,
    Read,
    Term,
}

impl Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Apply(arg1, arg2) => write!(f, "`{}{}", arg1, arg2),
            Self::Char('\n') => write!(f, "r"),
            Self::Char(c) => write!(f, ".{}", c),
            Self::Constant => write!(f, "k"),
            Self::Continuation => write!(f, "c"),
            Self::Distribute => write!(f, "s"),
            Self::Identity => write!(f, "i"),
            Self::Lazy => write!(f, "d"),
            Self::PrintCC => write!(f, "|"),
            Self::Read => write!(f, "@"),
            Self::Term => write!(f, "v"),
        }
    }
}

pub fn parse(i: &str) -> IResult<&str, Rc<AstNode>> {
    let (i, c) = anychar(i)?;
    match c {
        '`' => apply(i),
        'c' => Ok((i, Rc::new(AstNode::Continuation))),
        'd' => Ok((i, Rc::new(AstNode::Lazy))),
        'i' => Ok((i, Rc::new(AstNode::Identity))),
        'k' => Ok((i, Rc::new(AstNode::Constant))),
        'r' => Ok((i, Rc::new(AstNode::Char('\n')))),
        's' => Ok((i, Rc::new(AstNode::Distribute))),
        'v' => Ok((i, Rc::new(AstNode::Term))),
        '@' => Ok((i, Rc::new(AstNode::Read))),
        '|' => Ok((i, Rc::new(AstNode::PrintCC))),
        '.' => print_char(i),
        '\n' => parse(i),
        ' ' => parse(i),
        '#' => {
            let (i, _) = many1_count(none_of("\n"))(i)?;
            parse(i)
        }
        x => panic!("invalid character {}", x),
    }
}

fn apply(i: &str) -> IResult<&str, Rc<AstNode>> {
    let (i, f) = parse(i)?;
    let (i, a) = parse(i)?;
    Ok((i, Rc::new(AstNode::Apply(f, a))))
}

fn print_char(i: &str) -> IResult<&str, Rc<AstNode>> {
    let (i, c) = anychar(i)?;
    Ok((i, Rc::new(AstNode::Char(c))))
}