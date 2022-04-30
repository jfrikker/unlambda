use std::rc::Rc;

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