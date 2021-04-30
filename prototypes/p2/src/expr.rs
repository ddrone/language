use std::borrow::BorrowMut;

use crate::types::Ty;

#[derive(Debug)]
pub enum ExpLayer<R> {
    Uint(u64),
    Var(String),
    Lam {
        name: String,
        ty: Ty,
        body: R,
    },
    App(R, R),
    Add(R, R),
    Pair(R, R),
    First(R),
    Second(R),
    Ind {
        // TODO: is there a name for "thing you do an induction on"?
        number: R,
        zero_case: R,
        succ_name: String,
        succ_case: R,
    },
}

type Id = u64;

#[derive(Debug)]
pub struct Exp {
    pub(crate) id: Id,
    pub(crate) layer: ExpLayer<Box<Exp>>,
}

pub fn uint(u: u64) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Uint(u),
    }
}

pub fn var<S>(s: S) -> Exp
where
    S: ToString,
{
    Exp {
        id: 0,
        layer: ExpLayer::Var(s.to_string()),
    }
}

pub fn lam<S>(name: S, ty: Ty, body: Exp) -> Exp
where
    S: ToString,
{
    Exp {
        id: 0,
        layer: ExpLayer::Lam {
            name: name.to_string(),
            ty,
            body: Box::new(body),
        },
    }
}

pub fn ind<S>(number: Exp, zero_case: Exp, succ_name: S, succ_case: Exp) -> Exp
where
    S: ToString,
{
    Exp {
        id: 0,
        layer: ExpLayer::Ind {
            number: Box::new(number),
            zero_case: Box::new(zero_case),
            succ_name: succ_name.to_string(),
            succ_case: Box::new(succ_case),
        },
    }
}

fn app(e1: Exp, e2: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::App(Box::new(e1), Box::new(e2)),
    }
}

pub fn add(e1: Exp, e2: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Add(Box::new(e1), Box::new(e2)),
    }
}

pub fn pair(e1: Exp, e2: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Pair(Box::new(e1), Box::new(e2)),
    }
}

pub fn first(e: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::First(Box::new(e)),
    }
}

pub fn second(e: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Second(Box::new(e)),
    }
}

pub fn traverse<F>(exp: &mut Exp, f: &mut F)
where
    F: FnMut(&mut Exp) -> (),
{
    f(exp);
    match &mut exp.layer {
        ExpLayer::Uint(_) => {}
        ExpLayer::Var(_) => {}
        ExpLayer::Lam { ref mut body, .. } => traverse(body.borrow_mut(), f),
        ExpLayer::App(ref mut fun, ref mut arg) => {
            traverse(fun.borrow_mut(), f);
            traverse(arg.borrow_mut(), f);
        }
        ExpLayer::Add(ref mut e1, ref mut e2) => {
            traverse(e1.borrow_mut(), f);
            traverse(e2.borrow_mut(), f);
        }
        ExpLayer::Pair(ref mut e1, ref mut e2) => {
            traverse(e1.borrow_mut(), f);
            traverse(e2.borrow_mut(), f);
        }
        ExpLayer::First(ref mut e) => {
            traverse(e.borrow_mut(), f);
        }
        ExpLayer::Second(ref mut e) => {
            traverse(e.borrow_mut(), f);
        }
        ExpLayer::Ind {
            ref mut number,
            ref mut zero_case,
            ref mut succ_case,
            ..
        } => {
            traverse(number.borrow_mut(), f);
            traverse(zero_case.borrow_mut(), f);
            traverse(succ_case.borrow_mut(), f);
        }
    }
}
