use std::borrow::{Borrow, BorrowMut};

use types::{Ty, Type};

mod types;

#[derive(Debug)]
enum ExpLayer<R> {
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
struct Exp {
    id: Id,
    layer: ExpLayer<Box<Exp>>,
}

fn uint(u: u64) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Uint(u),
    }
}

fn var<S>(s: S) -> Exp
where
    S: ToString,
{
    Exp {
        id: 0,
        layer: ExpLayer::Var(s.to_string()),
    }
}

fn lam<S>(name: S, ty: Ty, body: Exp) -> Exp
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

fn ind<S>(number: Exp, zero_case: Exp, succ_name: S, succ_case: Exp) -> Exp
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

fn add(e1: Exp, e2: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Add(Box::new(e1), Box::new(e2)),
    }
}

fn pair(e1: Exp, e2: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Pair(Box::new(e1), Box::new(e2)),
    }
}

fn first(e: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::First(Box::new(e)),
    }
}

fn second(e: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::Second(Box::new(e)),
    }
}

fn traverse<F>(exp: &mut Exp, f: &mut F)
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

fn assign_unique_ids(exp: &mut Exp) {
    let mut id: u64 = 0;
    traverse(exp, &mut |e: &mut Exp| {
        e.id = id;
        id = id + 1;
    });
}

// TODO: figure out an idiomatic way to do this more performantly in Rust
fn lookup(env: &Vec<(String, Ty)>, key: String) -> Result<Ty, String> {
    for p in env.iter().rev() {
        if p.0.eq(&key) {
            return Ok(p.1.clone());
        }
    }
    return Err(key + " not found");
}

fn typecheck(env: &mut Vec<(String, Ty)>, exp: &Exp) -> Result<Ty, String> {
    match &exp.layer {
        ExpLayer::Uint(_) => Ok(Box::new(Type::Uint)),
        ExpLayer::Var(ref v) => lookup(&env, v.clone()),
        ExpLayer::Lam {
            ref name,
            ref ty,
            ref body,
        } => {
            env.push((name.clone(), ty.clone()));
            let body_ty = typecheck(env, body)?;
            env.pop();
            let result_ty = Box::new(Type::Arr(ty.clone(), body_ty));
            Ok(result_ty)
        }
        ExpLayer::App(ref fun, ref arg) => {
            let fun_ty = typecheck(env, fun)?;
            let arg_ty = typecheck(env, arg)?;
            match *fun_ty {
                Type::Arr(fun_arg, fun_res) => {
                    if fun_arg.eq(&arg_ty) {
                        Ok(fun_res)
                    } else {
                        Err("function applied to wrong argument".to_string())
                    }
                }
                _ => Err("trying to apply non-function".to_string()),
            }
        }
        ExpLayer::Add(ref e1, ref e2) => {
            let ty1 = typecheck(env, e1)?;
            let ty2 = typecheck(env, e2)?;
            if *ty1 == Type::Uint && *ty2 == Type::Uint {
                Ok(ty1)
            } else {
                Err("adding non-integers!".to_string())
            }
        }
        ExpLayer::Pair(ref e1, ref e2) => {
            let ty1 = typecheck(env, e1)?;
            let ty2 = typecheck(env, e2)?;
            Ok(Box::new(Type::Pair(ty1, ty2)))
        }
        ExpLayer::First(ref e) => {
            let ty = typecheck(env, e)?;
            match *ty {
                Type::Pair(ty1, _) => Ok(ty1),
                _ => Err("trying to get first of non-pair".to_string()),
            }
        }
        ExpLayer::Second(ref e) => {
            let ty = typecheck(env, e)?;
            match *ty {
                Type::Pair(_, ty2) => Ok(ty2),
                _ => Err("trying to get first of non-pair".to_string()),
            }
        }
        ExpLayer::Ind {
            ref number,
            ref zero_case,
            ref succ_name,
            ref succ_case,
        } => {
            let number_ty = typecheck(env, number)?;
            let zero_ty = typecheck(env, zero_case)?;
            env.push((succ_name.clone(), zero_ty.clone()));
            let succ_ty = typecheck(env, succ_case)?;
            env.pop();

            if *number_ty == Type::Uint && *zero_ty == *succ_ty {
                Ok(zero_ty)
            } else {
                Err("incorrect induction".to_string())
            }
        }
    }
}

fn main() {
    let mut test = lam(
        "x",
        types::uint(),
        ind(
            var("x"),
            pair(uint(0), uint(1)),
            "p",
            pair(second(var("p")), add(first(var("p")), second(var("p")))),
        ),
    );
    println!("{:?}", &test);
    assign_unique_ids(&mut test);
    println!("{:?}", &test);
    let ty = typecheck(&mut Vec::new(), &test);
    println!("{:?}", &ty);
}
