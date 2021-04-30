use std::borrow::{Borrow, BorrowMut};

#[derive(Debug, Eq, PartialEq, Clone)]
enum Type {
    Uint,
    Arr(Ty, Ty),
}
type Ty = Box<Type>;

#[derive(Debug)]
enum ExpLayer<R> {
    Uint(u64),
    Var(String),
    Lam { name: String, ty: Ty, body: R },
    App(R, R),
}

type Id = u64;

#[derive(Debug)]
struct Exp {
    id: Id,
    layer: ExpLayer<Box<Exp>>,
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

fn app(e1: Exp, e2: Exp) -> Exp {
    Exp {
        id: 0,
        layer: ExpLayer::App(Box::new(e1), Box::new(e2)),
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
    }
}

fn main() {
    let mut test = lam("x", Box::new(Type::Uint), var("x"));
    println!("{:?}", &test);
    assign_unique_ids(&mut test);
    println!("{:?}", &test);
    let ty = typecheck(&mut Vec::new(), &test);
    println!("{:?}", &ty);
}
