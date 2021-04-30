use std::borrow::{Borrow, BorrowMut};

use crate::parser::parse;
use expr::{Exp, ExpLayer};
use types::{Ty, Type};

mod eval;
mod expr;
mod parser;
mod types;
mod util;

fn assign_unique_ids(exp: &mut Exp) {
    let mut id: u64 = 0;
    expr::traverse(exp, &mut |e: &mut Exp| {
        e.id = id;
        id = id + 1;
    });
}

fn typecheck(env: &mut Vec<(String, Ty)>, exp: &Exp) -> Result<Ty, String> {
    match &exp.layer {
        ExpLayer::Uint(_) => Ok(Box::new(Type::Uint)),
        ExpLayer::Var(ref v) => util::lookup(&env, v.clone()),
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
    let mut test = expr::ind(
        expr::uint(10),
        expr::pair(expr::uint(0), expr::uint(1)),
        "p",
        expr::pair(
            expr::second(expr::var("p")),
            expr::add(expr::first(expr::var("p")), expr::second(expr::var("p"))),
        ),
    );
    println!("{:?}", &test);
    assign_unique_ids(&mut test);
    println!("{:?}", &test);
    let ty = typecheck(&mut Vec::new(), &test);
    println!("{:?}", &ty);
    let value = eval::eval(&mut Vec::new(), &test);
    println!("{:?}", &value);
    let tokens = parser::tokenize("(second(p), first(p))".to_string());
    println!("{:?}", &tokens);
    let parse_result = parse(&mut (tokens.iter().peekable()));
    println!("{:?}", &parse_result);
}
