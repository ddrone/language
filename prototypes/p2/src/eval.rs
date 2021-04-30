use crate::expr::{Exp, ExpLayer};
use crate::util::lookup;

#[derive(Clone, Debug)]
pub enum Value {
    Uint(u64),
    Pair(Val, Val),
}

type Val = Box<Value>;

type Env = Vec<(String, Value)>;

pub fn eval(env: &mut Vec<(String, Value)>, exp: &Exp) -> Result<Value, String> {
    match &exp.layer {
        ExpLayer::Uint(e) => Ok(Value::Uint(*e)),
        ExpLayer::Var(v) => lookup(env, v.clone()),
        ExpLayer::Lam { .. } => {
            unimplemented!()
        }
        ExpLayer::App(_, _) => {
            unimplemented!()
        }
        ExpLayer::Add(e1, e2) => {
            let v1 = eval(env, e1)?;
            let v2 = eval(env, e2)?;
            if let Value::Uint(i1) = v1 {
                if let Value::Uint(i2) = v2 {
                    Ok(Value::Uint(i1 + i2))
                } else {
                    Err("trying to add non-integers".to_string())
                }
            } else {
                Err("trying to add non-integers".to_string())
            }
        }
        ExpLayer::Pair(e1, e2) => {
            let v1 = eval(env, e1)?;
            let v2 = eval(env, e2)?;
            Ok(Value::Pair(Box::new(v1), Box::new(v2)))
        }
        ExpLayer::First(e) => {
            let v = eval(env, e)?;
            if let Value::Pair(v1, _) = v {
                Ok(*v1)
            } else {
                Err("trying to extract first of non-pair".to_string())
            }
        }
        ExpLayer::Second(e) => {
            let v = eval(env, e)?;
            if let Value::Pair(_, v2) = v {
                Ok(*v2)
            } else {
                Err("trying to extract first of non-pair".to_string())
            }
        }
        ExpLayer::Ind {
            number,
            zero_case,
            succ_name,
            succ_case,
        } => {
            let v = eval(env, number)?;
            if let Value::Uint(i) = v {
                let mut curr: u64 = 0;
                let mut curr_value = eval(env, zero_case)?;
                while curr < i {
                    env.push((
                        succ_name.clone(),
                        std::mem::replace(&mut curr_value, Value::Uint(0)),
                    ));
                    curr_value = eval(env, succ_case)?;
                    env.pop();
                    curr += 1;
                }
                Ok(curr_value)
            } else {
                Err("trying to do induction on non-number".to_string())
            }
        }
    }
}
