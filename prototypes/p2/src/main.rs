use std::borrow::BorrowMut;

#[derive(Debug)]
enum Type {
    Unit,
    Arr(Ty, Ty)
}
type Ty = Box<Type>;

#[derive(Debug)]
enum ExpLayer<R> {
    Unit,
    Var(String),
    Lam {
        name: String,
        ty: Type,
        body: R
    },
    App(R, R)
}

type Id = u64;

#[derive(Debug)]
struct Exp {
    id: Id,
    layer: ExpLayer<Box<Exp>>
}

fn traverse<F>(exp: &mut Exp, f: &mut F) where
    F: FnMut(&mut Exp) -> ()
{
    f(exp);
    match &mut exp.layer {
        ExpLayer::Unit => {}
        ExpLayer::Var(_) => {}
        ExpLayer::Lam { ref mut body, .. } => {
            traverse(body.borrow_mut(), f)
        }
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

fn main() {
    println!("Hello, world!");
}
