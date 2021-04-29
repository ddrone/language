enum Type {
    Unit,
    Arr(Ty, Ty)
}
type Ty = Box<Type>;

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

struct Exp {
    id: Id,
    layer: ExpLayer<Box<Exp>>
}

fn main() {
    println!("Hello, world!");
}
