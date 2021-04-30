#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Uint,
    Arr(Ty, Ty),
    Pair(Ty, Ty),
}

pub type Ty = Box<Type>;

pub fn uint() -> Ty {
    Box::new(Type::Uint)
}

pub fn arr(fun: Ty, arg: Ty) -> Ty {
    Box::new(Type::Arr(fun, arg))
}

pub fn pair(t1: Ty, t2: Ty) -> Ty {
    Box::new(Type::Pair(t1, t2))
}
