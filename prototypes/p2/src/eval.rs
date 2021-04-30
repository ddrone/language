pub enum Value {
    Uint(u64),
    Pair(Val, Val),
}

type Val = Box<Value>;
