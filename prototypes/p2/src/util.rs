// TODO: figure out an idiomatic way to do this more performantly in Rust
pub fn lookup<R>(env: &Vec<(String, R)>, key: String) -> Result<R, String>
where R: Clone {
    for p in env.iter().rev() {
        if p.0.eq(&key) {
            return Ok(p.1.clone());
        }
    }
    return Err(key + " not found");
}
