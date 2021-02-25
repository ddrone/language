use std::process::Command;
use warp::Filter;

#[tokio::main]
async fn main() {
    let root_handler = warp::path::end().map(|| format!("Root goes here"));
    let read_handler = warp::path!("read" / String).map(|name| format!("File {} will go here", name));

    println!("Should start serving I think");

    Command::new("xdg-open")
        .args(&["http://localhost:31337"])
        .spawn()
        .unwrap();

    warp::serve(root_handler.or(read_handler)).run(([127, 0, 0, 1], 31337)).await;
}
