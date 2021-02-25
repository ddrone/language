use warp::Filter;
use std::process::Command;

#[tokio::main]
async fn main() {
    let server = warp::path!("read" / String)
        .map(|name| format!("File {} will go here", name));

    println!("Should start serving I think");

    Command::new("xdg-open")
        .args(&["http://localhost:31337/read/note.md"])
        .spawn()
        .unwrap();

    warp::serve(server)
        .run(([127, 0, 0, 1], 31337))
        .await;
}
