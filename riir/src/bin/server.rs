use glob::glob;
use std::process::Command;
use warp::Filter;

fn generate_link_list() -> String {
    let mut result = String::new();
    result.push_str("<ul>");

    for entry in glob("**/*.md").unwrap() {
        let file_path = entry.unwrap();
        let file_name = file_path.to_str().unwrap();
        result.push_str(&format!(
            r#"
            <li><a href="/read/{}">{}</a></li>
        "#,
            file_name, file_name
        ));
    }

    result.push_str("</ul>");

    result
}

#[tokio::main]
async fn main() {
    let root_handler = warp::path::end().map(|| warp::reply::html(generate_link_list()));
    let read_handler =
        warp::path!("read" / String).map(|name| format!("File {} will go here", name));

    println!("Should start serving I think");

    Command::new("xdg-open")
        .args(&["http://localhost:31337"])
        .spawn()
        .unwrap();

    warp::serve(root_handler.or(read_handler))
        .run(([127, 0, 0, 1], 31337))
        .await;
}
