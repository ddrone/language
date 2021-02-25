use glob::glob;
use std::process::Command;
use warp::Filter;
use note_rusty::render_cards::render_markdown;

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

fn view_note(name: String) -> String {
    let markdown = std::fs::read_to_string(&name).unwrap();
    let mut response = r#"
    <!DOCTYPE html>
    <html>
        <head>
            <meta charset="utf-8" />
            <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css" integrity="sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X" crossorigin="anonymous">
            <script defer src="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.js" integrity="sha384-g7c+Jr9ZivxKLnZTDUhnkOnsh30B4H0rpLUpJ4jAIKs4fnJI+sEnkvrMWph2EDg4" crossorigin="anonymous"></script>
        </head>
        <body>
    "#.to_string();
    render_markdown(&markdown, &mut response).unwrap();

    response.push_str(r#"
            <script>
                window.addEventListener('load', () => {
                    let nodes = document.querySelectorAll("span.math.inline");
                    for (let i = 0; i < nodes.length; i++) {
                        let node = nodes[i];
                        katex.render(node.innerText, node);
                    }
                });
            </script>
        </body>
    </html>
    "#);
    response
}

#[tokio::main]
async fn main() {
    let root_handler = warp::path::end().map(|| warp::reply::html(generate_link_list()));
    let read_handler =
        warp::path!("read" / String).map(|name| warp::reply::html(view_note(name)));

    println!("Should start serving I think");

    Command::new("xdg-open")
        .args(&["http://localhost:31337"])
        .spawn()
        .unwrap();

    warp::serve(root_handler.or(read_handler))
        .run(([127, 0, 0, 1], 31337))
        .await;
}
