use glob::glob;
use note_rusty::cloze::ClozeChunk;
use note_rusty::render_cards::render_markdown;
use note_rusty::review_cards::{get_reviews, parse_cards};
use note_rusty::Card;
use percent_encoding::percent_decode;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::io::Write;
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

fn view_note(name: String) -> String {
    let decoded_name = percent_decode(&name.as_bytes()).decode_utf8().unwrap();
    let markdown = std::fs::read_to_string(&decoded_name.as_ref()).unwrap();
    let mut response = r#"
    <!DOCTYPE html>
    <html>
        <head>
            <meta charset="utf-8" />
            <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css" integrity="sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X" crossorigin="anonymous">
            <script defer src="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.js" integrity="sha384-g7c+Jr9ZivxKLnZTDUhnkOnsh30B4H0rpLUpJ4jAIKs4fnJI+sEnkvrMWph2EDg4" crossorigin="anonymous"></script>
        </head>
        <body>
            <div class="wrapper">
    "#.to_string();
    render_markdown(&markdown, &mut response).unwrap();

    response.push_str(
        r#"
            </div>
            <script>
                window.addEventListener('load', () => {
                    let nodes = document.querySelectorAll("span.math.inline");
                    for (let i = 0; i < nodes.length; i++) {
                        let node = nodes[i];
                        katex.render(node.innerText, node);
                    }
                });
            </script>
            <style>
                .card {
                    background: #eee;
                    border: 1px solid #ccc;
                    margin: 0.5em 0;
                }
                .wrapper {
                    max-width: 1024px;
                }
            </style>
        </body>
    </html>
    "#,
    );
    response
}

#[derive(Serialize, Deserialize)]
struct CardReview {
    card_index: usize,
    deletion_index: usize,
    rendered: String,
    answer: String,
}

#[derive(Serialize, Deserialize)]
struct CardsResponse {
    file_hash: String,
    cards: Vec<Card>,
    reviews: Vec<CardReview>,
}

fn cards_to_review() -> CardsResponse {
    let cards = parse_cards();
    let (reviews_raw, _) = get_reviews(&cards);

    let mut hasher = Sha256::new();
    for card in &cards {
        hasher.write(card.text.as_ref()).unwrap();
    }

    let mut reviews = Vec::new();
    for (card_index, deletion_index) in reviews_raw {
        let mut source = String::new();
        let mut answer = String::new();
        let mut i = 0;
        for chunk in &cards[card_index].data.chunks {
            match chunk {
                ClozeChunk::Open(s) => source.push_str(&s),
                ClozeChunk::Close { text, .. } => {
                    if i == deletion_index {
                        source.push_str("**...**");
                        answer.push_str(text)
                    } else {
                        source.push_str(text)
                    }
                    i += 1
                }
            }
        }
        let mut rendered = String::new();
        render_markdown(&source, &mut rendered).unwrap();
        reviews.push(CardReview {
            card_index,
            deletion_index,
            rendered,
            answer,
        })
    }

    CardsResponse {
        file_hash: format!("{:X}", hasher.finalize()),
        cards,
        reviews,
    }
}

#[tokio::main]
async fn main() {
    let root_handler = warp::path::end().map(|| warp::reply::html(generate_link_list()));
    let read_handler = warp::path!("read" / String).map(|name| warp::reply::html(view_note(name)));
    let cards_handler = warp::path("review").map(|| warp::reply::json(&cards_to_review()));

    println!("Should start serving I think");

    Command::new("xdg-open")
        .args(&["http://localhost:31337"])
        .spawn()
        .unwrap();

    warp::serve(root_handler.or(read_handler).or(cards_handler))
        .run(([127, 0, 0, 1], 31337))
        .await;
}
