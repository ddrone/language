use note_rusty::*;
use std::fs;

fn main() {
    let cards: Vec<Card> = parse_file("note.md");
    let json = serde_json::to_string(&cards).expect("serialize failed");
    fs::write("cards.json", json).expect("write failed");
}
