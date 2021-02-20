use note_rusty::pdf_annotations::get_annotations;
use note_rusty::render_cards::render_cards;
use note_rusty::review_cards::start_review;
use note_rusty::server::start_server;
use note_rusty::{parse_file, read_cards_index, Card};
use std::fs;

fn print_annotations(names: &[String]) {
    for arg in names {
        let annotations = get_annotations(arg);
        if annotations.is_empty() {
            println!("{}: no annotations", arg);
        } else {
            println!("{}", arg);
            for ann in annotations {
                println!("* {}", ann);
            }
        }
    }
}

fn parse_cards() {
    let mut cards: Vec<Card> = Vec::new();
    let index = read_cards_index();
    let paths = fs::read_dir("./").unwrap();
    for path in paths {
        if let Ok(entry) = path {
            if let Some(ext) = entry.path().extension() {
                if ext == "md" {
                    for card in parse_file(entry.path()) {
                        match index.get(&card.text) {
                            None => cards.push(card),
                            Some(old_card) => {
                                println!("Ignoring {}, already exists", &card.text);
                                cards.push(old_card.clone())
                            }
                        }
                    }
                } else {
                    println!("Ignoring {:?}, not markdown file, ext = {:?}", entry, ext);
                }
            }
        }
    }
    let json = serde_json::to_string(&cards).expect("serialize failed");
    std::fs::write("cards.json", json).expect("write failed");
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Command must be provided");
        return;
    }

    if args[1] == "pdf" {
        print_annotations(&args[2..])
    } else if args[1] == "parse" {
        parse_cards()
    } else if args[1] == "serve" {
        start_server();
    } else if args[1] == "review" {
        start_review();
    } else if args[1] == "render" {
        render_cards().unwrap();
    } else {
        println!("Unrecognized command {}", args[1]);
    }
}
