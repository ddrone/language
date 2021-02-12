use note_rusty::pdf_annotations::get_annotations;
use note_rusty::server::start_server;
use note_rusty::{parse_file, Card};

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

fn parse_cards(names: &[String]) {
    let mut cards: Vec<Card> = Vec::new();
    for name in names {
        cards.extend(parse_file(name))
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
        parse_cards(&args[2..])
    } else if args[2] == "serve" {
        start_server();
    } else {
        println!("Unrecognized command {}", args[1]);
    }
}
