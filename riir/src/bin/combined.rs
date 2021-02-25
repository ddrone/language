use note_rusty::pdf_annotations::get_annotations;
use note_rusty::render_cards::render_cards;
use note_rusty::review_cards::start_review;

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

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Command must be provided");
        return;
    }

    if args[1] == "pdf" {
        print_annotations(&args[2..])
    } else if args[1] == "review" {
        start_review();
    } else if args[1] == "render" {
        render_cards().unwrap();
    } else {
        println!("Unrecognized command {}", args[1]);
    }
}
