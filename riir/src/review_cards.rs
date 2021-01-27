use note_rusty::*;
use std::fs;
use std::io;
use std::io::Write;

fn yes_no() -> bool {
    io::stdout().flush().expect("i/o error");
    let mut input: String = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("did not get input");
    if input == "yes\n" {
        true
    } else if input == "no\n" {
        false
    } else {
        print!("Did not get expected answer, enter 'yes' or 'no': ");
        yes_no()
    }
}

fn review_card(card: &CardData) -> bool {
    match card {
        CardData::Simple { front, back } => {
            println!("{}", front);
            let mut input: String = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("did not get input");
            println!("Expected: {}", back);
            println!("Actual: {}", input);
        }
        CardData::Cloze {
            text,
            hint: _,
            answer,
        } => {
            println!("{}", text);
            // TODO: do not forget to use hint in the next iteration
            let mut input: String = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("did not get input");
            println!("Expected: {}", answer);
            println!("Actual: {}", input);
        }
    }
    print!("Did you get it right? ");
    yes_no()
}

fn main() {
    let input = fs::read_to_string("cards.json").expect("oops");
    let cards: Vec<Card> = serde_json::from_str(&input).expect("parse failed");

    for card in cards {
        review_card(&card.data);
    }
}
