use note_rusty::*;
use std::fs;
use std::io;
use std::io::Write;
use chrono::prelude::*;
use chrono::Duration;

fn yes_no() -> bool {
    let mut input: String = String::new();
    loop {
        io::stdout().flush().expect("i/o error");
        io::stdin()
            .read_line(&mut input)
            .expect("did not get input");
        if input == "yes\n" {
            return true;
        } else if input == "no\n" {
            return false;
        } else {
            print!("Did not get expected answer, enter 'yes' or 'no': ");
        }
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
    let mut cards: Vec<Card> = serde_json::from_str(&input).expect("parse failed");

    for card in &mut cards {
        let time = Utc::now();
        let days_to_review = 2_i64.pow(card.bucket as u32) - 1;
        let next_review = card.last_reviewed.checked_add_signed(Duration::days(days_to_review as i64)).unwrap();
        println!("Next review: {}", next_review);
        if next_review < time {
            if review_card(&card.data) {
                card.bucket = card.bucket + 1;
                println!("Moving to bucket {}", card.bucket);
            } else {
                card.bucket = 0;
                println!("Moving to first bucket!");
            }
            card.last_reviewed = Utc::now();
        }
    }

    let json = serde_json::to_string(&cards).expect("serialize failed");
    std::fs::write("cards.json", json).expect("write failed");
}
