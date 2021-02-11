use chrono::prelude::*;
use chrono::Duration;
use note_rusty::cloze::{Cloze, ClozeChunk};
use note_rusty::*;
use std::fs;
use std::io;
use std::io::Write;

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

fn review_card(card: &Cloze, close_id: usize) -> bool {
    let mut back = String::new();
    let mut i: usize = 0;
    for chunk in &card.chunks {
        match chunk {
            ClozeChunk::Open(s) => print!("{}", s),
            ClozeChunk::Close { text, .. } => {
                if i == close_id {
                    print!("[...]")
                } else {
                    print!("{}", text)
                }
                i += 1;
            }
        }
    }
    let mut input: String = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("did not get input");
    println!("Expected: {}", back);
    println!("Actual: {}", input);
    print!("Did you get it right? ");
    yes_no()
}

fn main() {
    let input = fs::read_to_string("cards.json").expect("oops");
    let mut cards: Vec<Card> = serde_json::from_str(&input).expect("parse failed");

    for card in &mut cards {
        for (id, review) in (&mut card.review).into_iter().enumerate() {
            let time = Utc::now();
            let days_to_review = 2_i64.pow(review.bucket as u32) - 1;
            let next_review = review
                .last_reviewed
                .checked_add_signed(Duration::days(days_to_review as i64))
                .unwrap();
            println!("Next review: {}", next_review);
            if next_review < time {
                if review_card(&card.data, id) {
                    review.bucket = review.bucket + 1;
                    println!("Moving to bucket {}", review.bucket);
                } else {
                    review.bucket = 0;
                    println!("Moving to first bucket!");
                }
                review.last_reviewed = Utc::now();
            }
        }
    }

    let json = serde_json::to_string(&cards).expect("serialize failed");
    std::fs::write("cards.json", json).expect("write failed");
}
