use crate::cloze::{Cloze, ClozeChunk};
use crate::*;
use chrono::Duration;
use rand::seq::SliceRandom;
use rand::thread_rng;
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
                    print!("[...]");
                    back = text.clone()
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
    input.truncate(input.len().saturating_sub(1)); // chop off trailing line break
    if back == input {
        return true;
    }
    println!("Exact match not detected.");
    println!("Expected: {}", back);
    println!("Actual: {}", input);
    print!("Did you get it right? ");
    yes_no()
}

pub fn clear_screen() {
    print!("\x1B[2J\x1B[1;1H");
}

pub fn start_review() {
    let input = fs::read_to_string("cards.json").expect("oops");
    let mut cards: Vec<Card> = serde_json::from_str(&input).expect("parse failed");
    let mut reviews: Vec<(usize, usize)> = Vec::new();

    for (i, card) in (&cards).into_iter().enumerate() {
        for (j, review) in (&card.review).into_iter().enumerate() {
            let time = Utc::now();
            let days_to_review = 2_i64.pow(review.bucket as u32) - 1;
            let next_review = review
                .last_reviewed
                .checked_add_signed(Duration::days(days_to_review as i64))
                .unwrap();
            println!("Next review: {}", next_review);
            if next_review < time {
                reviews.push((i, j));
            }
        }
    }

    reviews.shuffle(&mut thread_rng());
    let cards_to_review = 30;
    let mut reviews_pending = reviews.len().saturating_sub(cards_to_review);
    reviews.truncate(cards_to_review);

    for (i, j) in reviews {
        let card = &mut cards[i];
        let review = &mut card.review[j];
        if review_card(&card.data, j) {
            review.bucket = review.bucket + 1;
            clear_screen();
            println!("Moving to bucket {}", review.bucket);
        } else {
            review.bucket = 0;
            reviews_pending += 1;
            clear_screen();
            println!("Moving to first bucket!");
        }
        review.last_reviewed = Utc::now();
    }

    let json = serde_json::to_string(&cards).expect("serialize failed");
    std::fs::write("cards.json", json).expect("write failed");

    println!("{} reviews pending", reviews_pending);
}
