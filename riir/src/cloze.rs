use serde::{Deserialize, Serialize};

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub enum ClozeChunk {
    Open(String),
    Close {
        id: String,
        text: String,
        hint: Option<String>,
    },
}

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Cloze {
    pub chunks: Vec<ClozeChunk>,
}

fn parse_close_cloze<'a, 'b>(text: &'a str, sink: &'b mut Vec<ClozeChunk>) -> Option<usize> {
    match text.find("}}") {
        None => None,
        Some(end) => {
            let parts: Vec<&'a str> = text[..end].split("::").collect();
            match parts.len() {
                1 => {
                    // Interpret full thing as text, no id
                    sink.push(ClozeChunk::Close {
                        id: String::new(),
                        text: parts[0].to_string(),
                        hint: None,
                    });
                    Some(end + 2)
                }
                2 => {
                    // Text and id, no hint
                    sink.push(ClozeChunk::Close {
                        id: parts[0].to_string(),
                        text: parts[1].to_string(),
                        hint: None,
                    });
                    Some(end + 2)
                }
                3 => {
                    // Text, id and hint
                    sink.push(ClozeChunk::Close {
                        id: parts[0].to_string(),
                        text: parts[1].to_string(),
                        hint: Some(parts[2].to_string()),
                    });
                    Some(end + 2)
                }
                _ => {
                    // Wrong number of chunks
                    None
                }
            }
        }
    }
}

pub fn parse_cloze(mut text: &str) -> Option<Cloze> {
    let mut chunks = Vec::new();
    loop {
        match text.find("{{") {
            None => {
                if !text.is_empty() {
                    chunks.push(ClozeChunk::Open(text.to_string()));
                }
                break;
            }
            Some(start) => {
                if start > 0 {
                    chunks.push(ClozeChunk::Open(text[..start].to_string()));
                }
                text = &text[start + 2..];
                match parse_close_cloze(&text, &mut chunks) {
                    None => return None,
                    Some(end) => text = &text[end..],
                }
            }
        }
    }
    Some(Cloze { chunks })
}

#[cfg(test)]
mod tests {
    use crate::cloze::{parse_cloze, Cloze, ClozeChunk};

    #[test]
    fn test_parse() {
        let result = parse_cloze("{{London}} is the capital of {{c1::Great Britain::country}}");
        assert_eq!(
            result,
            Some(Cloze {
                chunks: vec![
                    ClozeChunk::Close {
                        id: String::new(),
                        text: "London".to_string(),
                        hint: None
                    },
                    ClozeChunk::Open(" is the capital of ".to_string()),
                    ClozeChunk::Close {
                        id: "c1".to_string(),
                        text: "Great Britain".to_string(),
                        hint: Some("country".to_string())
                    }
                ]
            })
        )
    }
}
