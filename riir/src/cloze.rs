#[derive(Eq, PartialEq, Debug)]
enum ClozeChunk<T> {
    Open(T),
    Close {
        id: Option<T>,
        text: T,
        hint: Option<T>,
    },
}

#[derive(Eq, PartialEq, Debug)]
pub struct Cloze<'a> {
    chunks: Vec<ClozeChunk<&'a str>>,
}

fn parse_close_cloze<'a, 'b>(
    text: &'a str,
    sink: &'b mut Vec<ClozeChunk<&'a str>>,
) -> Option<usize> {
    match text.find("}}") {
        None => None,
        Some(end) => {
            let parts: Vec<&'a str> = text[..end].split("::").collect();
            match parts.len() {
                1 => {
                    // Interpret full thing as text, no id
                    sink.push(ClozeChunk::Close {
                        id: None,
                        text: parts[0],
                        hint: None,
                    });
                    Some(end + 2)
                }
                2 => {
                    // Text and id, no hint
                    sink.push(ClozeChunk::Close {
                        id: Some(parts[0]),
                        text: parts[1],
                        hint: None,
                    });
                    Some(end + 2)
                }
                3 => {
                    // Text, id and hint
                    sink.push(ClozeChunk::Close {
                        id: Some(parts[0]),
                        text: parts[1],
                        hint: Some(parts[2]),
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
                    chunks.push(ClozeChunk::Open(text));
                }
                break;
            }
            Some(start) => {
                if start > 0 {
                    chunks.push(ClozeChunk::Open(&text[..start]));
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
                        id: None,
                        text: "London",
                        hint: None
                    },
                    ClozeChunk::Open(" is the capital of "),
                    ClozeChunk::Close {
                        id: Some("c1"),
                        text: "Great Britain",
                        hint: Some("country")
                    }
                ]
            })
        )
    }
}
