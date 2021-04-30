fn is_single_token_lexeme(c: char) -> bool {
    c == '(' || c == ')' || c == ','
}

enum State {
    Ambient,
    Started(usize)
}

pub(crate) fn tokenize(input: String) -> Vec<(usize, String)>
{
    let mut result = Vec::new();

    // TODO: this probably is not going to work for non-ascii characters since enumerating chars is
    // not going to return the right byte offsets. Probably should fix this later.
    let mut state = State::Ambient;
    for (i, c) in input.chars().enumerate() {
        match &state {
            State::Ambient => {
                if c == ' ' || c == '\n' {
                    continue
                }
                else if is_single_token_lexeme(c) {
                    result.push((i, c.to_string()))
                }
                else {
                    state = State::Started(i)
                }
            }
            State::Started(s) => {
                if c == ' ' || c == '\n' {
                    result.push((*s, (&input[*s..i]).to_string()));
                    state = State::Ambient
                }
                else if is_single_token_lexeme(c) {
                    result.push((*s, (&input[*s..i]).to_string()));
                    result.push((i, c.to_string()));
                    state = State::Ambient
                }
            }
        }
    }

    result
}
