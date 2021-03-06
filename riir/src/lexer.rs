use regex::Regex;

pub fn to_chunks(source: &str) -> Vec<String> {
    let regex = Regex::new(r"(?:\{\{.*?}})|(?:\$.*?\$)").unwrap();
    let regex_delim = Regex::new(r"\{\{|}}|\$").unwrap();

    for chunk in regex_delim.find_iter(source) {
        println!("{:?}", chunk.as_str())
    }

    regex.split(source).map(|s| s.to_string()).collect()
}

#[cfg(test)]
mod tests {
    use crate::lexer::to_chunks;

    #[test]
    fn split() {
        let result = to_chunks(r"There are {{deletions}} and formulas: $x^2 + y^2 = z^2$");
        println!("{:?}", &result)
    }

    #[test]
    fn multiple_entries() {
        let result = to_chunks(r"$f: X \to Y$ with condition that $f(x_0) = y_0$");
        println!("{:?}", &result)
    }
}