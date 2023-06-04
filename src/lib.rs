pub enum Error {
    UnexpectedIndentation {
        line_number: usize,
        present_indentation: usize,
        expected_indentation_levels: Vec<usize>,
    },
}

pub struct Filler<'a> {
    words: Vec<Word<'a>>,
}

pub enum Word<'a> {
    Raw(&'a str),
}

pub struct ActionInvocation<'a> {
    contents: Filler<'a>,
    attached_invocations: Vec<ActionInvocation<'a>>,
}

fn parse_filler(words: &str) -> Filler {
    Filler {
        words: words
            .split_whitespace()
            .map(|word| Word::Raw(word))
            .collect(),
    }
}

mod lines {
    use super::*;

    pub enum Error {
        UnexpectedIndentation {
            line_index: usize,
            present_indentation_level: usize,
            expected_indentation_levels_range: Vec<usize>,
        },
    }

    fn unindent(line: &str) -> (usize, &str) {
        let mut indentation_level = 0;
        loop {
            line = line.trim_start_matches(char::is_whitespace);
            line = match line.strip_prefix("-") {
                None => return (indentation_level, line),
                Some(line) => {
                    indentation_level += 1;
                    line
                },
            }
        }
    }

    pub fn parse<'a>(
        raw_lines: impl Iterator<Item = &'a str>,
    ) -> Result<Vec<ActionInvocation<'a>>, Error> {
        let mut raw_lines = raw_lines.enumerate().peekable();
        let mut action_invocations = Vec::new();
        for (line_index, raw_line) in raw_lines {
            let raw_line = raw_line.trim_start_matches(char::is_whitespace);
            match raw_line.strip_prefix("-") {
                None => {
                    let contents = match filler::parse(raw_line) {
                        Ok(contents) => contents,
                        Err(filler::Error {}) => unimplemented!(),
                    };
                    parsed_lines.push(ActionInvocation {
                        contents,
                        attached_invocations: Vec::new(),
                    })
                }
                Some(raw_line) => raw_lines.peek().and_then(|(line_index, raw_line)| {
                    let raw_line = raw_line.trim_start_matches(char::is_whitespace);
                    raw_line.strip_prefix("-")
                }),
            }
        }
        let mut action_invocations = Vec::new();
        let Some((mut line_index, mut previous_line)) = raw_lines.next() else {
            return Ok(action_invocations);
        };
        loop {
            let Some((line_index, current_line)) = match raw_lines.next() {
                Some(values) => values,
                None => return Ok(action_invocations),
            };
        }
        todo!()
    }
}

pub fn parse(program: &str) -> Result<Vec<ActionInvocation>, Error> {
    match lines::parse(program.split("\n")) {
        Ok(lines) => Ok(lines),
        Err(error) => Err(match error {
            lines::Error::UnexpectedIndentation {
                line_index,
                present_indentation_level: present_indentation,
            } => Error::UnexpectedIndentation {
                line_number: line_index + 1,
                present_indentation,
            },
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
