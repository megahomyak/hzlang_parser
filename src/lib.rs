pub enum Error {
    UnexpectedIndentation {
        line_number: usize,
        present_indentation: usize,
        expected_indentation_levels: Vec<usize>,
    },
}

pub type Range = std::ops::Range<usize>;

mod filler {
    use crate::{Filler, Word};

    pub struct Error {}

    pub fn parse(words: &str) -> Result<Filler, Error> {
        Ok(Filler {
            words: words
                .split_whitespace()
                .map(|word| Word::Raw(word))
                .collect(),
        })
    }
}

mod line {
    use crate::{filler, Filler};

    pub struct Line<'a> {
        unindented_line: Filler<'a>,
        indentation_level: usize,
    }

    pub struct Error {}

    pub fn parse(raw_line: &str) -> Result<Line, Error> {
        let mut indentation_level = 0;
        let unindented_line = loop {
            raw_line = raw_line.trim_start_matches(char::is_whitespace);
            raw_line = match raw_line.strip_prefix("-") {
                None => break raw_line,
                Some(raw_line) => {
                    indentation_level += 1;
                    raw_line
                }
            }
        };
        let unindented_line = match filler::parse(unindented_line) {
            Ok(filler) => filler,
            Err(error) => {return Err(Error {})}
        };

        Ok(Line {
            indentation_level,
            unindented_line,
        })
    }
}

mod lines {
    use crate::{ActionInvocation, Range};

    pub enum Error {
        UnexpectedIndentation {
            line_index: usize,
            present_indentation_level: usize,
            expected_indentation_levels_range: Range,
        },
    }

    pub fn parse<'a>(
        raw_lines: impl Iterator<Item = &'a str>,
        current_indentation_level: usize,
    ) -> Result<Vec<ActionInvocation<'a>>, Error> {
        let mut parsed_lines = Vec::new();
        let mut previous_indentation_level = None;
        for (line_index, mut raw_line) in raw_lines.enumerate() {
            let mut indentation_level = 0;
            let unindented_line = loop {
                raw_line = raw_line.trim_start_matches(char::is_whitespace);
                raw_line = match raw_line.strip_prefix("-") {
                    None => break raw_line,
                    Some(raw_line) => {
                        indentation_level += 1;
                        raw_line
                    }
                }
            };

            match previous_indentation_level {
                None => {
                    if indentation_level != 0 {
                        return Err(Error::UnexpectedIndentation {
                            line_index,
                            present_indentation_level: indentation_level,
                            expected_indentation_levels_range: Range {},
                        });
                    }
                }
                Some(previous_indentation_level) => {}
            }

            previous_indentation_level = Some(indentation_level);
        }

        todo!()
    }
}

pub struct Filler<'a> {
    words: Vec<Word<'a>>,
}

pub enum Word<'a> {
    Raw(&'a str),
}

pub struct ActionInvocation<'a> {
    contents: Vec<Word<'a>>,
    attached_invocations: Vec<ActionInvocation<'a>>,
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
