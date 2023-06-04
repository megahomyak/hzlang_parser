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
        struct Level<'a> {
            indentation: usize,
            lines: Filler<'a>,
        }

        let mut root = Vec::new();
        let mut current_level = &mut root;
        for (line_index, raw_line) in raw_lines.enumerate() {
            let (indentation_level, raw_line) = unindent(raw_line);
            if indentation_level > 
        }
        return Ok(root);
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
