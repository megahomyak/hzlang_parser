fn unindent(mut line: &str) -> (usize, &str) {
    let mut level = 0;
    loop {
        line = line.trim_start_matches(char::is_whitespace);
        line = match line.strip_prefix("-") {
            Some(line) => {
                level += 1;
                line
            }
            None => break (level, line),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Line<'a> {
    pub contents: &'a str,
    pub attached: Vec<Line<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    OverIndented {
        line_index: usize,
        expected_indentations: Vec<usize>,
        present_indentation: usize,
    },
}

pub fn parse(program: &str) -> Result<Vec<Line<'_>>, Error> {
    let mut program = program.lines().enumerate().peekable();
    let mut root = Vec::new();
    let mut levels: Vec<*mut Vec<Line>> = Vec::new();
    while let Some((index, line)) = program.next() {
        let (level, unindented) = unindent(line);
        if level != 0 && index == 0 {
            return Err(Error::OverIndented {
                line_index: index,
                expected_indentations: vec![0],
                present_indentation: level,
            });
        }
        let line = Line {
            contents: unindented,
            attached: Vec::new(),
        };
        let line = match levels.iter_mut().rev().next() {
            Some(level) => {
                let level: &mut _ = unsafe { &mut **level };
                level.push(line);
                level.last_mut().unwrap()
            }
            None => {
                root.push(line);
                root.last_mut().unwrap()
            }
        };
        if let Some((next_index, next_line)) = program.peek() {
            let (next_level, _next_unindented) = unindent(next_line);
            if next_level == level + 1 {
                levels.push((&mut line.attached) as *mut _);
            } else if next_level > level {
                let mut expected_indentations = vec![level, level + 1];
                if level != 0 {
                    expected_indentations.push(level - 1);
                }
                return Err(Error::OverIndented {
                    line_index: *next_index,
                    present_indentation: next_level,
                    expected_indentations,
                });
            } else if next_level != level {
                let rollback = level - next_level;
                for _ in 0..rollback {
                    levels.pop().unwrap();
                }
            }
        }
    }
    Ok(root)
}

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
mod tests {
    use super::*;

    fn line<'a>(contents: &'a str, attached: Vec<Line<'a>>) -> Line<'a> {
        Line { attached, contents }
    }

    #[test]
    fn parsing_a_correct_program() {
        assert_eq!(
            parse("a-d\n-b\n-  -   c\n  --d\ne"),
            Ok(vec![
                line(
                    "a-d",
                    vec![
                        line("b", vec![
                             line("c", vec![]),
                             line("d", vec![])
                        ]),
                    ],
                ),
                line("e", vec![]),
            ])
        );
    }

    #[test]
    fn parsing_an_incorrect_program() {
        assert_eq!(
            parse("-a"),
            Err(Error::OverIndented {
                line_index: 0,
                expected_indentations: vec![0],
                present_indentation: 1
            })
        );
    }

    #[test]
    fn parsing_an_incorrect_program_2() {
        assert_eq!(
            parse("a\n--b"),
            Err(Error::OverIndented {
                line_index: 1,
                expected_indentations: vec![0, 1],
                present_indentation: 2
            })
        );
    }
}
