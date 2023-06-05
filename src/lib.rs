use std::collections::HashSet;

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
pub struct ActionInvocation<'a> {
    pub contents: Filler<'a>,
    pub attached: Vec<ActionInvocation<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    OverIndented {
        line_index: usize,
        expected_indentations: HashSet<usize>,
        present_indentation: usize,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub enum Word<'a> {
    Raw(&'a str),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Filler<'a> {
    contents: Vec<Word<'a>>,
}

fn parse_filler(filler: &str) -> Filler {
    let mut contents = Vec::new();
    for word in filler.split_whitespace() {
        contents.push(Word::Raw(word));
    }
    Filler { contents }
}

pub fn parse(program: &str) -> Result<Vec<ActionInvocation<'_>>, Error> {
    let mut program = program.lines().enumerate().peekable();
    let mut root = Vec::new();
    let mut levels: Vec<*mut Vec<ActionInvocation>> = Vec::new();
    while let Some((index, line)) = program.next() {
        let (level, unindented) = unindent(line);
        if level != 0 && index == 0 {
            return Err(Error::OverIndented {
                line_index: index,
                expected_indentations: HashSet::from([0]),
                present_indentation: level,
            });
        }
        let line = ActionInvocation {
            contents: parse_filler(unindented),
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
                let mut expected_indentations = HashSet::from([level, level + 1]);
                if level != 0 {
                    expected_indentations.insert(level - 1);
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
mod tests {
    use super::*;

    fn line<'a>(
        contents: Vec<Word<'a>>,
        attached: Vec<ActionInvocation<'a>>,
    ) -> ActionInvocation<'a> {
        ActionInvocation {
            attached,
            contents: Filler { contents },
        }
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn correct_indentation() {
        assert_eq!(
            parse("a-d\n-b\n-  -   c\n  --d\ne"),
            Ok(vec![
                line(
                    Vec::from([Word::Raw("a-d")]),
                    vec![
                        line(Vec::from([Word::Raw("b")]), vec![
                             line(Vec::from([Word::Raw("c")]), vec![]),
                             line(Vec::from([Word::Raw("d")]), vec![])
                        ]),
                    ],
                ),
                line(Vec::from([Word::Raw("e")]), vec![]),
            ])
        );
    }

    #[test]
    fn incorrect_indentation_1() {
        assert_eq!(
            parse("-a"),
            Err(Error::OverIndented {
                line_index: 0,
                expected_indentations: HashSet::from([0]),
                present_indentation: 1
            })
        );
    }

    #[test]
    fn incorrect_indentation_2() {
        assert_eq!(
            parse("a\n--b"),
            Err(Error::OverIndented {
                line_index: 1,
                expected_indentations: HashSet::from([0, 1]),
                present_indentation: 2
            })
        );
    }

    #[test]
    fn incorrect_indentation_3() {
        assert_eq!(
            parse("a\n-b\n---c"),
            Err(Error::OverIndented {
                line_index: 2,
                expected_indentations: HashSet::from([0, 1, 2]),
                present_indentation: 3
            })
        );
    }
}
