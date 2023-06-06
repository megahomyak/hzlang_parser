use std::collections::HashSet;

use parco::Rest;

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
pub struct ActionInvocation {
    pub name: Name,
    pub attached: Vec<ActionInvocation>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    OverIndented {
        line_index: usize,
        expected_indentations: HashSet<usize>,
        present_indentation: usize,
    },
    UnclosedQuote,
    UnexpectedCharacterEscaped,
    WordInsideList,
}

#[derive(PartialEq, Eq, Debug)]
pub enum NamePart {
    Word(String),
    String(HzString),
    Name(Name),
    List(List),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Name {
    pub contents: Vec<NamePart>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ListPart {
    String(HzString),
    Name(Name),
    List(List),
}

#[derive(PartialEq, Eq, Debug)]
pub struct List {
    pub contents: Vec<ListPart>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum HzStringPart {
    Raw(String),
    Name(Name),
}

#[derive(PartialEq, Eq, Debug)]
pub struct HzString {
    pub parts: Vec<HzStringPart>,
}

type ParsingResult<'a, T> = parco::Result<T, &'a str, Error>;

fn skip_whitespace(s: &str) -> &str {
    for (i, c) in s.char_indices() {
        if !c.is_whitespace() {
            return &s[i..];
        }
    }
    ""
}

fn parse_string(rest: &str) -> ParsingResult<HzString> {
    let mut rest = rest.chars();
    let Some('"') = rest.next() else {
        return ParsingResult::Err;
    };
    let mut contents = Vec::new();
    let mut raw = String::new();
    while let Some(c) = rest.next() {
        match c {
            '\\' => match rest.next() {
                None => return ParsingResult::Fatal(Error::UnclosedQuote),
                Some(c @ ('\\' | '"')) => raw.push(c),
                Some(_) => return ParsingResult::Fatal(Error::UnexpectedCharacterEscaped),
            },
            '"' => {
                contents.shrink_to_fit();
                return ParsingResult::Ok((raw, Rest(rest.as_str())));
            }
            '{' => {

            }
            _ => raw.push(c),
        }
    }
    ParsingResult::Fatal(Error::UnclosedQuote)
}

fn parse_word(rest: &str) -> ParsingResult<String> {
    for (i, c) in rest.char_indices() {
        if "[]{}()\"".contains(c) || c.is_whitespace() {
            let word = &rest[..i];
            return if word.is_empty() {
                ParsingResult::Err
            } else {
                ParsingResult::Ok((word.to_owned(), Rest(&rest[i..])))
            };
        }
    }
    ParsingResult::Ok((rest.to_owned(), Rest("")))
}

fn parse_braced_name(rest: &str) -> ParsingResult<Name> {
    todo!()
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn parse_name_part(rest: &str) -> ParsingResult<NamePart> {
    rest = skip_whitespace(rest);
    parse_word(rest).map(|word| NamePart::Word(word))
        .or(|| parse_string(rest).map(|string| NamePart::String(string)))
        .or(|| parse_braced_name(rest).map(|filler| NamePart::Name(filler)))
        .or(|| parse_list(rest).map(|list| NamePart::List(list)))
}

fn parse_name(rest: &str) -> ParsingResult<Name> {
    todo!()
}

fn parse_list(rest: &str) -> ParsingResult<List> {
    let mut contents = Vec::new();
    let rest = {
        let mut chars = rest.chars();
        let Some('(') = chars.next() else {
            return ParsingResult::Err;
        };
        chars.as_str()
    };
    loop {
        rest = skip_whitespace(rest);
        match parse_name_part(rest) {
            ParsingResult::Ok((name_part, rest)) => match name_part {},
            ParsingResult::Err => 
        }
    }
}

pub fn parse(program: &str) -> Result<Vec<ActionInvocation>, Error> {
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
        let name = match parse_name(unindented) {
            ParsingResult::Ok((name, _rest)) => name,
            ParsingResult::Err => Name {
                contents: Vec::new(),
            },
            ParsingResult::Fatal(error) => return Err(error),
        };
        let line = ActionInvocation {
            name,
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

    fn line(contents: Vec<NamePart>, attached: Vec<ActionInvocation>) -> ActionInvocation {
        ActionInvocation {
            attached,
            name: Name { contents },
        }
    }

    fn word(word: &str) -> NamePart {
        NamePart::Word(word.to_owned())
    }

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn correct_indentation() {
        assert_eq!(
            parse("a-d\n-b\n-  -   c\n  --d\ne"),
            Ok(vec![
                line(
                    Vec::from([word("a-d")]),
                    vec![
                        line(Vec::from([word("b")]), vec![
                             line(Vec::from([word("c")]), vec![]),
                             line(Vec::from([word("d")]), vec![])
                        ]),
                    ],
                ),
                line(Vec::from([word("e")]), vec![]),
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

    #[test]
    fn correct_string() {}

    #[test]
    fn incorrect_string_1() {}

    #[test]
    fn incorrect_string_2() {}

    #[test]
    fn incorrect_string_3() {}
}
