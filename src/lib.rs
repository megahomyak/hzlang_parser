use std::collections::HashSet;

use parco::{Input, Rest};

#[derive(PartialEq, Eq, Debug)]
pub struct ActionInvocation {
    pub name: Name,
    pub attached: Vec<ActionInvocation>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    Overindented {
        expected_indentations: HashSet<usize>,
        present_indentation: usize,
    },
    UnexpectedCharacterEscaped {
        character: char,
    },
    UnclosedQuote,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub line_index: usize,
    pub kind: ErrorKind,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Word(pub String);

#[derive(PartialEq, Eq, Debug)]
pub enum NamePart {
    Word(Word),
    Filler(Filler),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Name {
    pub contents: Vec<NamePart>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Dict {
    pub contents: Vec<(Filler, Filler)>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Filler {
    String(HzString),
    Name(Name),
    List(List),
    Dict(Dict),
}

#[derive(PartialEq, Eq, Debug)]
pub struct List {
    pub contents: Vec<Filler>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct RawHzStringPart(pub String);

#[derive(PartialEq, Eq, Debug)]
pub enum HzStringPart {
    Raw(RawHzStringPart),
    Name(Name),
}

#[derive(PartialEq, Eq, Debug)]
pub struct HzString {
    pub parts: Vec<HzStringPart>,
}

type ParsingResult<'a, T> = parco::Result<T, &'a str, ErrorKind>;

impl<T> From<ErrorKind> for ParsingResult<'_, T> {
    fn from(error: ErrorKind) -> Self {
        ParsingResult::Fatal(error)
    }
}

trait Container {
    fn shrink_to_fit(&mut self);
    fn is_empty(&self) -> bool;
}

impl<T> Container for Vec<T> {
    fn shrink_to_fit(&mut self) {
        self.shrink_to_fit();
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl Container for String {
    fn shrink_to_fit(&mut self) {
        self.shrink_to_fit();
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

fn fail_if_empty<C: Container>(c: ParsingResult<C>) -> ParsingResult<C> {
    c.and(|container, rest| {
        if container.is_empty() {
            ParsingResult::Err
        } else {
            ParsingResult::Ok(container, rest)
        }
    })
}

fn shrink<C: Container>(mut result: ParsingResult<C>) -> ParsingResult<C> {
    if let parco::Result::Ok(container, _rest) = &mut result {
        container.shrink_to_fit();
    }
    result
}

fn skip_whitespace(s: &str) -> &str {
    for (i, c) in s.char_indices() {
        if !c.is_whitespace() {
            return &s[i..];
        }
    }
    ""
}

enum StringParsingError {
    OpeningQuoteNotFound,
    Fatal(Error),
}

fn chop(rest: &str) -> Option<(char, &str)> {
    let chars = rest.chars();
    chars.next().map(|c| (c, chars.as_str()))
}

fn matching(rest: &str, ch: char) -> Option<&str> {
    chop(rest).and_then(|(c, rest)| if c == ch { Some(rest) } else { None })
}

fn parse_string(rest: &str) -> Result<Option<(String, &str)>, StringParsingError> {
    matching(rest).map_or_else(|| StringParsingError::OpeningQuoteNotFound, |rest| )
}

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

pub fn parse(program: &str) -> Result<Vec<ActionInvocation>, Error> {
    let mut program = program.lines().enumerate().peekable();
    let mut root = Vec::new();
    let mut levels: Vec<*mut Vec<ActionInvocation>> = Vec::new();
    while let Some((index, line)) = program.next() {
        let (level, unindented) = unindent(line);
        if level != 0 && index == 0 {
            return Err(Error {
                kind: ErrorKind::Overindented {
                    expected_indentations: HashSet::from([0]),
                    present_indentation: level,
                },
                line_index: index,
            });
        }
        let name = match parse_name_contents(unindented).map(|contents| Name { contents }) {
            ParsingResult::Ok(name, Rest(rest)) => match skip_whitespace(rest).take_one_part() {
                None => Ok(name),
                Some((c, _rest)) => match c {
                    ']' => Err(ErrorKind::UnexpectedClosingBracket),
                    ')' => Err(ErrorKind::UnexpectedClosingParen),
                    '}' => Err(ErrorKind::UnexpectedClosingBrace),
                    _ => unreachable!(),
                },
            },
            ParsingResult::Err => unreachable!(),
            ParsingResult::Fatal(error_kind) => Err(error_kind),
        };
        let name = match name {
            Err(error_kind) => {
                return Err(Error {
                    line_index: index,
                    kind: error_kind,
                })
            }
            Ok(name) => name,
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
                return Err(Error {
                    line_index: *next_index,
                    kind: ErrorKind::Overindented {
                        present_indentation: next_level,
                        expected_indentations,
                    },
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

    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn correct_indentation() {
        fn word(word: &str) -> NamePart {
            NamePart::Word(Word(word.to_owned()))
        }

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
            Err(Error {
                line_index: 0,
                kind: ErrorKind::Overindented {
                    expected_indentations: HashSet::from([0]),
                    present_indentation: 1
                }
            })
        );
    }

    #[test]
    fn incorrect_indentation_2() {
        assert_eq!(
            parse("a\n--b"),
            Err(Error {
                line_index: 1,
                kind: ErrorKind::Overindented {
                    expected_indentations: HashSet::from([0, 1]),
                    present_indentation: 2
                }
            })
        );
    }

    #[test]
    fn incorrect_indentation_3() {
        assert_eq!(
            parse("a\n-b\n---c"),
            Err(Error {
                line_index: 2,
                kind: ErrorKind::Overindented {
                    expected_indentations: HashSet::from([0, 1, 2]),
                    present_indentation: 3
                }
            })
        );
    }

    #[test]
    fn correct_string() {
        assert_eq!(
            parse(r#""abc{def"ghi"jkl}m\\n\"o\{""#),
            Ok(vec![line(
                vec![NamePart::Filler(Filler::String(HzString {
                    parts: vec![
                        HzStringPart::Raw(RawHzStringPart("abc".to_owned())),
                        HzStringPart::Name(Name {
                            contents: vec![
                                NamePart::Word(Word("def".to_owned())),
                                NamePart::Filler(Filler::String(HzString {
                                    parts: vec![HzStringPart::Raw(RawHzStringPart(
                                        "ghi".to_owned()
                                    ))]
                                })),
                                NamePart::Word(Word("jkl".to_owned())),
                            ]
                        }),
                        HzStringPart::Raw(RawHzStringPart("m\\n\"o{".to_owned())),
                    ]
                }))],
                vec![]
            )])
        );
    }

    #[test]
    fn incorrect_string_1() {
        assert_eq!(
            parse(r#"""#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnclosedQuote,
            })
        )
    }

    #[test]
    fn incorrect_string_2() {
        assert_eq!(
            parse(r#""\"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnclosedQuote,
            })
        )
    }

    #[test]
    fn incorrect_string_3() {
        assert_eq!(
            parse(r#""\a"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedCharacterEscaped { character: 'a' },
            })
        )
    }

    #[test]
    fn incorrect_name_filler_1() {
        assert_eq!(
            parse(r#"{"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::NameExpected
            })
        )
    }

    #[test]
    fn incorrect_name_filler_2() {
        assert_eq!(
            parse(r#"{}"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::NameExpected
            })
        )
    }

    #[test]
    fn correct_dict_1() {
        assert_eq!(
            parse(r#"["a": "b", "c": {d}]"#),
            Ok(vec![line(
                vec![NamePart::Filler(Filler::Dict(Dict {
                    contents: vec![
                        (
                            Filler::String(HzString {
                                parts: vec![HzStringPart::Raw(RawHzStringPart("a".to_owned()))]
                            }),
                            Filler::String(HzString {
                                parts: vec![HzStringPart::Raw(RawHzStringPart("b".to_owned()))]
                            })
                        ),
                        (
                            Filler::String(HzString {
                                parts: vec![HzStringPart::Raw(RawHzStringPart("c".to_owned()))]
                            }),
                            Filler::Name(Name {
                                contents: vec![NamePart::Word(Word("d".to_owned()))]
                            })
                        )
                    ]
                }))],
                vec![]
            )])
        )
    }

    #[test]
    fn correct_dict_2() {
        assert_eq!(
            parse(r#"[]"#),
            Ok(vec![line(
                vec![NamePart::Filler(Filler::Dict(Dict { contents: vec![] }))],
                vec![]
            )])
        )
    }

    #[test]
    fn incorrect_dict_1() {
        assert_eq!(
            parse(r#"["#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::ClosingBracketOrCommaExpectedInDict
            })
        )
    }

    #[test]
    fn incorrect_dict_2() {
        assert_eq!(
            parse(r#"["a""#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::ColonExpectedInDict
            })
        )
    }

    #[test]
    fn incorrect_dict_3() {
        assert_eq!(
            parse(r#"["a":"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::FillerExpectedAsDictValue
            })
        )
    }

    #[test]
    fn incorrect_dict_4() {
        assert_eq!(
            parse(r#"["a": "b""#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::ClosingBracketOrCommaExpectedInDict
            })
        )
    }

    #[test]
    fn incorrect_dict_5() {
        assert_eq!(
            parse(r#"["a": "b" "c""#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::ClosingBracketOrCommaExpectedInDict
            })
        )
    }

    #[test]
    fn incorrect_dict_6() {
        assert_eq!(
            parse(r#"["a": "b" /"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::ClosingBracketOrCommaExpectedInDict
            })
        )
    }

    #[test]
    fn unexpected_bracket_1() {
        assert_eq!(
            parse(r#"a]"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedClosingBracket
            })
        )
    }

    #[test]
    fn unexpected_bracket_2() {
        assert_eq!(
            parse(r#"]"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedClosingBracket
            })
        )
    }

    #[test]
    fn unexpected_brace_1() {
        assert_eq!(
            parse(r#"a}"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedClosingBrace
            })
        )
    }

    #[test]
    fn unexpected_brace_2() {
        assert_eq!(
            parse(r#"}"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedClosingBrace
            })
        )
    }

    #[test]
    fn unexpected_paren_1() {
        assert_eq!(
            parse(r#"a)"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedClosingParen
            })
        )
    }

    #[test]
    fn unexpected_paren_2() {
        assert_eq!(
            parse(r#")"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::UnexpectedClosingParen
            })
        )
    }

    #[test]
    fn empty_program() {
        assert_eq!(parse(r#""#), Ok(vec![]))
    }

    #[test]
    fn correct_list() {}

    #[test]
    fn empty_list() {
        assert_eq!(
            parse(r#"()"#),
            Ok(vec![line(
                vec![NamePart::Filler(Filler::List(List {
                    contents: Vec::new()
                }))],
                vec![]
            )])
        )
    }
}
