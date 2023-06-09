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
    UnclosedParen,
    UnclosedBrace,
    ColonExpectedInDict,
    FillerExpectedInList,
    FillerExpectedAsDictKey,
    FillerExpectedAsDictValue,
    ClosingBracketOrCommaExpectedInDict,
    NameExpected,
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

fn parse_raw_string_part(rest: &str) -> ParsingResult<RawHzStringPart> {
    fail_if_empty(shrink(
        parco::collect_repeating(rest, |rest| {
            parco::one_part(*rest).and(|c, Rest(rest)| match c {
                '{' | '"' => ParsingResult::Err,
                '\\' => parco::one_part(rest)
                    .or(|| ErrorKind::UnclosedQuote.into())
                    .and(|c, rest| match c {
                        '{' | '"' | '\\' => ParsingResult::Ok(c, rest),
                        _ => ErrorKind::UnexpectedCharacterEscaped { character: c }.into(),
                    }),
                _ => ParsingResult::Ok(c, Rest(rest)),
            })
        })
        .into(),
    ))
    .map(|raw_part: String| RawHzStringPart(raw_part))
}

fn parse_string_parts(rest: &str) -> ParsingResult<Vec<HzStringPart>> {
    shrink(
        parco::collect_repeating(rest, |rest| {
            parse_raw_string_part(rest)
                .map(|raw| HzStringPart::Raw(raw))
                .or(|| parse_braced_name(rest).map(|name| HzStringPart::Name(name)))
        })
        .into(),
    )
}

fn parse_string(rest: &str) -> ParsingResult<HzString> {
    parco::one_matching_part(rest, |c| *c == '"')
        .and(|_, Rest(rest)| parse_string_parts(rest).map(|parts| HzString { parts }))
        .and(|contents, Rest(rest)| {
            parco::one_matching_part(rest, |c| *c == '"')
                .or(|| ErrorKind::UnclosedQuote.into())
                .and(|_, rest| ParsingResult::Ok(contents, rest))
        })
}

fn parse_word(rest: &str) -> ParsingResult<Word> {
    fail_if_empty(shrink(
        parco::collect_repeating(rest, |rest| {
            parco::one_matching_part(*rest, |c| !("[]{}()\"".contains(*c) || c.is_whitespace()))
        })
        .into(),
    ))
    .map(|word: String| Word(word))
}

fn parse_braced_name(rest: &str) -> ParsingResult<Name> {
    parco::one_matching_part(rest, |c| *c == '{')
        .and(|_, Rest(rest)| parse_name(rest).or(|| ErrorKind::NameExpected.into()))
        .and(|name, Rest(rest)| {
            parco::one_matching_part(rest, |c| *c == '}')
                .and(|_, rest| ParsingResult::Ok(name, rest))
        })
}

fn parse_filler(rest: &str) -> ParsingResult<Filler> {
    parse_string(rest)
        .map(|string| Filler::String(string))
        .or(|| parse_braced_name(rest).map(|filler| Filler::Name(filler)))
        .or(|| parse_list(rest).map(|list| Filler::List(list)))
        .or(|| parse_dict(rest).map(|dict| Filler::Dict(dict)))
}

fn parse_dict_pair(rest: &str) -> ParsingResult<(Filler, Filler)> {
    parse_filler(skip_whitespace(rest))
        .or(|| ErrorKind::FillerExpectedAsDictKey.into())
        .and(|key, Rest(rest)| {
            parco::one_matching_part(skip_whitespace(rest), |c| *c == ':')
                .or(|| ErrorKind::ColonExpectedInDict.into())
                .and(|_, Rest(rest)| {
                    parse_filler(skip_whitespace(rest))
                        .or(|| ErrorKind::FillerExpectedAsDictValue.into())
                        .and(|value, rest| ParsingResult::Ok((key, value), rest))
                })
        })
}

fn parse_dict_contents(rest: &str) -> ParsingResult<Vec<(Filler, Filler)>> {
    shrink(
        ParsingResult::from(parco::collect_repeating(rest, |rest| {
            parse_dict_pair(rest).and(|pair, Rest(rest)| {
                parco::one_matching_part(skip_whitespace(rest), |c| *c == ',')
                    .and(|_, rest| ParsingResult::Ok(pair, rest))
            })
        }))
        .and(|mut pairs: Vec<_>, Rest(rest)| {
            parse_dict_pair(rest).map(|pair| {
                pairs.push(pair);
                pairs
            })
        }),
    )
    .or(|| ParsingResult::Ok(Vec::new(), Rest(rest)))
}

fn parse_dict(rest: &str) -> ParsingResult<Dict> {
    parco::one_matching_part(rest, |c| *c == '[')
        .and(|_, Rest(rest)| parse_dict_contents(rest).map(|contents| Dict { contents }))
        .and(|contents, Rest(rest)| {
            parco::one_matching_part(rest, |c| *c == ']')
                .or(|| ErrorKind::ClosingBracketOrCommaExpectedInDict.into())
                .and(|_, rest| ParsingResult::Ok(contents, rest))
        })
}

fn parse_name(rest: &str) -> ParsingResult<Name> {
    fail_if_empty(shrink(
        parco::collect_repeating(rest, |rest| {
            let rest = skip_whitespace(rest);
            parse_filler(rest)
                .map(|filler| NamePart::Filler(filler))
                .or(|| parse_word(rest).map(|word| NamePart::Word(word)))
        })
        .into(),
    ))
    .map(|contents| Name { contents })
}

fn parse_list(rest: &str) -> ParsingResult<List> {
    parco::one_matching_part(rest, |c| *c == '(').and(|_, Rest(rest)| {
        shrink(
            parco::collect_repeating(rest, |rest| {
                parse_filler(skip_whitespace(rest)).or(|| match rest.take_one_part() {
                    None => ErrorKind::UnclosedParen.into(),
                    Some((')', _rest)) => ParsingResult::Err,
                    Some(_) => ErrorKind::FillerExpectedInList.into(),
                })
            })
            .into(),
        )
        .map(|contents| List { contents })
    })
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
        let name = match parse_name(unindented) {
            ParsingResult::Ok(name, _rest) => name,
            ParsingResult::Err => Name {
                contents: Vec::new(),
            },
            ParsingResult::Fatal(error_kind) => {
                return Err(Error {
                    line_index: index,
                    kind: error_kind,
                })
            }
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

    fn word(word: &str) -> NamePart {
        NamePart::Word(Word(word.to_owned()))
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
    fn incorrect_name_filler() {
        assert_eq!(
            parse(r#"{"#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::NameExpected
            })
        )
    }

    #[test]
    fn correct_dict() {
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
    fn incorrect_dict_1() {
        assert_eq!(
            parse(r#"["#),
            Err(Error {
                line_index: 0,
                kind: ErrorKind::FillerExpectedAsDictKey
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
}
