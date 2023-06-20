#[derive(PartialEq, Eq, Debug)]
pub struct Comment(pub String);

#[derive(PartialEq, Eq, Debug)]
pub struct Line {
    pub contents: LineContents,
    pub attached: Vec<Line>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct LineContents {
    action_invocation: Option<ActionInvocation>,
    comment: Option<Comment>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct ActionInvocation {
    pub name: Name,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Program {
    pub contents: Vec<Line>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    FirstLineIndented {
        present_indentation: usize,
    },
    OverindentedLine {
        max_allowed_indentation: usize,
        present_indentation: usize,
    },
    IndentationLevelIncreasedAfterEmptyLine {
        max_allowed_indentation: usize,
        present_indentation: usize,
    },
    UnclosedStringQuote,
    UnclosedBracedNameBrace,
    NameExpectedInBraces,
    FillerExpectedInList,
    UnclosedDictPairParen,
    UnclosedListParen,
    KeyFillerExpectedInDictPair,
    ValueFillerExpectedInDictPair,
    UnclosedDictBracket,
    DictPairExpectedInDict,
    UnexpectedClosingBracketInLine,
    UnexpectedClosingParenInLine,
    UnexpectedClosingBraceInLine,
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
    pub parts: Vec<NamePart>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Dict {
    pub pairs: Vec<DictPair>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct DictPair {
    pub key: Filler,
    pub value: Filler,
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
    pub items: Vec<Filler>,
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

trait Shrinkable {
    fn shrink_to_fit(&mut self);
}

impl<T> Shrinkable for Vec<T> {
    fn shrink_to_fit(&mut self) {
        self.shrink_to_fit();
    }
}

impl Shrinkable for String {
    fn shrink_to_fit(&mut self) {
        self.shrink_to_fit();
    }
}

fn shrink<C: Shrinkable>(mut container: C) -> C {
    container.shrink_to_fit();
    container
}

fn skip_whitespace(s: &str) -> &str {
    for (i, c) in s.char_indices() {
        if !c.is_whitespace() {
            return &s[i..];
        }
    }
    ""
}

mod string {
    use super::*;

    mod raw_part {
        use super::*;

        fn parse_char(rest: &str) -> ParsingResult<char> {
            parco::one_matching_part(rest, |c| !['"', '{'].contains(c))
        }

        pub fn parse(rest: &str) -> ParsingResult<RawHzStringPart> {
            parse_char(rest)
                .and(|c, rest| {
                    parco::collect_repeating(String::from(c), rest, |rest| parse_char(rest))
                })
                .map(|part| RawHzStringPart(shrink(part)))
        }
    }

    pub fn parse(rest: &str) -> ParsingResult<HzString> {
        parco::one_matching_part(rest, |c| *c == '"')
            .and(|_, rest| {
                parco::collect_repeating(Vec::new(), rest, |rest| {
                    raw_part::parse(rest)
                        .map(|raw_part| HzStringPart::Raw(raw_part))
                        .or(|| braced_name::parse(rest).map(|name| HzStringPart::Name(name)))
                })
            })
            .and(|parts, rest| {
                parco::one_matching_part(rest, |c| *c == '"')
                    .or(|| ErrorKind::UnclosedStringQuote.into())
                    .and(|_, rest| {
                        ParsingResult::Ok(
                            HzString {
                                parts: shrink(parts),
                            },
                            rest,
                        )
                    })
            })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn empty_string() {
            assert_eq!(
                parse(r#""" something else"#),
                ParsingResult::Ok(HzString { parts: vec![] }, " something else")
            );
        }

        #[test]
        fn unclosed_quote() {
            assert_eq!(
                parse(r#""text"#),
                ParsingResult::Fatal(ErrorKind::UnclosedStringQuote)
            );
        }

        #[test]
        fn unclosed_quote_2() {
            assert_eq!(
                parse(r#"""#),
                ParsingResult::Fatal(ErrorKind::UnclosedStringQuote)
            );
        }

        #[test]
        fn correct_string() {
            assert_eq!(
                parse(r#"" a {b "c" {d}} e " something else"#),
                ParsingResult::Ok(
                    HzString {
                        parts: vec![
                            HzStringPart::Raw(RawHzStringPart(" a ".to_owned())),
                            HzStringPart::Name(Name {
                                parts: vec![
                                    NamePart::Word(Word("b".to_owned())),
                                    NamePart::Filler(Filler::String(HzString {
                                        parts: vec![HzStringPart::Raw(RawHzStringPart(
                                            "c".to_owned()
                                        ))]
                                    })),
                                    NamePart::Filler(Filler::Name(Name {
                                        parts: vec![NamePart::Word(Word("d".to_owned()))]
                                    }))
                                ]
                            }),
                            HzStringPart::Raw(RawHzStringPart(" e ".to_owned()))
                        ]
                    },
                    " something else"
                )
            );
        }
    }
}

mod word {
    use super::*;

    fn parse_char(rest: &str) -> ParsingResult<char> {
        parco::one_matching_part(rest, |c| !("()[]{}\"|".contains(*c) || c.is_whitespace()))
    }

    pub fn parse(rest: &str) -> ParsingResult<Word> {
        parse_char(rest)
            .and(|c, rest| parco::collect_repeating(String::from(c), rest, |rest| parse_char(rest)))
            .map(|raw_word| Word(shrink(raw_word)))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn correct_word() {
            assert_eq!(
                parse("a something else"),
                ParsingResult::Ok(Word("a".to_owned()), " something else")
            );
        }

        #[test]
        fn unexpected_quote() {
            assert_eq!(parse("\""), ParsingResult::Err,);
        }

        #[test]
        fn a_word_and_a_comment() {
            assert_eq!(
                parse("blah|abc"),
                ParsingResult::Ok(Word("blah".to_owned()), "|abc")
            );
        }

        #[test]
        fn just_a_comment() {
            assert_eq!(parse("|abc"), ParsingResult::Err,);
        }
    }
}

mod name {
    use super::*;

    mod part {
        use super::*;

        pub fn parse(rest: &str) -> ParsingResult<NamePart> {
            filler::parse(rest)
                .map(|filler| NamePart::Filler(filler))
                .or(|| word::parse(rest).map(|word| NamePart::Word(word)))
        }
    }

    pub fn parse(rest: &str) -> ParsingResult<Name> {
        part::parse(rest)
            .and(|name_part, rest| {
                parco::collect_repeating(Vec::from([name_part]), rest, |rest| {
                    part::parse(skip_whitespace(rest))
                })
            })
            .map(|parts| Name {
                parts: shrink(parts),
            })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn unexpected_closing_bracket() {
            assert_eq!(parse("]"), ParsingResult::Err);
        }

        #[test]
        fn just_a_list() {
            assert_eq!(
                parse("()"),
                ParsingResult::Ok(
                    Name {
                        parts: vec![NamePart::Filler(Filler::List(List { items: vec![] }))]
                    },
                    ""
                )
            );
        }

        #[test]
        fn correct_name() {
            assert_eq!(
                parse("a ] something else"),
                ParsingResult::Ok(
                    Name {
                        parts: vec![NamePart::Word(Word("a".to_owned()))]
                    },
                    " ] something else"
                )
            );
        }

        #[test]
        fn correct_name_2() {
            assert_eq!(
                parse("a b] something else"),
                ParsingResult::Ok(
                    Name {
                        parts: vec![
                            NamePart::Word(Word("a".to_owned())),
                            NamePart::Word(Word("b".to_owned()))
                        ]
                    },
                    "] something else"
                )
            );
        }
    }
}

mod braced_name {
    use super::*;

    pub fn parse(rest: &str) -> ParsingResult<Name> {
        parco::one_matching_part(rest, |c| *c == '{')
            .and(|_, rest| name::parse(rest).or(|| ErrorKind::NameExpectedInBraces.into()))
            .and(|name, rest| {
                parco::one_matching_part(rest, |c| *c == '}')
                    .or(|| ErrorKind::UnclosedBracedNameBrace.into())
                    .and(|_, rest| ParsingResult::Ok(name, rest))
            })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn plain_name() {
            assert_eq!(parse("a"), ParsingResult::Err);
        }

        #[test]
        fn correct_braced_name() {
            assert_eq!(
                parse("{a} something else"),
                ParsingResult::Ok(
                    Name {
                        parts: vec![NamePart::Word(Word("a".to_owned()))]
                    },
                    " something else"
                )
            );
        }

        #[test]
        fn closing_brace_missing() {
            assert_eq!(
                parse("{a "),
                ParsingResult::Fatal(ErrorKind::UnclosedBracedNameBrace.into())
            );
        }

        #[test]
        fn closing_brace_missing_2() {
            assert_eq!(
                parse("{a"),
                ParsingResult::Fatal(ErrorKind::UnclosedBracedNameBrace.into())
            );
        }

        #[test]
        fn nothing_in_braces() {
            assert_eq!(
                parse("{ }"),
                ParsingResult::Fatal(ErrorKind::NameExpectedInBraces.into())
            );
        }

        #[test]
        fn nothing_in_braces_2() {
            assert_eq!(
                parse("{}"),
                ParsingResult::Fatal(ErrorKind::NameExpectedInBraces.into())
            );
        }

        #[test]
        fn nothing_in_braces_3() {
            assert_eq!(
                parse("{"),
                ParsingResult::Fatal(ErrorKind::NameExpectedInBraces.into())
            );
        }
    }
}

mod filler {
    use super::*;

    pub fn parse(rest: &str) -> ParsingResult<Filler> {
        braced_name::parse(rest)
            .map(|name| Filler::Name(name))
            .or(|| string::parse(rest).map(|string| Filler::String(string)))
            .or(|| list::parse(rest).map(|list| Filler::List(list)))
            .or(|| dict::parse(rest).map(|dict| Filler::Dict(dict)))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn unrelated_word() {
            assert_eq!(parse("a"), ParsingResult::Err);
        }

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn string() {
            assert_eq!(
                parse(r#""" something else"#),
                ParsingResult::Ok(
                    Filler::String(HzString { parts: vec![] }),
                    " something else"
                )
            );
        }

        #[test]
        fn list() {
            assert_eq!(
                parse(r#"() something else"#),
                ParsingResult::Ok(Filler::List(List { items: vec![] }), " something else")
            );
        }

        #[test]
        fn dict() {
            assert_eq!(
                parse(r#"[] something else"#),
                ParsingResult::Ok(Filler::Dict(Dict { pairs: vec![] }), " something else")
            );
        }

        #[test]
        fn braced_name() {
            assert_eq!(
                parse(r#"{a} something else"#),
                ParsingResult::Ok(
                    Filler::Name(Name {
                        parts: vec![NamePart::Word(Word("a".to_owned()))]
                    }),
                    " something else"
                )
            );
        }
    }
}

mod list {
    use super::*;

    pub fn parse(rest: &str) -> ParsingResult<List> {
        parco::one_matching_part(rest, |c| *c == '(')
            .and(|_, rest| {
                parco::collect_repeating(Vec::new(), rest, |rest| {
                    let rest = skip_whitespace(rest);
                    filler::parse(rest).or(|| {
                        parco::one_part(rest)
                            .or(|| ErrorKind::UnclosedListParen.into())
                            .and(|c, _rest| match c {
                                ')' => ParsingResult::Err,
                                _ => ErrorKind::FillerExpectedInList.into(),
                            })
                    })
                })
            })
            .and(|items, rest| {
                parco::one_matching_part(rest, |c| *c == ')')
                    .or(|| unreachable!())
                    .and(|_, rest| {
                        ParsingResult::Ok(
                            List {
                                items: shrink(items),
                            },
                            rest,
                        )
                    })
            })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn string_filler(part: &str) -> Filler {
            Filler::String(HzString {
                parts: vec![HzStringPart::Raw(RawHzStringPart(part.to_owned()))],
            })
        }

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn empty_list() {
            assert_eq!(
                parse("() something else"),
                ParsingResult::Ok(List { items: vec![] }, " something else")
            );
        }

        #[test]
        fn unrelated_word() {
            assert_eq!(parse("a"), ParsingResult::Err);
        }

        #[test]
        fn unfinished_list() {
            assert_eq!(
                parse("("),
                ParsingResult::Fatal(ErrorKind::UnclosedListParen.into())
            );
        }

        #[test]
        fn word_in_list() {
            assert_eq!(
                parse("(a"),
                ParsingResult::Fatal(ErrorKind::FillerExpectedInList.into())
            );
        }

        #[test]
        fn word_in_list_2() {
            assert_eq!(
                parse(r#"("a" a"#),
                ParsingResult::Fatal(ErrorKind::FillerExpectedInList.into())
            );
        }

        #[test]
        fn word_in_list_3() {
            assert_eq!(
                parse(r#"("a" a"#),
                ParsingResult::Fatal(ErrorKind::FillerExpectedInList.into())
            );
        }

        #[test]
        fn word_in_list_4() {
            assert_eq!(
                parse(r#"("a""#),
                ParsingResult::Fatal(ErrorKind::UnclosedListParen.into())
            );
        }

        #[test]
        fn correct_list() {
            assert_eq!(
                parse(r#"("a" "b") something else"#),
                ParsingResult::Ok(
                    List {
                        items: vec![string_filler("a"), string_filler("b")]
                    },
                    " something else"
                )
            );
        }
    }
}

mod dict {
    use super::*;

    fn parse_pair(rest: &str) -> ParsingResult<DictPair> {
        parco::one_matching_part(rest, |c| *c == '(')
            .and(|_, rest| {
                filler::parse(skip_whitespace(rest))
                    .or(|| ErrorKind::KeyFillerExpectedInDictPair.into())
            })
            .and(|key, rest| {
                filler::parse(skip_whitespace(rest))
                    .or(|| ErrorKind::ValueFillerExpectedInDictPair.into())
                    .and(|value, rest| {
                        parco::one_matching_part(rest, |c| *c == ')')
                            .or(|| ErrorKind::UnclosedDictPairParen.into())
                            .and(|_, rest| ParsingResult::Ok(DictPair { key, value }, rest))
                    })
            })
    }

    pub fn parse(rest: &str) -> ParsingResult<Dict> {
        parco::one_matching_part(rest, |c| *c == '[')
            .and(|_, rest| {
                parco::collect_repeating(Vec::new(), rest, |rest| {
                    let rest = skip_whitespace(rest);
                    parse_pair(rest).or(|| {
                        parco::one_part(rest)
                            .or(|| ErrorKind::UnclosedDictBracket.into())
                            .and(|c, _rest| match c {
                                ']' => ParsingResult::Err,
                                _ => ErrorKind::DictPairExpectedInDict.into(),
                            })
                    })
                })
            })
            .and(|pairs, rest| {
                parco::one_matching_part(rest, |c| *c == ']')
                    .or(|| unreachable!())
                    .and(|_, rest| {
                        ParsingResult::Ok(
                            Dict {
                                pairs: shrink(pairs),
                            },
                            rest,
                        )
                    })
            })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn string_filler(part: &str) -> Filler {
            Filler::String(HzString {
                parts: vec![HzStringPart::Raw(RawHzStringPart(part.to_owned()))],
            })
        }

        #[test]
        fn nothing() {
            assert_eq!(parse(""), ParsingResult::Err);
        }

        #[test]
        fn empty_dict() {
            assert_eq!(
                parse("[] something else"),
                ParsingResult::Ok(Dict { pairs: vec![] }, " something else")
            );
        }

        #[test]
        fn unrelated_word() {
            assert_eq!(parse("a"), ParsingResult::Err);
        }

        #[test]
        fn correct_dict() {
            assert_eq!(
                parse(r#"[("a" "b") ("c" "d")] something else"#),
                ParsingResult::Ok(
                    Dict {
                        pairs: vec![
                            DictPair {
                                key: string_filler("a"),
                                value: string_filler("b")
                            },
                            DictPair {
                                key: string_filler("c"),
                                value: string_filler("d")
                            },
                        ]
                    },
                    " something else"
                )
            )
        }

        #[test]
        fn unfinished_dict() {
            assert_eq!(
                parse("["),
                ParsingResult::Fatal(ErrorKind::UnclosedDictBracket.into())
            );
        }

        #[test]
        fn unfinished_dict_2() {
            assert_eq!(
                parse("[("),
                ParsingResult::Fatal(ErrorKind::KeyFillerExpectedInDictPair.into())
            );
        }

        #[test]
        fn unfinished_dict_3() {
            assert_eq!(
                parse(r#"[("a" "#),
                ParsingResult::Fatal(ErrorKind::ValueFillerExpectedInDictPair.into())
            );
        }

        #[test]
        fn unfinished_dict_4() {
            assert_eq!(
                parse(r#"[("a" "b""#),
                ParsingResult::Fatal(ErrorKind::UnclosedDictPairParen.into())
            );
        }

        #[test]
        fn unfinished_dict_5() {
            assert_eq!(
                parse(r#"[("a" "b")"#),
                ParsingResult::Fatal(ErrorKind::UnclosedDictBracket.into())
            );
        }

        #[test]
        fn word_in_unfinished_dict() {
            assert_eq!(
                parse(r#"[("a" "b" a"#),
                ParsingResult::Fatal(ErrorKind::UnclosedDictPairParen.into())
            );
        }

        #[test]
        fn word_in_unfinished_dict_2() {
            assert_eq!(
                parse(r#"[("a" "b") a"#),
                ParsingResult::Fatal(ErrorKind::DictPairExpectedInDict.into())
            );
        }

        #[test]
        fn unexpected_word() {
            assert_eq!(
                parse("[a"),
                ParsingResult::Fatal(ErrorKind::DictPairExpectedInDict.into())
            );
        }

        #[test]
        fn unexpected_word_2() {
            assert_eq!(
                parse("[(a"),
                ParsingResult::Fatal(ErrorKind::KeyFillerExpectedInDictPair.into())
            );
        }

        #[test]
        fn unexpected_word_3() {
            assert_eq!(
                parse(r#"[("a" a"#),
                ParsingResult::Fatal(ErrorKind::ValueFillerExpectedInDictPair.into())
            );
        }
    }
}

mod line_contents {
    use super::*;

    fn map_unexpected_character(c: char, rest: &str) -> Result<Comment, ErrorKind> {
        match c {
            '}' => Err(ErrorKind::UnexpectedClosingBraceInLine),
            ']' => Err(ErrorKind::UnexpectedClosingBracketInLine),
            ')' => Err(ErrorKind::UnexpectedClosingParenInLine),
            '|' => Ok(Comment(rest.to_owned())),
            _ => unreachable!(),
        }
    }

    pub fn parse(unindented: &str) -> Result<LineContents, ErrorKind> {
        if unindented.is_empty() {
            Ok(LineContents {
                comment: None,
                action_invocation: None,
            })
        } else {
            match name::parse(unindented) {
                ParsingResult::Ok(name, rest) => {
                    let rest = skip_whitespace(rest);
                    let action_invocation = ActionInvocation { name };
                    match parco::Input::take_one_part(&rest) {
                        None => Ok(LineContents {
                            action_invocation: Some(action_invocation),
                            comment: None,
                        }),
                        Some((unexpected_character, rest)) => {
                            map_unexpected_character(unexpected_character, rest).map(|comment| {
                                LineContents {
                                    comment: Some(comment),
                                    action_invocation: Some(action_invocation),
                                }
                            })
                        }
                    }
                }
                ParsingResult::Err => match parco::Input::take_one_part(&unindented) {
                    None => unreachable!(),
                    Some((unexpected_character, rest)) => {
                        map_unexpected_character(unexpected_character, rest).map(|comment| {
                            LineContents {
                                action_invocation: None,
                                comment: Some(comment),
                            }
                        })
                    }
                },
                ParsingResult::Fatal(error) => Err(error),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn unexpected_brace() {
            assert_eq!(parse("a}"), Err(ErrorKind::UnexpectedClosingBraceInLine));
        }

        #[test]
        fn unexpected_brace_2() {
            assert_eq!(parse("}"), Err(ErrorKind::UnexpectedClosingBraceInLine));
        }

        #[test]
        fn unexpected_paren() {
            assert_eq!(parse("a)"), Err(ErrorKind::UnexpectedClosingParenInLine));
        }

        #[test]
        fn unexpected_paren_2() {
            assert_eq!(parse(")"), Err(ErrorKind::UnexpectedClosingParenInLine));
        }

        #[test]
        fn unexpected_bracket() {
            assert_eq!(parse("a]"), Err(ErrorKind::UnexpectedClosingBracketInLine));
        }

        #[test]
        fn unexpected_bracket_2() {
            assert_eq!(parse("]"), Err(ErrorKind::UnexpectedClosingBracketInLine));
        }

        #[test]
        fn empty_line() {
            assert_eq!(
                parse(""),
                Ok(LineContents {
                    action_invocation: None,
                    comment: None
                })
            );
        }

        #[test]
        fn correct_line() {
            assert_eq!(
                parse("a b | c"),
                Ok(LineContents {
                    comment: Some(Comment(" c".to_owned())),
                    action_invocation: Some(ActionInvocation {
                        name: Name {
                            parts: vec![
                                NamePart::Word(Word("a".to_owned())),
                                NamePart::Word(Word("b".to_owned()))
                            ]
                        }
                    })
                })
            );
        }

        #[test]
        fn just_a_comment() {
            assert_eq!(
                parse("|abc"),
                Ok(LineContents {
                    action_invocation: None,
                    comment: Some(Comment("abc".to_owned()))
                })
            );
        }
    }
}

mod program {
    use super::*;

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

    pub fn parse(program: &str) -> Result<Program, Error> {
        let mut program = program.lines().enumerate().peekable();
        let mut root = Vec::new();
        let mut levels: Vec<*mut Vec<Line>> = Vec::new();
        while let Some((index, line)) = program.next() {
            let (level, unindented) = unindent(line);
            if level != 0 && index == 0 {
                return Err(Error {
                    line_index: index,
                    kind: ErrorKind::FirstLineIndented {
                        present_indentation: level,
                    },
                });
            }
            let line_contents =
                line_contents::parse(skip_whitespace(unindented)).map_err(|error_kind| Error {
                    line_index: index,
                    kind: error_kind,
                })?;
            let line = Line {
                attached: Vec::new(),
                contents: line_contents,
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
                if line.contents.action_invocation.is_some() {
                    if next_level == level + 1 {
                        levels.push((&mut line.attached) as *mut _);
                    } else if next_level > level {
                        return Err(Error {
                            line_index: *next_index,
                            kind: ErrorKind::OverindentedLine {
                                max_allowed_indentation: level + 1,
                                present_indentation: next_level,
                            },
                        });
                    }
                } else {
                    if next_level > level {
                        return Err(Error {
                            line_index: *next_index,
                            kind: ErrorKind::IndentationLevelIncreasedAfterEmptyLine {
                                max_allowed_indentation: level,
                                present_indentation: next_level,
                            },
                        });
                    }
                }
                if let Some(rollback) = level.checked_sub(next_level) {
                    for _ in 0..rollback {
                        levels.pop().unwrap();
                    }
                }
            }
        }
        Ok(Program { contents: root })
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn line(contents: Vec<NamePart>, attached: Vec<Line>) -> Line {
            Line {
                attached,
                contents: LineContents {
                    action_invocation: Some(ActionInvocation {
                        name: Name { parts: contents },
                    }),
                    comment: None,
                },
            }
        }

        #[test]
        fn empty_program() {
            assert_eq!(parse(""), Ok(Program { contents: vec![] }));
        }

        #[test]
        #[cfg_attr(rustfmt, rustfmt_skip)]
        fn correct_indentation() {
            fn word(word: &str) -> NamePart {
                NamePart::Word(Word(word.to_owned()))
            }

            assert_eq!(
                parse("a-d\n-b\n-  -   c\n  --d\ne"),
                Ok(Program { contents: vec![
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
                ]})
            );
        }

        #[test]
        fn first_line_indented() {
            assert_eq!(
                parse("-a"),
                Err(Error {
                    line_index: 0,
                    kind: ErrorKind::FirstLineIndented {
                        present_indentation: 1
                    }
                })
            );
        }

        #[test]
        fn overindented() {
            assert_eq!(
                parse("a\n--b"),
                Err(Error {
                    line_index: 1,
                    kind: ErrorKind::OverindentedLine {
                        max_allowed_indentation: 1,
                        present_indentation: 2
                    }
                })
            );
        }

        #[test]
        fn overindented_2() {
            assert_eq!(
                parse("a\n-b\n---c"),
                Err(Error {
                    line_index: 2,
                    kind: ErrorKind::OverindentedLine {
                        max_allowed_indentation: 2,
                        present_indentation: 3
                    }
                })
            );
        }

        #[test]
        fn indentation_after_empty_line() {
            assert_eq!(
                parse("a\n-\n--b"),
                Err(Error {
                    line_index: 2,
                    kind: ErrorKind::IndentationLevelIncreasedAfterEmptyLine {
                        max_allowed_indentation: 1,
                        present_indentation: 2,
                    }
                })
            )
        }
    }
}

pub fn parse(program: &str) -> Result<Program, Error> {
    program::parse(program)
}
