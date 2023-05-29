use parco::{PositionedString, Rest};

pub enum Error {
    ExtraColon,
    UnexpectedIndentation,
}

pub struct Expression {
    contents: Vec<Word>,
}

pub enum Word {
    Raw,
    Expression(Expression),
}

pub struct ActionInvocation {
    contents: Vec<Word>,
    attached_invocations: Vec<ActionInvocation>,
}

pub type ParsingResult<'a, T> = parco::Result<T, PositionedString<'a>, Error>;

fn parse_indentation(mut line: PositionedString) -> (usize, Rest<PositionedString>) {
    let mut level = 0;
    while let parco::Result::Ok((c, Rest(rest))) = parco::one_part::<_, ()>(line) {
        if c == '-' {
            level += 1;
        } else if !c.is_whitespace() || c == '\n' {
            break;
        }
        line = rest;
    }
    (level, Rest(line))
}

fn parse_lines(lines: impl Iterator<Item = &str>) -> impl Iterator

pub fn parse(program: &str) -> Result<Vec<ActionInvocation>, Error> {
    let action_invocations = Vec::new();
    let program = PositionedString::from(program);
    for line in program.split("\n") {
        let (indentation_level, Rest(rest)) = parse_indentation(line);
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
