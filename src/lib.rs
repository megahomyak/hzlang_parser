use parco::PositionedString;

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

struct Rest<T>(pub T);

fn parse_indentation(input: PositionedString) -> (usize, Rest(PositionedString)) {
    for c in parco::one_part() {}
}

pub fn parse(program: &str) -> Result<Vec<ActionInvocation>, Error> {
    let action_invocations = Vec::new();
    let program = PositionedString::from(program);
    for line in program.split("\n") {
        let chars = line.chars();
        
        for c in chars {
            if c.is_whitespace() {
                continue;
            }
        }
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
