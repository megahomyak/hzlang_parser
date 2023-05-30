pub enum Error {
    ExtraColon {
        line_number: usize,
    },
    UnexpectedIndentation {
        line_number: usize,
        present_indentation: usize,
    },
}

fn take(s: &str) -> Option<char> {}

mod lines {
    use crate::ActionInvocation;

    pub enum Error {
        ExtraColon {
            line_number: usize,
        },
        UnexpectedIndentation {
            line_index: usize,
            present_indentation: usize,
        },
    }

    fn parse<'a>(raw_lines: impl Iterator<Item = &'a str>) -> Result<Vec<ActionInvocation>, Error> {
        // Repeat while there are lines:
        // * Remove the empty lines
        // * Read a line that either creates an indent or not (no lines may be indented until now)
        // * If it indents, call this function recursively, removing one level of indentation from the
        // following input strings (only pass the lines that are indented)
        // * If it does not indent, just parse the line and move on
        let parsed_lines = Vec::new();
        for (line_index, raw_line) in raw_lines.enumerate() {

        }
    }
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

pub fn parse(program: &str) -> Result<Vec<ActionInvocation>, Error> {
    let action_invocations = Vec::new();
    todo!()
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
