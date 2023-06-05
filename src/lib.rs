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
    OverIndented,
}

pub fn parse(program: &str) -> Result<Vec<Line<'_>>, Error> {
    let mut program = program.lines().enumerate().peekable();
    let mut root = Vec::new();
    let mut levels: Vec<*mut Vec<Line>> = Vec::new();
    while let Some((index, line)) = program.next() {
        let (level, unindented) = unindent(line);
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
        if let Some((_next_index, next_line)) = program.peek() {
            let (next_level, _next_unindented) = unindent(next_line);
            if next_level == level + 1 {
                levels.push((&mut line.attached) as *mut _);
            } else if next_level > level {
                return Err(Error::OverIndented);
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
    fn parsing() {
        assert_eq!(
            parse("a\n-b\n-  -   c\n  --d\ne"),
            Ok(vec![
                line(
                    "a",
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
}
