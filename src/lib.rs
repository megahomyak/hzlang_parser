#[derive(Debug, PartialEq)]
struct Node<'a> {
    name: &'a str,
    children: Vec<Node<'a>>,
}

fn parse_input(input: &str) -> Result<Vec<Node>, &'static str> {
    let mut lines = input.lines().peekable();
    let mut nodes = Vec::new();

    while let Some(line) = lines.next() {
        let (indent, rest) = count_indentation(line);

        if let Some(next_line) = lines.peek() {
            let (next_indent, next_rest) = count_indentation(next_line);

            if next_indent > indent + 1 {
                return Err("Invalid indentation");
            } else if next_indent == indent + 1 {
                let node = Node {
                    name: rest.trim(),
                    children: parse_children(&mut lines)?,
                };
                nodes.push(node);
            } else {
                let node = Node {
                    name: rest.trim(),
                    children: Vec::new(),
                };
                nodes.push(node);
            }
        } else {
            let node = Node {
                name: rest.trim(),
                children: Vec::new(),
            };
            nodes.push(node);
        }
    }

    Ok(nodes)
}

fn count_indentation(mut line: &str) -> (usize, &str) {
    let mut counter = 0;
    loop {
        match line
            .trim_start_matches(char::is_whitespace)
            .strip_prefix("-")
        {
            None => return (counter, line),
            Some(new_line) => {
                line = new_line;
                counter += 1;
            }
        }
    }
}

fn parse_children<'a>(
    lines: &mut std::iter::Peekable<impl Iterator<Item = &'a str>>,
) -> Result<Vec<Node<'a>>, &'static str> {
    let mut nodes = Vec::new();

    while let Some(line) = lines.next() {
        let (indent, rest) = count_indentation(line);

        if let Some(next_line) = lines.peek() {
            let (next_indent, next_rest) = count_indentation(next_line);

            if next_indent > indent + 1 {
                return Err("Invalid indentation");
            } else if next_indent == indent + 1 {
                let node = Node {
                    name: rest.trim(),
                    children: parse_children(lines)?,
                };
                nodes.push(node);
            } else {
                let node = Node {
                    name: rest.trim(),
                    children: Vec::new(),
                };
                nodes.push(node);
            }
        } else {
            let node = Node {
                name: rest.trim(),
                children: Vec::new(),
            };
            nodes.push(node);
        }
    }

    Ok(nodes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        let input = "a\n- b\n- c\n- - d\ne";
        let expected = vec![
            Node {
                name: "a",
                children: vec![
                    Node {
                        name: "b",
                        children: vec![],
                    },
                    Node {
                        name: "c",
                        children: vec![Node {
                            name: "d",
                            children: vec![],
                        }],
                    },
                ],
            },
            Node {
                name: "e",
                children: vec![],
            },
        ];
        assert_eq!(parse_input(input).unwrap(), expected);
    }

    #[test]
    fn test_parse_input_invalid_indentation() {
        let input = "a\n- - b";
        assert_eq!(parse_input(input), Err("Invalid indentation"));
    }

    #[test]
    fn test_parse_input_empty_input() {
        let input = "";
        let expected: Vec<Node> = Vec::new();
        assert_eq!(parse_input(input).unwrap(), expected);
    }
}
