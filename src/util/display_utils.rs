pub fn indent_string(string: String, start_line: usize, indentation: usize) -> String {
    let mut output = String::new();
    for (num, line) in string.lines().enumerate() {
        if (start_line <= num) {
            output.push_str(&format!("{}{}\n", " ".repeat(indentation), line));
        } else {
            output.push_str(line);
            output.push('\n');
        }
    }
    output
}
