use tree_sitter::Parser;

fn count_errors(node: &tree_sitter::Node) -> usize {
    let mut count = if node.is_error() || node.is_missing() { 1 } else { 0 };
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i) {
            count += count_errors(&child);
        }
    }
    count
}

fn test(parser: &mut Parser, name: &str, source: &str) {
    if let Some(tree) = parser.parse(source, None) {
        let errors = count_errors(&tree.root_node());
        if errors > 0 {
            println!("{}: {} errors", name, errors);
        } else {
            println!("{}: OK", name);
        }
    }
}

fn main() {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::language()).unwrap();

    // Test 1: minimal
    test(&mut parser, "Test 1 (minimal)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 2: with nested group before
    test(&mut parser, "Test 2 (nested group)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL PIC X VALUE "X".
       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 3: with OCCURS
    test(&mut parser, "Test 3 (OCCURS)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 PIC X VALUE "X".
       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 4: with OCCURS TIMES
    test(&mut parser, "Test 4 (OCCURS TIMES)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".
       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 5: with blank line
    test(&mut parser, "Test 5 (blank line)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 6: two nested groups then 01
    test(&mut parser, "Test 6 (two groups)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".
       01  TABLE-ARR2.
           05  TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".
       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 7: two groups with blank lines
    test(&mut parser, "Test 7 (groups + blanks)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

       01  TABLE-ARR2.
           05  TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 8: exact copy from file with long values
    test(&mut parser, "Test 8 (long values)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  TABLE-ARR2.
           05  TAB-VL2  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  PATH-P1                            PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
       01  PATH-P2                            PIC X(20) VALUE "NXYXXXXXNNNXXXXXXXXXX".
"#);

    // Test 9: with 7-space "blank" lines like the file
    test(&mut parser, "Test 9 (7-space blanks)", "       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE \"X\".

       01  PATH-P1                            PIC X(20) VALUE \"NXYXXXXNNYXXXXXXXXX\".
");

    // Test string lengths
    for len in [10, 15, 18, 19, 20, 21, 22] {
        let value: String = (0..len).map(|_| 'X').collect();
        let source = format!(r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1 PIC X({}) VALUE "{}".
"#, len, value);
        test(&mut parser, &format!("String len {}", len), &source);
    }

    // Test 10: just the wide spacing
    test(&mut parser, "Test 10 (wide spacing)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1                            PIC X(20) VALUE "Y".
"#);

    // Test 11: extra spacing in OCCURS
    test(&mut parser, "Test 11 (OCCURS spacing)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".
"#);

    // Test 12: extra spacing in OCCURS + following 01
    test(&mut parser, "Test 12 (OCCURS + 01)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".
       01  PATH-P1 PIC X(20) VALUE "Y".
"#);

    // Test 13: OCCURS with blank line + wide spacing 01
    test(&mut parser, "Test 13 (OCCURS + blank + 01)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  PATH-P1                            PIC X(20) VALUE "Y".
"#);

    // Test 14: same as 13 but with 19-char value
    test(&mut parser, "Test 14 (19 char value)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  PATH-P1                            PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
"#);

    // Test 15: exact as test 8 but one line at a time
    test(&mut parser, "Test 15 (exact as 8)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  TABLE-ARR2.
           05  TAB-VL2  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  PATH-P1                            PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
       01  PATH-P2                            PIC X(20) VALUE "NXYXXXXXNNNXXXXXXXXXX".
"#);

    // Test 16: just the 19-char string
    test(&mut parser, "Test 16 (19 char NXYX)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1 PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
"#);

    // Test 17: compare X's vs mixed chars
    test(&mut parser, "Test 17 (19 X's)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-ARRAY.
           05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".

       01  PATH-P1                            PIC X(20) VALUE "XXXXXXXXXXXXXXXXXXX".
"#);

    // Test 18: mixed chars with no preceding OCCURS
    test(&mut parser, "Test 18 (no OCCURS)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1                            PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
"#);

    // Test 19: wide spacing but shorter string
    test(&mut parser, "Test 19 (wide + short)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1                            PIC X(20) VALUE "Y".
"#);

    // Test 20: narrow spacing + 19 char
    test(&mut parser, "Test 20 (narrow + 19)", r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PATH-P1 PIC X(20) VALUE "NXYXXXXNNYXXXXXXXXX".
"#);

    // Count spaces in the wide version
    let wide_line = "       01  PATH-P1                            PIC X(20) VALUE \"NXYXXXXNNYXXXXXXXXX\".";
    println!("\nWide line length: {}", wide_line.len());

    // Test 21: exactly 80 chars
    let line_80 = "       01  PATH-P1                       PIC X(20) VALUE \"NXYXXXXNNYXXXXXXXXX\".";
    println!("80-char line length: {}", line_80.len());
    test(&mut parser, "Test 21 (80 chars)", &format!(r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
{}
"#, line_80));

    // Test 22: 81 chars
    let line_81 = "       01  PATH-P1                        PIC X(20) VALUE \"NXYXXXXNNYXXXXXXXXX\".";
    println!("81-char line length: {}", line_81.len());
    test(&mut parser, "Test 22 (81 chars)", &format!(r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
{}
"#, line_81));
}
