use std::{
    fs::{read_dir, File},
    io::{Read, Write},
};

use dreamberdc::parser::parse_program;
use nom_locate::LocatedSpan;

#[test]
fn ui() {
    let bless = std::env::var("BLESS").is_ok();

    if bless {
        bless_tests();
    }

    for file in read_dir("tests/passing")
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
    {
        let file_name = file.file_name();

        let file_contents = {
            let mut file = File::open(&file_name).unwrap();
            let mut buf = String::new();
            file.read_to_string(&mut buf).unwrap();

            buf
        };

        let (leftover, program) = parse_program(LocatedSpan::new(&file_contents)).unwrap();
        assert_eq!(*leftover.fragment(), "");
        let program_text = format!("{program:#?}");

        let output_contents = {
            let mut file = File::open("tests/passing/{file_name}.out").unwrap();
            let mut buf = String::new();
            file.read_to_string(&mut buf).unwrap();

            buf
        };

        assert_eq!(program_text, output_contents);
    }
}

fn bless_tests() {
    for file in read_dir("tests/passing")
        .unwrap()
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
    {
        let file_name = file.file_name().into_string().unwrap();

        let file_contents = {
            let mut file = File::open(format!("tests/passing/{file_name}")).unwrap();
            let mut buf = String::new();
            file.read_to_string(&mut buf).unwrap();

            buf
        };

        let (leftover, program) = parse_program(LocatedSpan::new(&file_contents)).unwrap();
        assert_eq!(*leftover.fragment(), "");
        let program_text = format!("{program:#?}");

        let mut output = File::create(format!("tests/passing/{file_name}.out")).unwrap();
        output.write_all(program_text.as_bytes()).unwrap();
        println!("Successfully blessed {file_name:#?}");
    }
}
