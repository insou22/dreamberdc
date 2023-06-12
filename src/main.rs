use std::{fs::File, io::Read};

use dreamberdc::parser::parse_program;
use nom_locate::LocatedSpan;
#[cfg(feature = "trace")]
use nom_tracable::{cumulative_histogram, histogram, TracableInfo};

fn main() {
    let file_name = match std::env::args().nth(1) {
        Some(file_name) => file_name,
        None => {
            eprintln!("No file name provided!");
            return;
        }
    };

    let file_contents = match File::open(file_name).and_then(|mut file| {
        let mut buf = String::new();
        file.read_to_string(&mut buf).map(|_| buf)
    }) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error opening file: {err:?}");
            return;
        }
    };

    #[cfg(feature = "trace")]
    let parsed = parse_program(LocatedSpan::new_extra(
        &file_contents,
        TracableInfo::new().parser_width(64).fold("term"),
    ));
    #[cfg(not(feature = "trace"))]
    let parsed = parse_program(LocatedSpan::new(&file_contents));

    match parsed {
        Ok((leftover, parsed)) => match leftover.as_ref() {
            "" => {
                println!("{parsed:#?}");
            }
            leftover => {
                println!("{parsed:#?}");
                println!("Parsing did not complete... Leftover:");
                println!("{leftover}");

                #[cfg(feature = "trace")]
                {
                    histogram();
                    cumulative_histogram();
                }
            }
        },
        Err(err) => {
            eprintln!("Error parsing file: {err}");

            #[cfg(feature = "trace")]
            {
                histogram();
                cumulative_histogram();
            }
        }
    }
}
