# DreamBerd Compiler / Interpreter

Parser in progress

# TODO (parser)

* array access
* `after`
* lifetimes
* closures
* enforcing indentation
* string quotation (just `"foo"` for now)
* string interpolation (but we can maybe deal with that at runtime)
* type annotations
* regexp
* nested classes (oops)
* DBX (JSX)
* noop is just an (unquoted) string (oops)
* array destructuring
* Automatic-Bracket-Insertion
* Automatic-Quotation-Marks-Insertion
* Automatic-Insertion

Not from the spec (from [LICENSE.md](https://github.com/TodePond/DreamBerd/blob/main/LICENSE.md))
* `else` (for `when`)
* `else when`
* `else` (for `if`)
* `else if`
* modulus
* and / or
* omitted braces (for `when`, `if`, etc.)
* dicts

# Example output

```
❯ cargo run -- tests/test.db
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
     Running `target/debug/dreamberdc tests/test.db`
Program {
    items: [
        Statement(
            Expr(
                FreeFnCall(
                    FreeFnCallExpr {
                        fn_name: Ident {
                            name: "print",
                        },
                        params: [
                            StringLit(
                                StringLit {
                                    contents: "Hello world",
                                },
                            ),
                        ],
                    },
                ),
            ),
        ),
    ],
}
```
