# DreamBerd Compiler / Interpreter

Parser in progress

# TODO

* Debug `?` suffix
* Lots more...

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
