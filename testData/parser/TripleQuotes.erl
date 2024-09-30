quotes() ->
    S = """
        "I always have a quotation for everything -
        it saves original thinking." - Dorothy L. Sayers

        "Real stupidity beats artificial intelligence every time."
        - Terry Pratchett
        """,
    io:put_chars(S),
    io:nl().


bad_quotes() -> """missing new line""".