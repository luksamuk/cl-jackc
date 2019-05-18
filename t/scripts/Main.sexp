(:CLASS (:KEYWORD "class") (:IDENTIFIER "Main") (:SYMBOL "{")
 ((:CLASS-VAR-DEC (:KEYWORD "static") (:IDENTIFIER "Board")
   (:IDENTIFIER "board") (:SYMBOL ";"))
  (:CLASS-VAR-DEC (:KEYWORD "static") (:KEYWORD "int")
   (:IDENTIFIER "game_playing") (:SYMBOL ";"))
  (:CLASS-VAR-DEC (:KEYWORD "static") (:KEYWORD "int")
   (:IDENTIFIER "input_line") (:SYMBOL ";")))
 ((:SUBROUTINE-DEC (:KEYWORD "function") (:KEYWORD "int")
   (:IDENTIFIER "get_input") (:SYMBOL "(")
   (:PARAMETER-LIST ((:IDENTIFIER "String") (:IDENTIFIER "message")))
   (:SYMBOL ")")
   (:SUBROUTINE-BODY (:SYMBOL "{")
    ((:VAR-DEC (:KEYWORD "var") (:KEYWORD "int") (:IDENTIFIER "valid_input")
      (:SYMBOL ";"))
     (:VAR-DEC (:KEYWORD "var") (:KEYWORD "int") (:IDENTIFIER "input")
      (:SYMBOL ";")))
    (:STATEMENTS
     (:STATEMENT
      (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "valid_input")
       (:SYMBOL "=") (:INTEGER-CONSTANT 0) (:SYMBOL ";")))
     (:STATEMENT
      (:WHILE-STATEMENT (:KEYWORD "while") (:SYMBOL "(")
       (:EXPRESSION
        ((:IDENTIFIER "valid_input") ((:SYMBOL "=") (:INTEGER-CONSTANT 0))))
       (:SYMBOL ")") (:SYMBOL "{")
       (:STATEMENTS
        (:STATEMENT
         (:DO-STATEMENT (:KEYWORD "do")
          (:SUBROUTINE-CALL
           ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "moveCursor")
            (:SYMBOL "(")
            (:EXPRESSION-LIST (:IDENTIFIER "input_line")
             ((:SYMBOL ",") (:INTEGER-CONSTANT 0)))
            (:SYMBOL ")")))
          (:SYMBOL ";")))
        (:STATEMENT
         (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "input") (:SYMBOL "=")
          (:EXPRESSION
           (:TERM
            (:SUBROUTINE-CALL
             ((:IDENTIFIER "Keyboard") (:SYMBOL ".") (:IDENTIFIER "readInt")
              (:SYMBOL "(") (:EXPRESSION-LIST (:IDENTIFIER "message"))
              (:SYMBOL ")")))))
          (:SYMBOL ";")))
        (:STATEMENT
         (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "valid_input")
          (:SYMBOL "=") (:INTEGER-CONSTANT 1) (:SYMBOL ";")))
        (:STATEMENT
         (:IF-STATEMENT (:KEYWORD "if") (:SYMBOL "(")
          (:EXPRESSION
           ((:IDENTIFIER "input") ((:SYMBOL "<") (:INTEGER-CONSTANT 1))))
          (:SYMBOL ")") (:SYMBOL "{")
          (:STATEMENTS
           (:STATEMENT
            (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "valid_input")
             (:SYMBOL "=") (:INTEGER-CONSTANT 0) (:SYMBOL ";"))))
          (:SYMBOL "}")))
        (:STATEMENT
         (:IF-STATEMENT (:KEYWORD "if") (:SYMBOL "(")
          (:EXPRESSION
           ((:IDENTIFIER "valid_input") ((:SYMBOL "=") (:INTEGER-CONSTANT 1))))
          (:SYMBOL ")") (:SYMBOL "{")
          (:STATEMENTS
           (:STATEMENT
            (:IF-STATEMENT (:KEYWORD "if") (:SYMBOL "(")
             (:EXPRESSION
              ((:IDENTIFIER "input") ((:SYMBOL ">") (:INTEGER-CONSTANT 9))))
             (:SYMBOL ")") (:SYMBOL "{")
             (:STATEMENTS
              (:STATEMENT
               (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "valid_input")
                (:SYMBOL "=") (:INTEGER-CONSTANT 0) (:SYMBOL ";"))))
             (:SYMBOL "}"))))
          (:SYMBOL "}"))))
       (:SYMBOL "}")))
     (:STATEMENT
      (:RETURN-STATEMENT (:KEYWORD "return")
       (:EXPRESSION (:IDENTIFIER "input")) (:SYMBOL ";"))))
    (:SYMBOL "}")))
  (:SUBROUTINE-DEC (:KEYWORD "function") (:KEYWORD "void")
   (:IDENTIFIER "run_game") (:SYMBOL "(") (:PARAMETER-LIST) (:SYMBOL ")")
   (:SUBROUTINE-BODY (:SYMBOL "{")
    ((:VAR-DEC (:KEYWORD "var") (:KEYWORD "int") (:IDENTIFIER "line")
      (:SYMBOL ";"))
     (:VAR-DEC (:KEYWORD "var") (:KEYWORD "int") (:IDENTIFIER "column")
      (:SYMBOL ";"))
     (:VAR-DEC (:KEYWORD "var") (:KEYWORD "char") (:IDENTIFIER "answ")
      (:SYMBOL ";")))
    (:STATEMENTS
     (:STATEMENT
      (:WHILE-STATEMENT (:KEYWORD "while") (:SYMBOL "(")
       (:EXPRESSION
        (:TERM
         ((:SYMBOL "~")
          (:TERM
           ((:SYMBOL "(")
            (:EXPRESSION
             ((:IDENTIFIER "game_playing")
              ((:SYMBOL "=") (:INTEGER-CONSTANT 0))))
            (:SYMBOL ")"))))))
       (:SYMBOL ")") (:SYMBOL "{")
       (:STATEMENTS
        (:STATEMENT
         (:DO-STATEMENT (:KEYWORD "do")
          (:SUBROUTINE-CALL
           ((:IDENTIFIER "board") (:SYMBOL ".") (:IDENTIFIER "print")
            (:SYMBOL "(") (:EXPRESSION-LIST (:INTEGER-CONSTANT 0))
            (:SYMBOL ")")))
          (:SYMBOL ";")))
        (:STATEMENT
         (:DO-STATEMENT (:KEYWORD "do")
          (:SUBROUTINE-CALL
           ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "println")
            (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
          (:SYMBOL ";")))
        (:STATEMENT
         (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "line") (:SYMBOL "=")
          (:EXPRESSION
           (:TERM
            (:SUBROUTINE-CALL
             ((:IDENTIFIER "Main") (:SYMBOL ".") (:IDENTIFIER "get_input")
              (:SYMBOL "(")
              (:EXPRESSION-LIST
               (:STRING-CONSTANT "\"Type a number (1-9) for line:   \""))
              (:SYMBOL ")")))))
          (:SYMBOL ";")))
        (:STATEMENT
         (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "column") (:SYMBOL "=")
          (:EXPRESSION
           (:TERM
            (:SUBROUTINE-CALL
             ((:IDENTIFIER "Main") (:SYMBOL ".") (:IDENTIFIER "get_input")
              (:SYMBOL "(")
              (:EXPRESSION-LIST
               (:STRING-CONSTANT "\"Type a number (1-9) for column: \""))
              (:SYMBOL ")")))))
          (:SYMBOL ";")))
        (:STATEMENT
         (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "line") (:SYMBOL "=")
          (:EXPRESSION
           (:TERM
            (:SUBROUTINE-CALL
             ((:IDENTIFIER "board") (:SYMBOL ".") (:IDENTIFIER "step")
              (:SYMBOL "(")
              (:EXPRESSION-LIST
               (:EXPRESSION
                ((:IDENTIFIER "column") ((:SYMBOL "-") (:INTEGER-CONSTANT 1))))
               ((:SYMBOL ",")
                (:EXPRESSION
                 ((:IDENTIFIER "line")
                  ((:SYMBOL "-") (:INTEGER-CONSTANT 1))))))
              (:SYMBOL ")")))))
          (:SYMBOL ";")))
        (:STATEMENT
         (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "column") (:SYMBOL "=")
          (:EXPRESSION
           (:TERM
            (:SUBROUTINE-CALL
             ((:IDENTIFIER "Random") (:SYMBOL ".") (:IDENTIFIER "rand")
              (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))))
          (:SYMBOL ";")))
        (:STATEMENT
         (:IF-STATEMENT (:KEYWORD "if") (:SYMBOL "(")
          (:EXPRESSION
           ((:IDENTIFIER "line") ((:SYMBOL "=") (:INTEGER-CONSTANT 1))))
          (:SYMBOL ")") (:SYMBOL "{")
          (:STATEMENTS
           (:STATEMENT
            (:DO-STATEMENT (:KEYWORD "do")
             (:SUBROUTINE-CALL
              ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "moveCursor")
               (:SYMBOL "(")
               (:EXPRESSION-LIST (:IDENTIFIER "input_line")
                ((:SYMBOL ",") (:INTEGER-CONSTANT 0)))
               (:SYMBOL ")")))
             (:SYMBOL ";")))
           (:STATEMENT
            (:DO-STATEMENT (:KEYWORD "do")
             (:SUBROUTINE-CALL
              ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "printString")
               (:SYMBOL "(")
               (:EXPRESSION-LIST
                (:STRING-CONSTANT "\"You stepped on a mine! Game over.\""))
               (:SYMBOL ")")))
             (:SYMBOL ";")))
           (:STATEMENT
            (:DO-STATEMENT (:KEYWORD "do")
             (:SUBROUTINE-CALL
              ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "println")
               (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
             (:SYMBOL ";")))
           (:STATEMENT
            (:DO-STATEMENT (:KEYWORD "do")
             (:SUBROUTINE-CALL
              ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "printString")
               (:SYMBOL "(")
               (:EXPRESSION-LIST (:STRING-CONSTANT "\"Play again? (Y/N) \""))
               (:SYMBOL ")")))
             (:SYMBOL ";")))
           (:STATEMENT
            (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "answ") (:SYMBOL "=")
             (:EXPRESSION
              (:TERM
               (:SUBROUTINE-CALL
                ((:IDENTIFIER "Keyboard") (:SYMBOL ".")
                 (:IDENTIFIER "readChar") (:SYMBOL "(") (:EXPRESSION-LIST)
                 (:SYMBOL ")")))))
             (:SYMBOL ";")))
           (:STATEMENT
            (:IF-STATEMENT (:KEYWORD "if") (:SYMBOL "(")
             (:EXPRESSION
              ((:IDENTIFIER "answ") ((:SYMBOL "=") (:INTEGER-CONSTANT 89))))
             (:SYMBOL ")") (:SYMBOL "{")
             (:STATEMENTS
              (:STATEMENT
               (:DO-STATEMENT (:KEYWORD "do")
                (:SUBROUTINE-CALL
                 ((:IDENTIFIER "board") (:SYMBOL ".") (:IDENTIFIER "dispose")
                  (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
                (:SYMBOL ";")))
              (:STATEMENT
               (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "board")
                (:SYMBOL "=")
                (:EXPRESSION
                 (:TERM
                  (:SUBROUTINE-CALL
                   ((:IDENTIFIER "Board") (:SYMBOL ".") (:IDENTIFIER "new")
                    (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))))
                (:SYMBOL ";")))
              (:STATEMENT
               (:DO-STATEMENT (:KEYWORD "do")
                (:SUBROUTINE-CALL
                 ((:IDENTIFIER "Output") (:SYMBOL ".")
                  (:IDENTIFIER "moveCursor") (:SYMBOL "(")
                  (:EXPRESSION-LIST
                   (:EXPRESSION
                    ((:IDENTIFIER "input_line")
                     ((:SYMBOL "+") (:INTEGER-CONSTANT 1))))
                   ((:SYMBOL ",") (:INTEGER-CONSTANT 0)))
                  (:SYMBOL ")")))
                (:SYMBOL ";")))
              (:STATEMENT
               (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "line")
                (:SYMBOL "=") (:INTEGER-CONSTANT 0) (:SYMBOL ";")))
              (:STATEMENT
               (:WHILE-STATEMENT (:KEYWORD "while") (:SYMBOL "(")
                (:EXPRESSION
                 (:TERM
                  ((:SYMBOL "~")
                   (:TERM
                    ((:SYMBOL "(")
                     (:EXPRESSION
                      ((:IDENTIFIER "line")
                       ((:SYMBOL "=") (:INTEGER-CONSTANT 20))))
                     (:SYMBOL ")"))))))
                (:SYMBOL ")") (:SYMBOL "{")
                (:STATEMENTS
                 (:STATEMENT
                  (:DO-STATEMENT (:KEYWORD "do")
                   (:SUBROUTINE-CALL
                    ((:IDENTIFIER "Output") (:SYMBOL ".")
                     (:IDENTIFIER "printChar") (:SYMBOL "(")
                     (:EXPRESSION-LIST (:INTEGER-CONSTANT 32)) (:SYMBOL ")")))
                   (:SYMBOL ";")))
                 (:STATEMENT
                  (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "line")
                   (:SYMBOL "=")
                   (:EXPRESSION
                    ((:IDENTIFIER "line")
                     ((:SYMBOL "+") (:INTEGER-CONSTANT 1))))
                   (:SYMBOL ";"))))
                (:SYMBOL "}"))))
             (:SYMBOL "}")
             ((:KEYWORD "else") (:SYMBOL "{")
              (:STATEMENTS
               (:STATEMENT
                (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "game_playing")
                 (:SYMBOL "=") (:INTEGER-CONSTANT 0) (:SYMBOL ";"))))
              (:SYMBOL "}")))))
          (:SYMBOL "}"))))
       (:SYMBOL "}")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "println")
         (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "printString")
         (:SYMBOL "(")
         (:EXPRESSION-LIST (:STRING-CONSTANT "\"Thanks for playing!\""))
         (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT (:RETURN-STATEMENT (:KEYWORD "return") (:SYMBOL ";"))))
    (:SYMBOL "}")))
  (:SUBROUTINE-DEC (:KEYWORD "function") (:KEYWORD "void") (:IDENTIFIER "main")
   (:SYMBOL "(") (:PARAMETER-LIST) (:SYMBOL ")")
   (:SUBROUTINE-BODY (:SYMBOL "{")
    (:STATEMENTS
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Random") (:SYMBOL ".") (:IDENTIFIER "init")
         (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT
      (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "board") (:SYMBOL "=")
       (:EXPRESSION
        (:TERM
         (:SUBROUTINE-CALL
          ((:IDENTIFIER "Board") (:SYMBOL ".") (:IDENTIFIER "new")
           (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))))
       (:SYMBOL ";")))
     (:STATEMENT
      (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "game_playing")
       (:SYMBOL "=") (:INTEGER-CONSTANT 1) (:SYMBOL ";")))
     (:STATEMENT
      (:LET-STATEMENT (:KEYWORD "let") (:IDENTIFIER "input_line") (:SYMBOL "=")
       (:INTEGER-CONSTANT 15) (:SYMBOL ";")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "printString")
         (:SYMBOL "(")
         (:EXPRESSION-LIST
          (:STRING-CONSTANT
           "\"Minesweeper for the nand2tetris Hack Platform\""))
         (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "println")
         (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Output") (:SYMBOL ".") (:IDENTIFIER "printString")
         (:SYMBOL "(")
         (:EXPRESSION-LIST
          (:STRING-CONSTANT "\"v1.0 Copyright (c) 2019 Lucas Vieira\""))
         (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "Main") (:SYMBOL ".") (:IDENTIFIER "run_game")
         (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT
      (:DO-STATEMENT (:KEYWORD "do")
       (:SUBROUTINE-CALL
        ((:IDENTIFIER "board") (:SYMBOL ".") (:IDENTIFIER "dispose")
         (:SYMBOL "(") (:EXPRESSION-LIST) (:SYMBOL ")")))
       (:SYMBOL ";")))
     (:STATEMENT (:RETURN-STATEMENT (:KEYWORD "return") (:SYMBOL ";"))))
    (:SYMBOL "}"))))
 (:SYMBOL "}"))