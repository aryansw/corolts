module RUST-SYNTAX
  imports INT-SYNTAX
  imports ID-SYNTAX
  imports BOOL-SYNTAX
  imports STRING-SYNTAX

  syntax KResult ::= Int | Bool | String | "()"

  syntax Exp ::= Int | Bool | String 
                  | "()" 
                  | Id 
                  | "(" Exp ")" [bracket]
                  | "{" Seq "}"
                  | IExp
                  | BExp
                  | "while" Exp Exp
                  | "let" Id "=" Exp
                  | IfExp
  
  syntax Seq ::= List{Exp,";"}

  syntax IfExp ::= "if" Exp Exp "else" Exp
                   | "if" Exp Exp

  syntax IExp ::= Exp "+" Exp [add] | Exp "-" Exp [add] | Exp "*" Exp [mul] | Exp "/" Exp [mul]

  syntax BExp ::= Exp "&&" Exp | Exp "||" Exp | "!" Exp
                  | Exp "<" Exp | Exp "<=" Exp | Exp ">" Exp | Exp ">=" Exp | Exp "==" Exp | Exp "!=" Exp

  syntax priorities mul > add
  syntax left mul
  syntax left add

  syntax Pgm ::= Seq
endmodule

module RUST
  imports RUST-SYNTAX

  configuration <k> $PGM:Pgm </k>
endmodule