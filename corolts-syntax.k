module COROLTS-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Id ::= "main" [token]

  // Types
  syntax Type ::= "void" | "int" | "bool" | "string"
                | Type "[" "]"
                | "mut" Type               [strict]
                | "&" Type                 [strict]
                | "&mut" Type              [strict]
                | "(" Type ")"             [bracket]
                > Types "->" Type

  syntax Types ::= List{Type,","}          [klabel(exps)]
  
  // Declarations
  syntax Param ::= Id ":" Type
  syntax Params ::= List{Param,","}

  syntax Stmt ::= "let" Id ":" Type ";"
                | "let" Id ":" Type "=" Exp ";"          
                | "fn" Id "(" Params ")" "->" Type Block
                | "fn" Id "(" Params ")" Block           
  
  // Expressions
  syntax Exp ::= Int | Bool | String | Id
               // New Rust syntax
               | "&" Exp                 [strict]
               | "&mut" Exp              [strict]
               | "*" Exp                 [strict]
               // End of Rust syntax
               | "(" Exp ")"             [bracket]
               | "++" Exp
               > Exp "[" Exps "]"        [strict]
               > "new" Type "[" Exps "]" 
               > Exp "(" Exps ")"        [strict]
               | "-" Exp                 [strict]
               | "sizeOf" "(" Exp ")"    [strict]
               | "read" "(" ")"
               > left:
                 Exp "*" Exp             [strict, left]
               | Exp "/" Exp             [strict, left]
               | Exp "%" Exp             [strict, left]
               > left:
                 Exp "+" Exp             [strict, left]
               | Exp "-" Exp             [strict, left]
               > non-assoc:
                 Exp "<" Exp             [strict, non-assoc]
               | Exp "<=" Exp            [strict, non-assoc]
               | Exp ">" Exp             [strict, non-assoc]
               | Exp ">=" Exp            [strict, non-assoc]
               | Exp "==" Exp            [strict, non-assoc]
               | Exp "!=" Exp            [strict, non-assoc]
               > "!" Exp                 [strict]
               > left:
                 Exp "&&" Exp            [strict, left]
               | Exp "||" Exp            [strict, left]
               > Exp "=" Exp             [strict(2), right]

  syntax Exps ::= List{Exp,","}          [strict, klabel(exps)]

  // Statements
  syntax Block ::= "{" "}"
                | "{" Stmt "}"

  syntax Stmt ::= Block
                | Exp ";"                                  [strict]
                | "if" Exp Block "else" Block      [avoid, strict(1)]
                | "if" Exp Block                   [macro]
                | "while" Exp Block                [strict]
                | "for" "(" Stmt Exp ";" Exp ")" Block     [macro]
                | "return" Exp ";"                         [strict]
                | "return" ";"
                | "println!" "(" Exps ")" ";"                 [strict]
  
  syntax Stmt ::= Stmt Stmt                             [right]

  // Expanding Macros
  rule if E S => if E S else {}
  rule for(Start Cond; Step) {S:Stmt} => {Start while(Cond){S Step;}}
  rule for(Start Cond; Step) {} => {Start while(Cond){Step;}}
  rule fn Id(Params) B:Block => fn Id(Params) -> void B
  rule let X:Id : T:Type = E; => let X : T; X = E;                                  [anywhere]

endmodule