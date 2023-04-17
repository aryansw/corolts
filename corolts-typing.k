requires "corolts-syntax.k"

module COROLTS-TYPING
  imports COROLTS-SYNTAX
  imports DOMAINS

  syntax Exp ::= Type
  syntax Exps ::= Types
  syntax BlockOrStmtType ::= "block" | "stmt"
  syntax Type ::= BlockOrStmtType
  syntax Block ::= BlockOrStmtType
  syntax KResult ::= Type
                   | Types

  configuration <T color="yellow">
                  <tasks color="orange">
                    <task multiplicity="*" color="yellow" type="Set">
                      <k color="green"> $PGM:Stmt </k>
                      <tenv multiplicity="?" color="cyan"> .Map </tenv>
                      <returnType multiplicity="?" color="black"> void </returnType>
                    </task>
                  </tasks>
                  <gtenv color="blue"> .Map </gtenv>
                </T>

  rule <task> <k> let X:Id : T:Type; => stmt ...</k> </task>
       <gtenv> Rho (.Map => X |-> T) </gtenv>
    requires notBool(X in keys(Rho))
  rule <k> let X:Id : T:Type ; => stmt ...</k> <tenv> Rho => Rho[X <- T] </tenv>

  context let _::Exp[HOLE::Exps] : _:Type;
  rule let E:Exp[int,Ts:Types] : T:Type; => let E[Ts] : T[];  [structural]
  rule let E:Exp[.Types] : T:Type; => let E : T;          [structural]

  rule <task> <k> fn F:Id (Ps:Params) -> T:Type S => let F : getTypes(Ps)->T; ...</k> </task>
       (.Bag => <task>
               <k> mkDecls(Ps) S </k> <tenv> .Map </tenv> <returnType> T </returnType>
             </task>)
    [structural]
  
  rule <task> <k> stmt => main(.Exps); </k> (.Bag => <tenv> .Map </tenv>) </task>
    [structural]

  rule <task>... <k> _:BlockOrStmtType </k> <tenv> _ </tenv> ...</task> => .Bag

  rule _:Int => int
  rule _:Bool => bool
  rule _:String => string

  rule <k> X:Id => T ...</k> <tenv>... X |-> T ...</tenv>

  rule <k> X:Id => T ...</k> <tenv> Rho </tenv> <gtenv>... X |-> T ...</gtenv>
    requires notBool(X in keys(Rho))

  rule <task> <k> X:Id => T ...</k> </task> <gtenv>... X |-> T ...</gtenv>

  context ++(HOLE => ltype(HOLE))
  rule ++ int => int

  rule int + int => int
  rule string + string => string
  rule int - int => int
  rule int * int => int
  rule int / int => int
  rule int % int => int
  rule - int => int
  rule int < int => bool
  rule int <= int => bool
  rule int > int => bool
  rule int >= int => bool
  rule T:Type == T => bool
  rule T:Type != T => bool
  rule bool && bool => bool
  rule bool || bool => bool
  rule ! bool => bool

  rule (T[])[int, Ts:Types] => T[Ts]
  rule T:Type[.Types] => T

  rule sizeOf(_T[]) => int

  rule read() => int

  rule println!(T:Type, Ts => Ts); requires T ==K int orBool T ==K string
  rule println!(.Types); => stmt

  context (HOLE => ltype(HOLE)) = _
  rule T:Type = T => T

  rule (Ts:Types -> T)(Ts) => T requires Ts =/=K .Types
  rule (void -> T)(.Types) => T

  rule <k> return T:Type; => stmt ...</k> <returnType> T </returnType>
  rule <k> return; => stmt ...</k> <returnType> _ </returnType>

  rule {} => block

  rule <task> <k> {S} => block ...</k> <tenv> Rho </tenv> R </task>
       (.Bag => <task> <k> S </k> <tenv> Rho </tenv> R </task>)

  rule _:Type; => stmt

  rule if bool block else block => stmt
  rule while bool block => stmt

  rule _:BlockOrStmtType _:BlockOrStmtType => stmt

  syntax Stmt ::= mkDecls(Params)  [function]
  rule mkDecls(X:Id : T:Type , Ps:Params) => let X: T; mkDecls(Ps)
  rule mkDecls(.Params) => {}

  syntax LValue ::= Id
  rule isLValue(_:Exp[_:Exps]) => true
  syntax Exp ::= LValue 

  syntax Exp ::= ltype(Exp)

  context ltype(HOLE) requires isLValue(HOLE)

  syntax Types ::= getTypes(Params)  [function]
  rule getTypes(_:Id : T:Type) => T, .Types
  rule getTypes( _:Id : T:Type, P, Ps) => T, getTypes(P,Ps)
  rule getTypes(.Params) => void, .Types

endmodule