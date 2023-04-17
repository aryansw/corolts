requires "corolts-syntax.k"

module COROLTS-EXECUTE
  imports COROLTS-SYNTAX
  imports DOMAINS

  syntax Val ::= Int | Bool | String
              |  array(Int, Int) // Location to first element and size
              |  func(Params, Block) // Parameters and body

  syntax Vals ::= List{Val,","} [klabel(exps)]

  syntax Exp ::= Val
  syntax Exps ::= Vals
  syntax KResult ::= Val | Vals

  syntax ControlCell
  syntax ControlCellFragment

  configuration <T color="red">
                  <k color="green"> $PGM:Stmt ~> execute </k>
                  <control color="cyan">
                    <fstack color="blue"> .List </fstack>
                  </control>
                  <env color="violet"> .Map </env>
                  <genv color="pink"> .Map </genv>
                  <store color="white"> .Map </store>
                  <input color="magenta" stream="stdin"> .List </input>
                  <output color="brown" stream="stdout"> .List </output>
                  <nextLoc color="gray"> 0 </nextLoc>
                </T>
  
  syntax KItem ::= "undefined"

  // Variable Declarations
  rule <k> let X:Id : _; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> undefined ...</store>
       <nextLoc> L => L +Int 1 </nextLoc>

  // Function Declarations
  rule <k> fn F:Id(Xs:Params) -> _ B => . ...</k>
       <env> Env => Env[F <- L] </env>
       <store>... .Map => L |-> func(Xs, B) ...</store>
       <nextLoc> L => L +Int 1 </nextLoc>


  syntax KItem ::= "execute"  
  rule <k> execute => main(.Exps); </k>
       <env> Env </env>
       <genv> .Map => Env </genv>  [structural]

  // Expression Evaluation
  rule <k> X:Id => V ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [lookup]

  context ++(HOLE => lvalue(HOLE))
  rule <k> ++loc(L) => I +Int 1 ...</k>
       <store>... L |-> (I => I +Int 1) ...</store>  [increment]

  rule I1 + I2 => I1 +Int I2
  rule Str1 + Str2 => Str1 +String Str2
  rule I1 - I2 => I1 -Int I2
  rule I1 * I2 => I1 *Int I2
  rule I1 / I2 => I1 /Int I2 requires I2 =/=K 0
  rule I1 % I2 => I1 %Int I2 requires I2 =/=K 0
  rule - I => 0 -Int I
  rule I1 < I2 => I1 <Int I2
  rule I1 <= I2 => I1 <=Int I2
  rule I1 > I2 => I1 >Int I2
  rule I1 >= I2 => I1 >=Int I2

  rule V1:Val == V2:Val => V1 ==K V2
  rule V1:Val != V2:Val => V1 =/=K V2

  rule ! T => notBool(T)
  rule true  && E => E
  rule false && _ => false
  rule true  || _ => true
  rule false || E => E

  context new _:Type[HOLE]
  rule <k> new _:Type[N:Int] => array(L, N) ...</k>
       <store>... .Map => (L +Int 1) ... (L +Int N) |-> undefined ...</store>
       <nextLoc> L => L +Int N </nextLoc> 

  rule <k> new T:Type[N1:Int, N2:Vals] => array(L, N1) ...</k>
       <store>... .Map => (L +Int 1) ... (L +Int N1) |-> new T[N2] ...</store>
       <nextLoc> L => L +Int N1 </nextLoc> 
    requires N1 >Int 0

  // Array lookups
  rule V:Val[N1:Int, N2:Int, Vs:Vals] => V[N1][N2, Vs]
    [structural]

  rule array(L,_)[N:Int] => lookup(L +Int N)
    [structural]

  rule sizeOf(array(_,N)) => N

  // Function Calls
  syntax KItem ::=  (Map,K,ControlCellFragment)

  // Build function call, switch over evaluation of arguments
  rule <k> func(Xs,S)(Vs:Vals) ~> K => mkDecls(Xs,Vs) S return; </k>
       <control>
         <fstack> .List => ListItem((Env,K,C)) ...</fstack>
         C
       </control>
       <env> Env => GEnv </env>
       <genv> GEnv </genv>

  // On return, restore environment and give result to caller
  rule <k> return(V:Val); ~> _ => V ~> K </k>
       <control>
         <fstack> ListItem((Env,K,C)) => .List ...</fstack>
         (_ => C)
       </control>
       <env> _ => Env </env>

  syntax Val ::= "nothing"
  rule return; => return nothing;

  rule <k> read() => I ...</k> <input> ListItem(I:Int) => .List ...</input>  [read]

  // Reassignment
  context (HOLE => lvalue(HOLE)) = _
  rule <k> loc(L) = V:Val => V ...</k> <store>... L |-> (_ => V) ...</store>
    [assignment]

  // Scoping (restore's environment after call)
  rule {} => .  [structural]
  rule <k> { S } => S ~> setEnv(Env) ...</k>  <env> Env </env>  [structural]

  rule S1:Stmt S2:Stmt => S1 ~> S2  [structural]
  rule _:Val; => .

  // Control Flow
  rule if true S else _ => S
  rule if false _ else S => S
  rule while E S => if E {S while E S}  [structural]

  rule <k> println!(V:Val, Es => Es); ...</k> <output>... .List => ListItem(V) </output>
    [print]
  rule println!(.Vals); => .  [structural]

  // Helper functions
  syntax Stmt ::= mkDecls(Params,Vals)  [function]
  rule mkDecls((X:Id : T:Type, Xs:Params), (V:Val, Vs:Vals)) => let X: T = V; mkDecls(Xs,Vs)
  rule mkDecls(.Params,.Vals) => {}

  syntax Exp ::= lookup(Int)
  rule <k> lookup(L) => V ...</k> <store>... L |-> V:Val ...</store>  [lookup]

  syntax KItem ::= setEnv(Map)
  rule <k> setEnv(Env) => . ...</k> <env> _ => Env </env>  [structural]
  rule (setEnv(_) => .) ~> setEnv(_)  [structural]

  // lookup value and get locations
  syntax Exp ::= lvalue(K)
  syntax Val ::= loc(Int)

  rule <k> lvalue(X:Id => loc(L)) ...</k> <env>... X |-> L:Int ...</env>
    [structural]

  context lvalue(_::Exp[HOLE::Exps])
  context lvalue(HOLE::Exp[_::Exps])

  rule lvalue(lookup(L:Int) => loc(L))  [structural]

  // Map Operations
  syntax Map ::= Int "..." Int "|->" K [function]
  rule N...M |-> _ => .Map  requires N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  requires N <=Int M

endmodule