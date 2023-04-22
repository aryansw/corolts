requires "corolts-syntax.k"

module COROLTS-EXECUTE
  imports COROLTS-SYNTAX
  imports DOMAINS

  syntax Val ::= Int | Bool | String
              |  array(Type, Int, Int) // Location to first element and size
              |  func(Type, Params, Block) // Parameters and body

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
                    <returnType color="LimeGreen"> void </returnType>
                  </control>
                  <env color="violet"> .Map </env>
                  <genv color="pink"> .Map </genv>
                  <store color="white"> .Map </store>
                  <nextLoc color="gray"> 0 </nextLoc>
                  // Rust based semantics
                  <mutType color="orange"> .Map </mutType>
                  <borrow color="yellow"> .Map </borrow>
                  <ref color="magenta"> .Map </ref>
                  <refType color="brown"> .Map </refType>
                  <moved color="purple"> .Map </moved>
                  <input color="magenta" stream="stdin"> .List </input>
                  <output color="brown" stream="stdout"> .List </output>
                </T>
  
  syntax KItem ::= "undefined"

  // Variable Declarations
  rule <k> let X:Id : T:Type; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> undefined ...</store>
       <typeEnv>... .Map =>  L |-> T ...</typeEnv>
       <mutType>... .Map => L |-> false ...</mutType>
       <borrow>... .Map => L |-> undefined ...</borrow>
       <ref>... .Map => L |-> undefined ...</ref>
       <refType>... .Map => L |-> undefined ...</refType>
       <moved>... .Map => L |-> false ...</moved>
       <nextLoc> L => L +Int 1 </nextLoc>

  rule <k> let X:Id : T:Type; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> undefined ...</store>
       <typeEnv>... .Map =>  L |-> T ...</typeEnv>
       <mutType>... .Map => L |-> true ...</mutType>
       <borrow>... .Map => L |-> undefined ...</borrow>
       <ref>... .Map => L |-> undefined ...</ref>
       <refType>... .Map => L |-> undefined ...</refType>
       <moved>... .Map => L |-> false ...</moved>
       <nextLoc> L => L +Int 1 </nextLoc>

  // Array declarations
  context new _:Type[HOLE]
  
  rule <k> new T:Type[N:Int] => array(T, L, N) ...</k>
       <store>... .Map => (L +Int 1) ... (L +Int N) |-> undefined ...</store>
       <nextLoc> L => L +Int N </nextLoc> 
    requires N >Int 0

  // Function Declarations
  rule <k> fn F:Id(Xs:Params) -> T:Type B => . ...</k>
       <env> Env => Env[F <- L] </env>
       <store>... .Map => L |-> func(T, Xs, B) ...</store>
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

  // Array lookups
  rule V:Val[N1:Int, N2:Int, Vs:Vals] => V[N1][N2, Vs]
    [structural, anywhere]

  rule array(_:Type, L:Int, M:Int)[N:Int] => lookup(L +Int N)
    when N >=Int 0 andBool N <Int M [structural, anywhere]

  rule sizeOf(array(_,_,N)) => N

  // Function Calls
  syntax KItem ::= (Type,Map,K,ControlCellFragment)

  // Build function call, switch over evaluation of arguments
  rule <k> func(T,Xs,S)(Vs:Vals) ~> K => mkDecls(Xs,Vs) S return; </k>
       <control>
         <fstack> .List => ListItem((T',Env,K,C)) ...</fstack>
         <returnType> T' => T </returnType>
         C
       </control>
       <env> Env => GEnv </env>
       <genv> GEnv </genv>

  // On return, restore environment and give result to caller
  rule <k> return(V:Val); ~> _ => V ~> K </k>
       <control>
         <fstack> ListItem((T',Env,K,C)) => .List ...</fstack>
         <returnType> T => T' </returnType>
         (_ => C)
       </control>
       <env> _ => Env </env>
    when typeOf(V) ==K T

  syntax Val ::= nothing(Type)
  rule <k> return; => return nothing(T); ...</k> <returnType> T </returnType> [structural]

  rule <k> read() => I ...</k> <input> ListItem(I:Int) => .List ...</input>  [read]

  // Reassignment
  context (HOLE => lvalue(HOLE)) = _

  rule <k> loc(L) = V:Val => V ...</k> 
       <typeEnv>... .Map => L |-> T ...</typeEnv>
       <mutType>... .Map => L |-> true ...</mutType>
       <store>... .Map => L |-> (_ => V) ...</store> 
       when T ==K typeOf(V') [assignment]

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
  rule mkDecls((X:Id : T:Type, Xs:Params), (V:Val, Vs:Vals)) => let X : T = V; mkDecls(Xs,Vs)
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

  syntax Type ::= Type "<" Vals ">"  [function]
  rule T:Type<_,Vs:Vals> => T[]<Vs>
  rule T:Type<.Vals> => T

  // Type of a value.
  syntax Type ::= typeOf(K)  [function]
  rule typeOf(_:Int) => int
  rule typeOf(_:Bool) => bool
  rule typeOf(_:String) => string
  rule typeOf(array(T,_,_)) => (T[])   // () needed! K parses [] as "no tags"
  rule typeOf(func(T,Ps,_)) => getTypes(Ps) -> T
  rule typeOf(nothing(T)) => T

  syntax Types ::= getTypes(Params)  [function]
  rule getTypes(_:Id : T:Type) => T, .Types   // I would like to not use .Types
  rule getTypes(_:Id : T:Type, P, Ps) => T, getTypes(P,Ps)
  rule getTypes(.Params) => void, .Types
endmodule