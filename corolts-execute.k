requires "corolts-syntax.k"

module COROLTS-EXECUTE
  imports COROLTS-SYNTAX
  imports DOMAINS

  syntax Val ::= Int | Bool | String
              |  func(Type, Params, Block) // Parameters and body
              |  corodef(Type, Params, Params, Block) // Coroutine definition
              |  coro(Params, Type, Int) // Coroutine Instance (type and id)
              |  ref(Type, Int)
              |  mref(Type, Int)

  syntax Vals ::= List{Val,","} [klabel(exps)]

  syntax Exp ::= Val
  syntax Exps ::= Vals
  syntax KResult ::= Val | Vals

  syntax ControlCell
  syntax ControlCellFragment

  configuration <T color="red">
                  <stacks color="orange">
                    <stack color="yellow" multiplicity="*" type="Map">
                         <id color="pink"> 0 </id>
                         <k color="green"> $PGM:Stmt ~> execute </k>
                         <control color="cyan">
                              <fstack color="blue"> .List </fstack>
                              <returnType color="LimeGreen"> void </returnType>
                         </control>
                         <env color="violet"> .Map </env>
                    </stack>
                  </stacks>
                  <typeEnv color="magenta"> .Map </typeEnv>
                  <genv color="pink"> .Map </genv>
                  <store color="white"> .Map </store>
                  <nextLoc color="gray"> 0 </nextLoc>
                  <input color="magenta" stream="stdin"> .List </input>
                  <output color="brown" stream="stdout"> .List </output>
                  // Rust based semantics
                  <alive color="orange"> .List </alive>  // is x in it's appropriate scope? 
                  <mutable color="orange"> .Map </mutable> // let mut x = 0; is mutable, let x = 0; is immutable
                  <borrow color="yellow"> .Map </borrow>  // is x borrowed somewhere? if so, how?
                  <moved color="purple"> .Map </moved>    // has x been moved completely to somewhere else?
                </T>
  
  syntax KItem ::= "undefined" | "mRef" | "iRef"

  // Variable Declarations
  rule <k> let X:Id : T:Type; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> undefined ...</store>
       <typeEnv>... .Map =>  L |-> T ...</typeEnv>
       <mutable>... .Map => L |-> false ...</mutable>
       <borrow>... .Map => L |-> undefined ...</borrow>
       <moved>... .Map => L |-> false ...</moved>
       <nextLoc> L => L +Int 1 </nextLoc>

  rule <k> let mut X:Id : T:Type; => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> undefined ...</store>
       <typeEnv>... .Map =>  L |-> T ...</typeEnv>
       <mutable>... .Map => L |-> true ...</mutable>
       <borrow>... .Map => L |-> undefined ...</borrow>
       <moved>... .Map => L |-> false ...</moved>
       <nextLoc> L => L +Int 1 </nextLoc>

  // Function Declarations
  rule <k> fn F:Id(Xs:Params) -> T:Type B => . ...</k>
       <env> Env => Env[F <- L] </env>
       <store>... .Map => L |-> func(T, Xs, B) ...</store>
       <nextLoc> L => L +Int 1 </nextLoc>

  rule <k> cr C:Id(Xs:Params)(Ys:Params) -> T:Type B => . ...</k>
       <env> Env => Env[C <- L] </env>
       <store>... .Map => L |-> corodef(T, Xs, Ys, B) ...</store>
       <nextLoc> L => L +Int 1 </nextLoc>

  syntax KItem ::= "execute"  
  rule <k> execute => call main(.Exps); </k>
       <env> Env </env>
       <genv> .Map => Env </genv>  [structural]

  // Reads are legal, as long as the value is 
  rule <k> X:Id => V ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [lookup]

  context ++(HOLE => lvalue(HOLE))
  rule <k> ++loc(L) => I +Int 1 ...</k>
       <store>... L |-> (I => I +Int 1) ...</store>  [increment]

  context &(HOLE => lvalue(HOLE))
  rule <k> &loc(L) => ref(T, L) ...</k> 
       <borrow>... L |-> (Q => iRef) ...</borrow>   
       <typeEnv>... L |-> T:Type ...</typeEnv> 
       when Q =/=K mRef  [reference]

  context &mut(HOLE => lvalue(HOLE))
  rule <k> &mut loc(L) => mref(T, L) ...</k> 
       <mutable>... L |-> true ...</mutable>
       <borrow>... L |-> (Q => mRef) ...</borrow>   
       <typeEnv>... L |-> T:Type ...</typeEnv>   
       when Q ==K undefined [reference]

  rule <k> *ref(T, L) => V ...</k> 
       <store>... L |-> V:Val ...</store>   
       when T ==K typeOf(V) [dereference]

  rule <k> *mref(T, L) => V ...</k> 
       <store>... L |-> V:Val ...</store>   
       when T ==K typeOf(V) [dereference]

  // Function Calls
  syntax KItem ::= (Type,Map,K,ControlCellFragment)

  // Build function call, switch over evaluation of arguments
  rule <k> call func(T,Xs,S)(Vs:Vals) ~> K => mkDecls(Xs,Vs) S return; </k>
       <control>
         <fstack> .List => ListItem((T',Env,K,C)) ...</fstack>
         <returnType> T' => T </returnType>
         C
       </control>
       <env> Env => GEnv </env>
       <genv> GEnv </genv>

  syntax KItem ::= "wait"
  rule <stack>... 
          <k> start corodef(Y:Type, Xs:Params, Ys:Params, B)(Ps:Vals) => coro(Ys, Y, !T +Int 1) ...</k> ...</stack> <genv> GEnv </genv>
          (.Bag => <stack>...
                    <k> mkDecls(Xs,Ps) ~> crDecls(Ys) ~> wait ~> B </k>
                    <env> GEnv </env>
                    <id> !T +Int 1 </id>
                    ...</stack>)

  rule <stack>...
          <k> call coro(Ys, _, T:Int)(Vs:Vals) ...</k> 
        ...</stack>
       <stack>...
           <k> wait => mtDecls(Ys,Vs) ...</k>
           <id> T </id>
       ...</stack>
  
  rule <stack>...
          <k> call coro(_, Y, T:Int)(Vs:Vals) => V ...</k> 
        ...</stack>
       <stack>...
           <k> yield V:Val; ~> K => wait ~> K </k>
           <id> T </id>
       ...</stack>
     when typeOf(V) ==K Y

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
       <typeEnv>... L |-> T:Type ...</typeEnv>
       <mutable>... L |-> true ...</mutable>
       <store>... L |-> (_ => V) ...</store> 
       when T ==K typeOf(V) [assignment]

  rule <k> loc(L) = V:Val => V ...</k> 
       <typeEnv>... L |-> T:Type ...</typeEnv>
       <store>... L |-> (undefined => V) ...</store> 
       when T ==K typeOf(V) [assignment]


  // Scoping (restore's environment after call)
  rule {} => .  [structural]
  rule <k> { S } => S ~> setEnv(Env) ...</k>  <env> Env </env>  [structural]

  rule S1:Stmt S2:Stmt => S1 ~> S2  [structural]
  rule _:Val; => .


  rule <k> println!(V:Val, Es => Es); ...</k> 
       <output>... .List => ListItem(V) </output> 
       when typeOf(V) ==K int orBool typeOf(V) ==K string [print]

  rule <k> println!(.Vals); => . ...</k> 
       <output>... .List => ListItem("\n") </output> [print]

  // Helper functions
  syntax Stmt ::= mkDecls(Params,Vals)  [function]
  rule mkDecls((X:Id : T:Type, Xs:Params), (V:Val, Vs:Vals)) => let X : T = V; mkDecls(Xs,Vs)
  rule mkDecls(.Params,.Vals) => {}

  syntax Stmt ::= crDecls(Params)  [function]
  rule crDecls(X:Id : T:Type, Xs:Params) => let mut X : T; crDecls(Xs)
  rule crDecls(.Params) => {}

  syntax Stmt ::= mtDecls(Params,Vals)  [function]
  rule mtDecls((X:Id : T:Type, Xs:Params), (V:Val, Vs:Vals)) => X = V; mtDecls(Xs,Vs)
  rule mtDecls(.Params,.Vals) => {}

  syntax KItem ::= setEnv(Map)
  rule <k> setEnv(Env) => . ...</k> <env> _ => Env </env>  [structural]
  rule (setEnv(_) => .) ~> setEnv(_)  [structural]

  // lookup value and get locations
  syntax Exp ::= lvalue(K)
  syntax Val ::= loc(Int)

  rule <k> lvalue(X:Id => loc(L)) ...</k> 
       <env>... X |-> L:Int ...</env>           
       <borrow>... L |-> Q ...</borrow>
       when Q =/=K mRef                [structural]


  rule <k> lvalue(*X:Id => loc(L2)) ...</k> 
       <env>... X |-> L:Int ...</env> 
       <store>... L |-> mref(_, L2) ...</store>  
       <borrow>... L2 |-> mRef </borrow>  [structural]

  // Map Operations
  syntax Map ::= Int "..." Int "|->" K [function]
  rule N...M |-> _ => .Map  requires N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  requires N <=Int M

  // Type of a value.
  syntax Type ::= typeOf(K)  [function]
  rule typeOf(_:Int) => int
  rule typeOf(_:Bool) => bool
  rule typeOf(_:String) => string
  rule typeOf(func(T,Ps,_)) => fn getTypes(Ps) -> T
  rule typeOf(corodef(T,Xs,Ys, _)) => cr getTypes(Xs) -> getTypes(Ys) -> T
  rule typeOf(coro(Xs, T, _)) => ct getTypes(Xs) ->> T
  rule typeOf(ref(T, _)) => &T
  rule typeOf(mref(T, _)) => &mut T
  rule typeOf(nothing(T)) => T

  syntax Types ::= getTypes(Params)  [function]
  rule getTypes(_:Id : T:Type) => T, .Types   // I would like to not use .Types
  rule getTypes(_:Id : T:Type, P, Ps) => T, getTypes(P,Ps)
  rule getTypes(.Params) => void, .Types
endmodule