requires "corolts-syntax.k"
requires "corolts-execute.k"

module COROLTS-ARRAY
  imports COROLTS-SYNTAX
  imports COROLTS-EXECUTE
  imports DOMAINS

  
  // Array declarations
  context new _:Type[HOLE]
  
  rule <k> new T:Type[N:Int] => array(T, L, N) ...</k>
       <store>... .Map => (L) ... (L +Int N -Int 1) |-> undefined ...</store>
       <nextLoc> L => L +Int N </nextLoc> 
    requires N >Int 0


  // Array lookups
  rule V:Val[N1:Int, N2:Int, Vs:Vals] => V[N1][N2, Vs]
    [structural, anywhere]

  rule array(_:Type, L:Int, M:Int)[N:Int] => lookup(L +Int N)
    when N >=Int 0 andBool N <Int M [structural, anywhere]

  rule sizeOf(array(_,_,N)) => N


endmodule