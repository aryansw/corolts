module LAMBDA

  syntax Fruit ::= Banana() | Blueberry() | Apple() | Pear()
  syntax Container ::= Jar(Fruit)

  syntax Fruit ::= contentsOfJar(Container) [function]

  rule contentsOfJar(Jar(F)) => F

endmodule