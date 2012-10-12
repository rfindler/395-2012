/*

The K cell contains a sequence of:

   Pgm
   Stmt
   AExp
   BExp
   KResult

separated by ~>

*/

module IMP
  syntax AExp ::= Int 
                | "(" AExp ")"    [bracket]
                > AExp "*" AExp   [seqstrict, left]
                | AExp "/" AExp   [seqstrict, left]
                > AExp "+" AExp   [seqstrict, left]
  syntax BExp ::= Bool
                | "(" BExp ")"    [bracket]
                | AExp "<=" AExp  [seqstrict]
                > BExp "and" BExp [strict(2)]
                | "not" BExp      [strict]
  syntax Stmt ::= StmtVal
                | Id ":=" AExp    [strict(2)]
                | "if" BExp "then" Stmt "else" Stmt  [strict(1)]
                | "while" BExp "do" Stmt
                > Stmt ";" Stmt   [seqstrict, left]
                | "(" Stmt ")"    [bracket]
  syntax StmtVal ::= "skip"

  syntax Pgm ::= "var" Ids ";" Stmt
  syntax Ids ::= List{Id,","} 

  syntax KResult ::= Bool | Int | StmtVal
         	 
  configuration <T color="yellow">
                  <k color="green"> $PGM:K </k>
                  <state color="red"> .Map </state>
                </T>

  rule <k> var (X:Id, Xs:Ids => Xs) ; _ </k>
       <state> Rho:Map (. => X |-> 0) </state>

  rule <k> var .Ids ; S:Stmt => S </k>

  rule <k> (X:Id := I1:Int => skip) ... </k>
       <state> Rho:Map (X |-> I2 => X |-> I1) </state>

  rule I1:Int + I2:Int => I1 +Int I2 
  rule I1:Int * I2:Int => I1 *Int I2 
  rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0
  rule <k> I1:Int / 0 ...</k> => <k> "div0" </k>

  rule I1:Int <= I2:Int => I1 <=Int I2        

  rule if true then S:Stmt else _ => S
  rule if false then _ else S:Stmt => S

  rule not false => true
  rule not true => false

  rule true and B:BExp => B

endmodule