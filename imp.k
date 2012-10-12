module IMP
  syntax AExp ::= Int 
                | "(" AExp ")"   [bracket]
                > AExp "*" AExp  [seqstrict, left]
                | AExp "/" AExp  [seqstrict, left]
                > AExp "+" AExp  [seqstrict, left]
  syntax Stmt ::= StmtVal
                | Id ":=" AExp
                | Stmt ";" Stmt  [seqstrict, left]
  syntax StmtVal ::= "skip"

  syntax Pgm ::= "var" Ids ";" Stmt
  syntax Ids ::= List{Id,","} 

  syntax KResult ::= Int | StmtVal
         
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
  rule <k> I1:Int / 0 ...</k> => <k> 123456789</k>
        


endmodule