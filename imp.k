module IMP
  syntax AExp ::= Int | Id | String
                | "(" AExp ")"    [bracket]
                > AExp "*" AExp   [seqstrict, left]
                | AExp "/" AExp   [seqstrict, left]
                > AExp "+" AExp   [seqstrict, left]
                | AExp "-" AExp   [seqstrict, left]
                | "read" "()"
  syntax BExp ::= Bool
                | "(" BExp ")"    [bracket]
                | AExp "<=" AExp  [seqstrict]
                | AExp ">=" AExp 
                > BExp "and" BExp [strict(1)]
                | "not" BExp      [strict]
  syntax Stmt ::= StmtVal
                | "print" "(" AExps ")"   [strict]
                | Id ":=" AExp            [strict(2)]
                | "if" BExp "then" Stmt "else" Stmt  [strict(1)]
                | "while" BExp "do" Stmt
                > "var" Ids
                > "spawn" Stmt
                > Stmt ";" Stmt           [strict(1), left]
                | "(" Stmt ")"            [bracket]
                | "{" Stmt "}"
  syntax StmtVal ::= "skip"

  syntax Ids ::= List{Id,","}          [strict]
  syntax AExps ::= List{AExp,","}      [strict]

  syntax KResult ::= Bool | Int | String | StmtVal
         	 
  configuration <C color="yellow">
                  <threads>
                    <thread multiplicity="*">
                      <k color="green"> $PGM:K </k>
                    </thread>
                  </threads>
                  <env color="red"> .Map </env>
                  <store color="red"> .Map </store>
                  <allocptr color="cyan"> 0 </allocptr>
                  <in color="magenta" stream="stdin">  .List </in>
                  <out color="Orchid" stream="stdout"> .List </out>
                </C>

  rule <k> var (X:Id, Xs:Ids => Xs) ... </k>
       <env> Env (. => X |-> N) </env>
       <store> Store:Map (. => N |-> 0) </store>
       <allocptr> (N => N +Int 1) </allocptr>
       when notBool(X in keys(Env))

  rule <k> var (X:Id, Xs:Ids => Xs) ... </k>
       <env> Env (X |-> N2 => X |-> N) </env>
       <store> Store:Map (. => N |-> 0) </store>
       <allocptr> (N => N +Int 1) </allocptr>

  rule var .Ids => skip     [structural]

  rule <k> (X:Id := I1:Int => skip) ... </k>
       <env> Env:Map (X |-> N:Int) </env>
       <store> Store:Map (N |-> I2 => N |-> I1) </store>

  rule I1:Int + I2:Int => I1 +Int I2 
  rule I1:String + I2:String => I1 +String I2 
  rule I1:Int - I2:Int => I1 -Int I2 
  rule I1:Int * I2:Int => I1 *Int I2 
  rule I1:Int / I2:Int => I1 /Int I2 when I2 =/=Int 0
  rule <k> I1:Int / 0 ...</k> => <k> "div0" </k>

  rule I1:Int <= I2:Int => I1 <=Int I2        
  rule A:AExp >= B:AExp => B <= A         [structural]

  rule if true then S:Stmt else _ => S
  rule if false then _ else S:Stmt => S

  rule not false => true
  rule not true => false

  rule true and B:BExp => B
  rule false and B:BExp => false

  rule skip ; S:Stmt => S    [structural]

  rule while B:BExp do S:Stmt =>
       if B then (S ; while B do S) else skip

  rule <k> (X:Id=>I) ...</k>
       <env> Env(X |-> N) </env>
       <store> Store(N |-> I) </env>

  syntax Printable ::= Int | String
  rule <k> print(P:Printable,AEs:AExps => AEs) ...</k>
       <out>... (. => ListItem(P)) </out>
  rule print(.AExps) => skip

  rule <k> (read() => I:Int) ...</k>       
       <in> (ListItem(I) => .) ... </in>

  syntax K ::= "env" "(" Map ")"

  rule <k> {S:Stmt} => S ~> env(Rho) ...</k>
       <env> Rho:Map </env>

  rule <k> (skip ~> env(Rho:Map) => skip) ...</k>
       <env> Rho2:Map => Rho </env>

  rule <k> (spawn S => skip)  ...</k>
       . => <thread>... <k> S </k> ...</thread>

endmodule

