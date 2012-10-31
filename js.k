module JS-SYNTAX
  syntax Value ::= Ref
                 | String
                 | "undefined"
  syntax Ref ::= Int

  syntax Expr ::= Value
                | "{" ObjFields "}"
                | Expr "[" Expr "]"
                | Expr "[" Expr "]" "=" Expr

  syntax ObjFields ::= List{ObjField,","}
  syntax ObjField ::= String ">" Expr

endmodule

module JS
  imports JS-SYNTAX

  syntax ValObjField ::= String ">" Value
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"

  configuration <C> <K> $PGM:Expr </K> 
                    <S> .Map      </S>
                    <R> 0         </R>
                </C>


  /* Basic rules for records (\S4 of \cite{Gibbs2012})*/
  rule [E-Object] : <K> (V:ValObj => R:Ref) ...</K>
                    <S> S:Map(. => R:Ref |-> V) </S>
                    <R> R:Ref => R +Int 1 </R>

  // Doesn't Work
  //rule [E-GetField] : <K> ( R:Ref [ Str:String ] => 1 ) ... </K>
  //                    <S> S:Map(R |-> { ... Str > V:Value ... }) </S>

  

endmodule
