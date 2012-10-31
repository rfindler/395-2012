module JS-SYNTAX
  syntax Value ::= R
                 | String
                 | "undefined"
  syntax R ::= Int

  syntax Expr ::= Value
                | "{" ObjFields "}"
                | Expr "[" Expr "]"
                | Expr "[" Expr "]" "=" Expr

  syntax ObjFields ::= List{ObjField,","}
  syntax ObjField ::= String ":" Expr

endmodule

module JS
  imports JS-SYNTAX

  syntax ValObjField ::= String ":" Value
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"

  configuration <C> <K> $PGM:Expr </K> 
                    <S> .Map      </S>
                    <R> 0         </R>
                </C>

  // Broken ? Any guesses why?
  //rule <K> (ValObj => R) ...</K>
  //     <S> S:Map(. => R |-> ValObj) </S>
  //     <R> R:Int => R +Int 1 </R>

endmodule
