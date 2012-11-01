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
  syntax ValObjFields ::= List{ValObjField, ","}
  syntax ValObj ::= "{" ValObjFields "}"
  syntax ValPair ::= ValObj "/-" Ref
  syntax ObjMap ::= Obj(Map)
  syntax K ::= ValPair | ObjMap

  configuration <C> <K>     $PGM:Expr </K>
                    <Cache> .Map      </Cache>
                    <S>     .Map      </S>
                    <R>     0         </R>
                </C>

  /* Basic rules for records (\S4 of \cite{Gibbs2012}) */
  // This won't work with GetField

  //rule [E-Object] : <K> (V:ValObj => R:Ref) ...</K>
  //                  <S> S:Map(. => R:Ref |-> V) </S>
  //                  <R> R:Ref => R +Int 1 </R>

  rule [ValPair-Intro] : <K>      (V:ValObj => V /- R ~> Obj(C)) ... </K>
                         <Cache>  C:Map => .Map </Cache>
			 <R>      R:Ref => R +Int 1 </R>
			 
  rule [ValPair-Elim] : <K> ( { .ValObjFields } /- R : Ref ~> Obj(C:Map)
                              => R ) ... </K>
                        <Cache> O:Map => C </Cache>
			<S> S:Map(. => R |-> Obj(O)) </S>

  // Two cases: Either it is in the Map or it is not
  //// Case A: Not in the array
  rule [ValPair-A] : <K> ({S:String > V:Value ,
                           Fields:ValObjFields } /- R : Ref =>
			  { Fields:ValObjFields } /- R) ... </K>
	             <Cache> C:Map(. => S |-> V) </Cache>
                      when notBool(S in keys(C))

  //// Case B: Not in the array
  rule [ValPair-B] : <K> ({S:String > V:Value ,
                           Fields:ValObjFields } /- R : Ref =>
			  { Fields:ValObjFields } /- R) ... </K>
	             <Cache> C:Map(S |-> _ => S |-> V) </Cache>
  
 
  // Doesn't Work
  rule [E-GetField] : <K> ( R:Ref [ Str:String ] => V ) ... </K>
                      <S> S:Map(R |-> O( Str |-> V:Value) )</S>


endmodule

// TODO: Make a bibliograph with bibtex
