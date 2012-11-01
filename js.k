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

  // Evaluated Object Records
  syntax ValObjField ::= String ">" Value
  syntax ValObjFields ::= List{ValObjField, ","}
  syntax ValObj ::= "{" ValObjFields "}"
  
  // Machinery For Object Maps 
  syntax ValPair ::= ValObj "/-" Ref
  syntax ObjMap ::= Obj(Map)
  syntax GetField ::= "?" String | "!" Value
  syntax SetField ::= "??" Ref "[" String "]" "=" Value
                    | "!!" Ref Value
  syntax K ::= ValPair | ObjMap | GetField | SetField

  configuration <C color="yello">
                    <K color="green">   $PGM:Expr </K>
                    <Stage color="red"> .Map      </Stage>
                    <S color="purple">  .Map      </S>
                    <R color="orange">   0        </R>
                </C>

  /* Basic rules for records (\S4 of \cite{Gibbs2012}) */

  // This won't work with GetField
  //rule [E-Object] : <K> (V:ValObj => R:Ref) ...</K>
  //                  <S> S:Map(. => R:Ref |-> V) </S>
  //                  <R> R:Ref => R +Int 1 </R>

  /* To create a record, we will want to make a map

     To do this, we need a staging area for constructing
     the map.

     The semantics in \cite{Gibbs2012} underspecify
     what to do when there are duplicate fields.
     I suggest testing in your favorite reference
     javascript console to see what is going on for now.
     
     Ultimately we should dig up what to do in the
     ECMA5 specs. */

  rule [ValPair-Intro] :
       <K> (V:ValObj => V /- R ~> Obj(C)) ... </K>
       <Stage> C:Map => .Map </Stage>
       <R> R:Ref => R +Int 1 </R>
			 
  rule [ValPair-Elim] :
       <K> ( { .ValObjFields } /- R : Ref ~> Obj(C:Map)
       	     => R ) ... </K>
       <Stage> O:Map => C </Stage>
       <S> S:Map(. => R |-> Obj(O)) </S>

  // Two cases: Either it is in the Map or it is not
  //// Case 1: Not in the array
  rule [ValPair-1] :
       <K> ({ S:String > V:Value
            , Fields:ValObjFields } /- R : Ref
	    => { Fields:ValObjFields } /- R) ... </K>
       <Stage> C:Map(. => S |-> V) </Stage>
       when notBool(S in keys(C))

  //// Case 2: Not in the array
  rule [ValPair-2] :
       <K> ({S:String > V:Value
            , Fields:ValObjFields } /- R : Ref
            => { Fields:ValObjFields } /- R) ... </K>
       <Stage> C:Map(S |-> _ => S |-> V) </Stage>

  // GetField
  //// Case 1: Reference DNE
  rule [E-GetField-1] :
       <K> ( R:Ref [ _ ] => undefined ) ... </K>
       <S> S:Map </S>
       when notBool(R in keys(S))

  //// Case 2: Reference exists, push the Stage and stage the reference's Object map
  rule [E-GetField-2] :
       <K> ( R:Ref [ Str : String ]
       	   => ? Str ~> Obj(C) ) ... </K>
       <Stage> C => O </Stage> 
       <S> Store( R |-> Obj(O:Map) ) </S>

  ////// Case 2.1: Reference DNE in staged Object Map
  rule [E-Getfield-2.1] :
        <K> ( ? Str : String  => ! undefined ) ... </K>
	<Stage> O : Map </Stage>
	when notBool(Str in keys(O))
      
  ////// Case 2.2: Reference DNE in staged Object Map
  rule [E-Getfield-2.2] :
       <K> ( ? Str:String  => ! V ) ... </K>
       <Stage> O(Str |-> V:Value) </Stage>

  //// Cleanup : Pop stage and set the K cell to the value
  rule [E-Field-Cleanup] :
       <K> ( ! V ~> Obj(C:Map)  => V ) ... </K>
       <Stage> _ => C </Stage>

  // SetField
  //// Case 0: When reference DNE, throw ReferenceError
  ////         (not handled)
  //// Assume reference exists and stage object map
  rule [E-SetField-Stage] :
       <K> ( R:Ref [ Str:String ] = V:Value
           => ?? R [Str] = V ~> Obj(C) ) ... </K>
       <Stage> C => O </Stage> 
       <S> S( R |-> Obj(O:Map) ) </S>

  //// Stage is set, now do work     
  //// Case 1: Field DNE
  rule [E-SetField-1] :
       <K> ( ? R:Ref [ Str:String ] = V:Value
           => !! R V ) ... </K>
       <Stage> O:Map(. => Str |-> V) </Stage>
       when notBool(Str in keys(O))
       
  //// Case 2: Field Exists
  rule [E-SetField-1] :
       <K> ( ? R:Ref [Str:String] = V:Value
           => !! R V ) ... </K>
       <Stage> O(Str |-> _ => Str |-> V) </Stage>

  /*
   Cleanup :
     (1) Pop old stage back
     (2) Update Reference with changes made
  */
  
  rule [E-SetField-Cleanup] :
       <K> ( !! R V ~> Obj(C:Map) => V ) ... </K>
       <Stage> O:Map => C </Stage>
       <S> S( R |-> _ => R |-> Obj(O) ) </S>
       
endmodule

// TODO: Make a bibliograph with bibtex
