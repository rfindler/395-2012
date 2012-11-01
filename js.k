module JS-SYNTAX
  syntax Value ::= Ref
                 | String
                 | "undefined"
  syntax Ref ::= Int

  syntax Expr ::= Value
                | "{" ObjFields "}"
                | Expr "[" Expr "]" [seqstrict]
                | Expr "[" Expr "]" "=" Expr

  syntax ObjFields ::= List{ObjField,","}
  syntax ObjField ::= String ">" Expr

endmodule

module JS
  imports JS-SYNTAX

  syntax ValObjField ::= String ">" Value
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"
  // ValStage corresponds to a new object that is staged
  syntax ValStage ::= "!-" ValObj
  syntax ObjMap ::= Obj(Map)
  
  
  syntax K ::= ValStage | ObjMap
  syntax KResult ::= Value

  configuration <C> <k> $PGM:Expr </k> 
                    <Stage> .Map </Stage>
                    <S> .Map      </S>
                    <R> 0         </R>
                </C>

  // Infinite loops?
  //rule <k> (V:ValObj => R) ...</k>
  //     <S> S:Map(. => R |-> V) </S>
  //     <R> R:Ref => R +Int 1 </R>
  rule [ValStage-Intro] :
       <k> (V:ValObj => !- V ~> Obj(C)) ... </k>
       <Stage> C:Map => .Map </Stage>
                         
  rule [ValStage-Elim] :
       <k> ( !- { .ValObjFields } ~> Obj(C:Map)
             => R ) ... </k>
       <Stage> O:Map => C </Stage>
       <S> S:Map(. => R |-> Obj(O)) </S>
       <R> R:Ref => R +Int 1 </R>

  // Two cases: Either it is in the Map or it is not
  //// Case 1: Not in the Map
  rule [ValStage-1] :
       <k> (!- { S:String > V:Value, Fields:ValObjFields }
            => !- { Fields:ValObjFields } ) ... </k>
       <Stage> C:Map(. => S |-> V) </Stage>
       when notBool(S in keys(C))

  //// Assuming no duplicates, this should never happen!
  //// Case 2: In the Map
  rule [ValStage-2] :
       <k> (!- {S:String > V:Value, Fields:ValObjFields }
            => !- { Fields:ValObjFields }) ... </k>
       <Stage> C:Map(S |-> _ => S |-> V) </Stage>
 
   //// Case 1: Reference exists, push the Stage and stage the reference's Object map
  rule [E-GetField] :
       <k> ( R:Ref [ Str : String ] => V) ... </k>
       <S> S:Map( R |-> Obj(O:Map(Str |-> V:Value))) </S>
       
  rule [E-NoField] :
       <k> ( R:Ref [ Str : String ] => undefined) ... </k>
       <S> S:Map( R |-> Obj(O)) </S>
       when notBool(Str in keys(O))

  
endmodule
