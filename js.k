require "modules/substitution.k"

module JS-SYNTAX
  syntax Value ::= Ref
                 | String
                 | "undefined"
		 > "lamb" Id "." Expr        [binder]

  syntax Ref ::= Int

  syntax Expr ::= "(" Expr ")"               [bracket]
                | "{" ObjFields "}"          [strict]
                | Expr "[" Expr "]"          [seqstrict]
                | Expr "[" Expr "]" "=" Expr [seqstrict]
		| Id
		| Expr Expr                  [seqstrict, left]
                > Value

  syntax ObjFields ::= List{ObjField,","}    [seqstrict]
  syntax ObjField ::= String ":" Expr        [strict(2)]

endmodule

module JS
  imports JS-SYNTAX
  imports SUBSTITUTION

  syntax ValObjField ::= String ":" Value
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"
  syntax ObjMap ::= Obj(Map)
  
  
  syntax K ::= ObjMap
  syntax KResult ::= Value | ValObjField

  configuration <C> <k> $PGM:Expr </k> 
                    <Stage> .Map </Stage>
                    <S> .Map      </S>
                    <R> 0         </R>
                </C>

  rule <k> ( { O:ValObjFields } => O ~> Obj(M) ) ...</k>
       <Stage> M:Map => .Map </Stage>

  rule <k> ( Str:String : V:Value, O:ValObjFields => O) ...</k>
       <Stage> M:Map=>M[V / Str] </Stage>

  rule <k> ( .ValObjFields ~> Obj(Old:Map) => R ) ...</k>
       <Stage> M:Map => Old </Stage>
       <S> S:Map(. => R |-> Obj(M) ) </S>
       <R> R:Ref => R +Int 1 </R>
		
  rule [E-GetField] :
       <k> ( R:Ref [ Str : String ] => V) ... </k>
       <S> S:Map( R |-> Obj(O:Map(Str |-> V:Value))) </S>
       
  rule [E-NoField] :
       <k> ( R:Ref [ Str : String ] => undefined) ... </k>
       <S> S:Map( R |-> Obj(O)) </S>
       when notBool(Str in keys(O))

  rule [E-SetField] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ...</k>
       <S> S:Map(R |-> Obj(M:Map=>M[V / Str])) </S>

  rule [App] : 
       (lamb X:Id . E:Expr) V:Value  => E[V / X]
  
endmodule
