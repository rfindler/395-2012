require "modules/substitution.k"

module JS-SYNTAX
  syntax Value ::= Ref
                 | String
                 | "undefined"
                 > Lambda
  syntax Lambda ::= "lamb" Id "." Expr       [binder]

  syntax Ref ::= Int

  syntax Expr ::= "(" Expr ")"               [bracket]
                | "{" ObjFields "}"          [strict]
                | Expr "[" Expr "]"          [seqstrict]
                | Expr "[" Expr "]" "=" Expr [seqstrict]
                | Id
                | Expr Expr                  [seqstrict, left]
                | "print" "(" Expr ")"       [strict]
                > Value

  syntax ObjFields ::= List{ObjField,","}    [seqstrict]
  syntax ObjField ::= String ":" Expr        [strict(2)]
                    | GetterSetter

  syntax GetterSetter ::= Getter | Setter
  syntax Getter ::= "get" String "()" "{" Expr "}"         [strict(2)]
  syntax Setter ::= "set" String "(" Id ")" "{" Expr "}"   [strict(2)]

endmodule

module JS
  imports JS-SYNTAX
  imports SUBSTITUTION

  syntax ValObjField ::= String ":" Value
                       | GetterSetter
                 
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"
  syntax ObjMap ::= Obj(Map)
  
  // Used to map strings to <getter> ** <setter> pairs
  syntax AccessPair ::= MaybeFun "**" MaybeFun 
  syntax MaybeFun ::= Lambda
                    | "nofun"

  syntax SetterReturn ::= SetRet(Value)                                      
                
  syntax K ::= ObjMap | SetterReturn
  syntax KResult ::= Value | ValObjField

  configuration <C>
                    <k> $PGM:Expr </k>
                    <Stage> .Map </Stage>
                    <S> .Map </S>
                    <R> 0 </R>
                    <in stream="stdin"> .List </in>
                    <out stream="stdout"> .List </out>
                </C>

  rule [Object-Intro]:
       <k> ( { O:ValObjFields } => O ~> Obj(M) ) ... </k>
       <Stage> M:Map => .Map </Stage>

  rule [Property-Intro]:
        <k> ( Str:String : V:Value, O:ValObjFields => O) ... </k>
       <Stage> M:Map => M[V / Str] </Stage> 
       
  rule [Getter-Update]:
       <k> (get S:String () { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(S |-> (F1 => E) ** F2) </Stage>

  rule [Getter-Intro]:
       <k> (get S:String () { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => S |-> E ** nofun) </Stage>
       when notBool(S in keys(M))     
  
  rule [Setter-Update]:
       <k> (set S:String ( I:Id ) { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(S |-> F ** (F1 => (lamb I . E))) </Stage>

  rule [Setter-Intro]:
       <k> (set S:String ( I:Id ) { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => S |-> nofun ** (lamb I . E)) </Stage>
       when notBool(S in keys(M)) 
                
  rule [Reference-Object]:
       <k> ( .ValObjFields ~> Obj(Old:Map) => R ) ... </k>
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

  rule [E-Getter] :
       <k> ( R:Ref [ Str : String ] => E) ... </k>
       <S> S:Map(R |-> Obj(O:Map(Str |-> E:Expr ** F))) </S>
 
  rule [E-SetField] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ... </k>
       <S> S:Map(R |-> Obj(M:Map(Str |-> V1:Value ) => M[V / Str])) </S>
  
  rule [E-AddField] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ... </k>
       <S> S:Map(R |-> Obj(M:Map => M[V / Str])) </S>
       when notBool(Str in keys(M))

  rule [E-Setter] :
       <k> ( R:Ref [Str:String] = V:Value => F V ~> SetRet(V)) ... </k>
       <S> S:Map(R |-> Obj(M:Map (Str |-> F1 ** F:Lambda))) </S>
  
  rule [Setter-Return]:
       <k> (V:Value ~> SetRet(V2:Value) => V2) ... </k>

  rule [App] : 
       (lamb X:Id . E:Expr) V:Value  => E[V / X]
  
  rule [Printing] :
       <k> ( print ( V:Value ) => undefined) ... </k>
       <out> ... (. => ListItem(V)) </out>

endmodule
