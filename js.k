require "modules/substitution.k"

module JS-SYNTAX
  syntax Value ::= Ref
                 | String
                 | "undefined"
                 | "null"
                 > Lambda
  syntax Lambda ::= "function" "(" Id ")" "{" Stmt "}" [binder]

  syntax Ref ::= Int

  syntax Expr ::= "(" Expr ")"               [bracket]
                | "{" ObjFields "}"          [strict]
                | Expr "[" Expr "]"          [seqstrict]
                | Expr "[" Expr "]" "=" Expr [seqstrict]
                | Id
                | Expr "(" Expr ")"          [seqstrict, left]
                | "print" "(" Expr ")"       [strict]
                | "var" Id "=" Expr          [strict(2)]
                > Value

  syntax Stmt ::= Expr
                > Stmt ";" Stmt              [strict(1), left]

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

  syntax ScopeObj ::= Scope(Value, Map) [strict(1)]

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
                
  syntax K ::= ObjMap | SetterReturn | "FEnd"
  syntax KResult ::= Value | ValObjField

  configuration <C>
                    <k> $PGM:Stmt </k>
                    <Stage> .Map </Stage>
                    <E> 0 </E>
                    <S> Map( 0 |-> Scope(null, .Map) ) </S>
                    <R> 1 </R>
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
       <Stage> M:Map(S |-> F ** (F1 => ( function (I) { E } ))) </Stage>

  rule [Setter-Intro]:
       <k> (set S:String ( I:Id ) { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => S |-> nofun ** ( function (I) { E } )) </Stage>
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
       <S> S:Map(R |-> Obj(M:Map => M[V / Str])) </S>

  rule [E-Setter] :
       <k> ( R:Ref [Str:String] = V:Value => F(V) ~> SetRet(V)) ... </k>
       <S> S:Map(R |-> Obj(M:Map (Str |-> F1 ** F:Lambda))) </S>
  
  rule [Setter-Return]:
       <k> (V:Value ~> SetRet(V2:Value) => V2) ... </k>

  rule [E-Apply] :
       <k> ( (function (I:Id) { Body:Stmt })(V:Value) => var I = V; Body ~> FEnd ) ... </k>
       <S> S:Map (. => R |-> Scope(Env, .Map)) </S>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref => R </E>

  rule [E-ApplyEnd] :
       <k> ( V:Value ~> FEnd => undefined ) ... </k>
       <S> S:Map ( Env |-> Scope(ContextEnv:Ref, _) ) </S>
       <E> Env:Ref => ContextEnv </E>

  rule [Printing] :
       <k> ( print ( V:Value ) => undefined) ... </k>
       <out> ... (. => ListItem(V)) </out>

  rule [E-Assign]:
       <k> ( var I:Id = V:Value => undefined ) ... </k>
       <S> S:Map ( Env |-> Scope(_, M:Map => M[R / I]) ) (. => R |-> V) </S>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>

  rule [E-Loc]:
       <k> ( I:Id => V ) ... </k>
       <S> S:Map( Env |-> Scope(_, M:Map(I |-> R:Ref)) ) (R |-> V:Value) </S>
       <E> Env:Ref </E>

  rule undefined ; S:Stmt => S [structural]

endmodule
