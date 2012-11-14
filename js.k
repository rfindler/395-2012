require "modules/substitution.k"

module JS-SYNTAX
  syntax Ref ::= Int

  syntax Func ::= "function" "(" Ids ")" "{" Stmt "}" [binder]

  syntax Value ::= Ref
                 | String
                 | "undefined"
                 | "null"
                 > Func

  syntax VarExpr ::= Id
                   | Id "=" Expr             [strict(2)]

  syntax VarExprs ::= List{VarExpr,","}

  syntax Expr ::= "{" ObjFields "}"          [strict]
                | Expr "[" Expr "]"          [seqstrict]
                | Expr "[" Expr "]" "=" Expr [seqstrict]
                | Id
                | Expr "(" Values ")"        [seqstrict, left]
                | "print" "(" Expr ")"       [strict]
                > "(" Expr ")"               [bracket]
                | Value

  syntax Stmt ::= Expr
                | "var" VarExprs
                | "return" Expr              [strict]
                > Stmt ";" Stmt              [strict(1), left]

  syntax Ids ::= List{Id,","}                [strict]
  syntax Values ::= List{Value,","}          [strict]

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

  syntax Lambda ::= "FBegin" Ids "**" Values "**" Stmt [strict(2)]

  syntax ValObjField ::= String ":" Value
                       | GetterSetter
                 
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"
  syntax ObjMap ::= Obj(Map)

  // Used to map strings to <getter> ** <setter> pairs
  syntax AccessPair ::= MaybeFun "**" MaybeFun 
  syntax MaybeFun ::= Func
                    | "nofun"

  syntax SetterReturn ::= SetRet(Value)

  syntax RetValue ::= Ref
                    | String
                    > Func

  syntax K ::= ObjMap | SetterReturn | "FEnd"
  syntax KResult ::= Value | ValObjField | RetValue | "return" RetValue

  configuration <C>
                    <k> $PGM:Stmt </k>
                    <Stage> .Map </Stage>
                    <E> 0 </E>
                    <Store> .Map </Store>
                    <ObjStore> Map( 0 |-> Scope(null, .Map) ) </ObjStore>
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
       <ObjStore> OS:Map(. => R |-> Obj(M) ) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
                    
  rule [E-GetField] :
       <k> ( R:Ref [ Str : String ] => V) ... </k>
       <ObjStore> OS:Map( R |-> Obj(O:Map(Str |-> V:Value))) </ObjStore>
       
  rule [E-NoField] :
       <k> ( R:Ref [ Str : String ] => undefined) ... </k>
       <ObjStore> OS:Map( R |-> Obj(O)) </ObjStore>
       when notBool(Str in keys(O))

  rule [E-Getter] :
       <k> ( R:Ref [ Str : String ] => E) ... </k>
       <ObjStore> OS:Map(R |-> Obj(O:Map(Str |-> E:Expr ** F))) </ObjStore>
 
  rule [E-SetField] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map => M[V / Str])) </ObjStore>

  rule [E-Setter] :
       <k> ( R:Ref [Str:String] = V:Value => F(V) ~> SetRet(V)) ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map (Str |-> F1 ** F:Func))) </ObjStore>
  
  rule [Setter-Return]:
       <k> (V:Value ~> SetRet(V2:Value) => V2) ... </k>

  rule [E-ApplyBegin] :
       <k> ( (function (Xs:Ids) { Body:Stmt })(Vs:Values) => FBegin Xs ** Vs ** Body ~> FEnd ) ... </k>
       <ObjStore> OS:Map (. => R |-> Scope(Env, .Map)) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref => R </E>

  rule [E-ApplyBindParameters] :
       <k> ( FBegin X:Id, Xs:Ids ** V:Value, Vs:Values ** Body => FBegin Xs ** Vs ** Body ) ... </k>
       <Store> S:Map (. => R |-> V) </Store>
       <ObjStore> OS:Map ( Env |-> Scope(_, M:Map => M[R / X]) ) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>

  rule FBegin .Ids ** .Values ** Body:Stmt => Body

  rule [E-ApplyEnd] :
       <k> ( V:Value ~> FEnd => undefined ) ... </k>
       <ObjStore> OS:Map ( Env |-> Scope(ContextEnv:Ref, _) ) </ObjStore>
       <E> Env:Ref => ContextEnv </E>

  rule [E-ApplyReturn] :
       <k> ( return V:RetValue ~> FEnd => V ) ... </k>
       <ObjStore> OS:Map ( Env |-> Scope(ContextEnv:Ref, _) ) </ObjStore>
       <E> Env:Ref => ContextEnv </E>

  rule return V:RetValue ; S:Stmt => return V

  rule [Printing] :
       <k> ( print ( V:Value ) => undefined) ... </k>
       <out> ... (. => ListItem(V)) </out>

  rule [Declare]:
       <k> var (X:Id, Xs:VarExprs => Xs) ... </k>
       <Store> S:Map (. => R |-> undefined) </Store>
       <ObjStore> OS:Map ( Env |-> Scope(_, M:Map => M[R / X]) ) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>

  rule [E-Assign]:
       <k> var (X:Id = V:Value, Xs:VarExprs => Xs) ... </k>
       <Store> S:Map (. => R |-> V) </Store>
       <ObjStore> OS:Map ( Env |-> Scope(_, M:Map => M[R / X]) ) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>

  rule var .VarExprs => undefined [structural]

  rule [E-Loc]:
       <k> ( X:Id => V ) ... </k>
       <Store> S:Map (R |-> V:Value) </Store>
       <ObjStore> OS:Map ( Env |-> Scope(_, M:Map(X |-> R:Ref)) ) </ObjStore>
       <E> Env:Ref </E>

  rule undefined ; S:Stmt => S [structural]

endmodule
