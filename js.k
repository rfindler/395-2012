require "modules/substitution.k"

module JS-SYNTAX
  syntax Ref ::= Int

  syntax Func ::= "function" "(" Ids ")" "{" Stmt "}" [binder]

  syntax Value ::= Ref
                 | String
                 | "undefined"
                 | "null"

  syntax VarExpr ::= Id
                   | Id "=" Expr             [strict(2)]

  syntax VarExprs ::= List{VarExpr,","}

  syntax Expr ::= "{" ObjFields "}"          [strict]
                | Expr "[" Expr "]"          [seqstrict]
                | Expr "[" Expr "]" "=" Expr [seqstrict]
                | Id
                | Func
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
  
  syntax Refs ::= List{Ref,","}

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

  syntax ScopeObj ::= Scope(Value, Map)

  syntax Lambda ::= "FBegin" Ids "**" Values "**" Stmt  [strict(2)]

  syntax ValObjField ::= String ":" Value
                       | GetterSetter
                 
  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"
  syntax ObjMap ::= Obj(Map, MaybeFuncObj)

  // Used to map strings to <getter> ** <setter> pairs
  syntax AccessPair ::= MaybeFun "**" MaybeFun 
  syntax MaybeFun ::= Func
                    | "nofun"

  syntax SetterReturn ::= SetRet(Value)

  syntax RetValue ::= Ref
                    | String
                    
  syntax FuncObj ::= FuncObj(Ref, Ids, Stmt)
  
  syntax MaybeFuncObj ::= FuncObj
                        | "nofunobj"
  
  syntax CurEnv  ::= env(Ref)

  syntax K ::= ObjMap | SetterReturn | "FEnd" Rs
  syntax KResult ::= Value | ValObjField | RetValue | "return" RetValue 

  configuration <C>
                    <k> $PGM:Stmt </k>
                    <Stage> .Map </Stage>
                    <ObjStore> .Map </ObjStore>
                    <Scope> 0, .Refs </Scope>
                    <R> 1 </R>
                    <in stream="stdin"> .List </in>
                    <out stream="stdout"> .List </out>
                </C>

  rule undefined ; S:Stmt => S [structural]

  /**
   * Objects
   */

  rule [Object-Intro]:
       <k> ( { O:ValObjFields } => O ~> Obj(M, nofunobj) ) ... </k>
       <Stage> M:Map => .Map </Stage>

  rule [Property-Intro]:
        <k> ( Str:String : V:Value, O:ValObjFields => O) ... </k>
       <Stage> M:Map => M[V / Str] </Stage> 

  rule [Reference-Object]:
       <k> ( .ValObjFields ~> Obj(Old:Map, nofunobj) => R ) ... </k>
       <Stage> M:Map => Old </Stage>
       <ObjStore> OS:Map(. => R |-> Obj(M, nofunobj) ) </ObjStore>
       <R> R:Ref => R +Int 1 </R> 

  rule [E-GetField] :
       <k> ( R:Ref [ Str : String ] => V) ... </k>
       <ObjStore> OS:Map( R |-> Obj(O:Map(Str |-> V:Value), _)) </ObjStore>
       
  rule [E-NoField] :
       <k> ( R:Ref [ Str : String ] => undefined) ... </k>
       <ObjStore> OS:Map( R |-> Obj(O, _)) </ObjStore>
       when notBool(Str in keys(O))

  rule [E-SetField] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map => M[V / Str], _)) </ObjStore>

  /**
   * Getters
   */

  rule [Getter-Update]:
       <k> (get S:String () { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(S |-> (F1 => E) ** F2) </Stage>

  rule [Getter-Intro]:
       <k> (get S:String () { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => S |-> E ** nofun) </Stage>
       when notBool(S in keys(M))

  rule [E-Getter] :
       <k> ( R:Ref [ Str : String ] => E) ... </k>
       <ObjStore> OS:Map(R |-> Obj(O:Map(Str |-> E:Expr ** F, _))) </ObjStore>
  
  /**
   * Setters
   */

  rule [Setter-Update]:
       <k> (set S:String ( I:Id ) { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(S |-> F ** (F1 => ( function (I) { E } ))) </Stage>

  rule [Setter-Intro]:
       <k> (set S:String ( I:Id ) { E:Expr }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => S |-> nofun ** ( function (I) { E } )) </Stage>
       when notBool(S in keys(M)) 
                    
  rule [E-Setter] :
       <k> ( R:Ref [Str:String] = V:Value => F(V) ~> SetRet(V)) ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map (Str |-> F1 ** F:Func), _)) </ObjStore>
  
  rule [Setter-Return]:
       <k> (V:Value ~> SetRet(V2:Value) => V2) ... </k>

  /**
   * Function Application
   */
  rule [E-FuncDefine] :
       <k> ( function (Xs:Ids) { Body:Stmt } => R ) ... </k>
       <ObjStore> OS:Map (. => R |-> Obj(.Map, FuncObj(Env, Xs, Body))) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref</E>
      
  rule [E-ApplyBegin] :
       <k> ( F:Ref(Vs:Values) => FBegin Xs ** Vs ** Body ~> FEnd) ... </k>
       <ObjStore> (. => R |-> Obj((Xs|->Vs), _)) OS:Map ( F |-> Obj(_, FuncObj(Rs:Refs, Xs, Body))) </ObjStore>
       <Scope> R, Rs</Scope>
       <R> R:Ref => R +Int 1 </R>
       
  rule [E-ApplyBindParameters] :
       <k> ( FBegin X:Id, Xs:Ids ** V:Value, Vs:Values ** Body => FBegin Xs ** Vs ** Body ) ... </k>
       <Store> S:Map (. => R |-> V) </Store>
       <Scope> Sc:Map ( Env |-> Scope(_, M:Map => M[R / X]) ) </Scope>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>
       
  rule FBegin .Ids ** .Values ** Body:Stmt => Body
  
  
  //The code below has not been seen in class yet
  
  rule [E-ApplyEnd] :
       <k> ( V:Value ~> FEnd => undefined ) ... </k>
       <Scope> Sc:Map ( Env |-> Scope(ContextEnv:Ref, _) ) </Scope>
       <E> Env:Ref => ContextEnv </E>
       
  rule [E-ApplyReturn] :
       <k> ( return V:RetValue ~> FEnd => V ) ... </k>
       <Scope> OS:Map ( Env |-> Scope(ContextEnv:Ref, _) ) </Scope>
       <E> Env:Ref => ContextEnv </E>
       
  rule return V:RetValue ; S:Stmt => return V
   
  /**
   * Printing
   */

  rule [Printing] :
       <k> ( print ( V:Value ) => undefined) ... </k>
       <out> ... (. => ListItem(V)) </out>

  /**
   * Variables
   */

  rule [Declare]:
       <k> var (X:Id, Xs:VarExprs => Xs) ... </k>
       <Store> S:Map (. => R |-> undefined) </Store>
       <Scope> Sc:Map ( Env |-> Scope(_, M:Map => M[R / X]) ) </Scope>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>

  rule [E-Assign]:
       <k> var (X:Id = V:Value, Xs:VarExprs => Xs) ... </k>
       <Store> S:Map (. => R |-> V) </Store>
       <Scope> Sc:Map ( Env |-> Scope(_, M:Map => M[R / X]) ) </Scope>
       <R> R:Ref => R +Int 1 </R>
       <E> Env:Ref </E>

  rule var .VarExprs => undefined [structural]

  rule [E-Loc]:
       <k> ( X:Id => V ) ... </k>
       <Store> S:Map (R |-> V:Value) </Store>
       <Scope> Sc:Map ( Env |-> Scope(_, M:Map(X |-> R:Ref)) ) </Scope>
       <E> Env:Ref </E>

  rule [E-LocChainBegin]:
       <k> ( X:Id => X ~> env(Env) ) ... </k>
       <Scope> Sc:Map ( Env |-> Scope(Parent:Ref, M:Map) ) </Scope>
       <E> Env:Ref => Parent</E>
       when notBool(X in keys(M))
       
  rule [E-LocChainEnd]:
       <k> ( V:Value ~> env(Parent) =>  V) ... </k>
       <E> Env:Ref => Parent:Ref</E>
endmodule
