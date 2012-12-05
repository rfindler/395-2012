module JS-SYNTAX
  syntax Block ::= "var" Ids ";" Stmt

  syntax Stmt ::= Expr   		     
                | "return" Expr                        [strict]
                > Stmt ";" Stmt                        [strict(1), left]

  syntax Expr ::= "{" ObjFields "}"                    [strict]
                | Expr "[" Expr "]"                    [seqstrict]
                | Expr "[" Expr "]" "=" Expr           [seqstrict]
                | Id "=" Expr		               [strict(2)]
                | Id			     
                | "function" "(" Ids ")" "{" Block "}"
                | Expr "(" Exprs ")"                   [seqstrict, left]
                | "print" "(" Expr ")"                 [strict]
                > "(" Expr ")"                         [bracket]
                | Value
                | "with" "(" Expr ")" "{" Stmt "}"     [strict(1)] 
  syntax Exprs ::= List{Expr,","}                      [seqstrict]
  
  syntax Value ::= Ref
                 | String
                 | "undefined"
                 | "null"
  syntax Values ::= List{Value,","}

  syntax Ref  ::= Int
  syntax Refs ::= List{Ref,","}

  syntax Ids ::= List{Id,","}

  syntax ObjFields ::= List{ObjField,","}              [seqstrict]
  syntax ObjField  ::= String ":" Expr                 [strict(2)]
                     | Getter
                     | Setter

  syntax Getter ::= "get" Id "(" ")" "{" Block "}"
  syntax Setter ::= "set" Id "(" Id ")" "{" Block "}"

endmodule

module JS
  imports JS-SYNTAX

  syntax ValObjField ::= Getter
                       | Setter
  rule isValObjField('_:_(K1:String,,K2:Value)) => true 

  syntax ValObjFields ::= List{ValObjField,","}
  syntax ValObj ::= "{" ValObjFields "}"
  syntax ObjMap ::= Obj(Map, MaybeFuncObj)

  syntax MaybeFuncObj ::= FuncObj
                        | "nofunobj"

  syntax FuncObj ::= FuncObj(Refs, Ids, Block)
  
  syntax AccessPair ::= MaybeFuncObj "**" MaybeFuncObj

  syntax K ::= ObjMap | SetRet(Value) | FEnd(Refs) | "PopScope"
		| Sets(Refs, String, Value)
		| Gets(Refs, String)

  syntax KResult ::= Value | ValObjField | FuncObj 
  rule isKResult(return _:Value) => true

  syntax Expr ::= FuncObj

  configuration <C>
                    <k> $PGM:Block </k>
                    <Stage> .Map </Stage>
                    <ObjStore> (0 |-> Obj(.Map, nofunobj)) </ObjStore>
                    <Scope> 0, .Refs </Scope>
                    <R> 1 </R>
                    <in stream="stdin"> .List </in>
                    <out stream="stdout"> .List </out>
                </C>

  rule [Val-Sequencing] :
      V:Value ; St:Stmt => St

  /**
   * Objects
   */
  rule [Gets-Got] :
       <k> Gets((R:Ref, _), Str:String) => V ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map(Str |-> V:Value), _)) </ObjStore>

  rule [Gets-Miss] :
       <k> Gets((R:Ref, Rs:Refs), Str:String) => Gets(Rs, Str) ...</k>
       <ObjStore> OS:Map(R |-> Obj(M:Map, _)) </ObjStore>
       when notBool(Str in keys(M))
    
  rule [Sets-Sot] :
       <k> Sets((R:Ref, R2:Ref, Rs:Refs), Str:String, V:Value) => V ...</k>
       <ObjStore> OS:Map(R |-> Obj(M:Map (Str |-> (_ => V)), nofunobj)) </ObjStore>

  rule [Sets-Miss] :
       <k> Sets((R:Ref, R2:Ref, Rs:Refs => R2, Rs), Str:String, V:Value) ...</k>
       <ObjStore> OS:Map(R |-> Obj(M:Map, _)) </ObjStore>
       when notBool(Str in keys(M))

  rule [Sets-Glo] :
       Sets((R:Ref, .Refs), Str:String, V:Value) => R[Str] = V 

  rule [Object-Intro] :
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
       <k> ( R:Ref [ Str : String ] => V ) ... </k>
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

/*
  rule [Getter-Update] :
       <k> (get X:Id () { B:Block }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(Id2String(X) |-> (F1 => FuncObj(Rs, .Ids, B)) ** F2) </Stage>
       <Scope> Rs:Refs </Scope>

  rule [Getter-Intro] :
       <k> (get X:Id () { B:Block }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => Id2String(X) |-> FuncObj(Rs,.Ids,B) ** nofun) </Stage>
       <Scope> Rs </Scope>
       when notBool(Id2String(X) in keys(M))

  rule [E-Getter] :
       <k> ( R:Ref [ Str : String ] => B) ... </k>
       <ObjStore> OS:Map(R |-> Obj(O:Map(Str |-> B:Block ** F), _)) </ObjStore>
*/
  
  /**
   * Setters
   */

/*
  rule [Setter-Update] :
       <k> (set S:Id ( I:Id ) { B:Block }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(Id2String(S) |-> F ** (F1 => ( function (I) { B } ))) </Stage>

  rule [Setter-Intro] :
       <k> (set S:Id ( I:Id ) { B:Block }, O:ValObjFields => O) ... </k>
       <Stage> M:Map(. => Id2String(S) |-> nofun ** ( function (I) { B } )) </Stage>
       when notBool(Id2String(S) in keys(M)) 
                    
  rule [E-Setter] :
       <k> ( R:Ref [Str:String] = V:Value => F(V) ~> SetRet(V)) ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map (Str |-> F1 ** F:Func), _)) </ObjStore>
  
  rule [Setter-Return] :
       <k> (V:Value ~> SetRet(V2:Value) => V2) ... </k>
*/

  /**
   * Functions
   */

  rule [Fun-Alloc] :
       <k> function (Xs:Ids) {B:Block} => R ...</k>
       <ObjStore> OS:Map(. => R |-> Obj(.Map, FuncObj(Rs,Xs,B))) </ObjStore>
       <R> R:Ref => R +Int 1 </R>
       <Scope> Rs:Refs </Scope>
       
  rule [Fun-Lookup] : 
       <k>( (F:Ref => FuncObj(Env,Xs,B)) ( Vs:Values ))  ...</k>
       <ObjStore> OS:Map(F |-> Obj(M:Map,FuncObj(Env:Refs,Xs:Ids,B:Block)))</ObjStore>

  rule [Fun-Call-Some-Args] :
       <k> FuncObj(Env:Refs,(X:Id, Xs:Ids),(var Ys:Ids; S:Stmt)) ( V:Value, Vs:Values ) => 
           FuncObj(Env:Refs,Xs,(var X,Ys; X=V; S)) ( Vs ) ... </k>

  rule [Fun-Call-No-Args] :
       <k> (FuncObj(E:Refs,.Ids,B:Block) ( .Exprs )) => B ~> FEnd(Rs) ... </k>
       <ObjStore> OS:Map(. => R |-> Obj(.Map, nofunobj)) </ObjStore>
       <Scope> Rs:Refs => R, E </Scope>
       <R> R:Ref => R +Int 1 </R>

  rule [Fun-End] :
       <k> ( return V:Value ~> FEnd( Rs ) => V ) ...</k>
       <Scope> R:Refs => Rs </Scope>
   
  rule [Short-Circuit-Return] :
       return V:Value ; S:Stmt => return V

  rule [Fun-Done-No-Return] :
       <k> (V:Value => return undefined) ~> FEnd( Rs ) ...</k>

  /**
   * Variables
   */
  rule [Var-Declaration] :
       <k> var ( X:Id, Rest:Ids => Rest ) ; St:Stmt ... </k>
       <ObjStore> OS:Map(R |-> Obj(M:Map => M[undefined / Id2String(X)], _)) </ObjStore>
       <Scope> R:Ref, _ </Scope>
  
  rule [Empty-Var] :
       var .Ids ; St:Stmt => St [structural]

  rule [Var-Assignment] :
       <k> X:Id = V:Value => Sets(Rs, Id2String(X), V) ... </k>
       <Scope> Rs:Refs </Scope>

  rule [Var-Lookup] :
       <k> X:Id => Gets(Rs, Id2String(X)) ... </k>
       <Scope> Rs:Refs </Scope>

  rule [With] :
       <k> with (R:Ref) { Rest:Stmt } => Rest ~> PopScope ... </k>
       <Scope> Rs:Refs => R,Rs </Scope>

  rule [Pop-With] :
       <k> V:Value ~> PopScope => V ... </k>
       <Scope> _, Rs:Refs => Rs </Scope>

  /**
   * Printing
   */

  syntax K ::= Print(K, Int) | "Newline"

  rule [Print] :
       print(V:Value) => Print(V,5) ~> Newline

  rule [P-Newline] : 
       <k> Newline => undefined ... </k>
       <out> ... (. => ListItem("\n")) </out>

  rule [P-undefined] :
       <k> Print(undefined, N) => . ... </k>
       <out> ... (. => ListItem("undefined")) </out>

  rule [P-null] : 
       <k> Print(null, N) => . ... </k>
       <out> ... (. => ListItem("null")) </out>

  rule [P-String] : 
       <k> Print(S:String, N) => . ... </k>
       <out> ... (. => ListItem("\"") ListItem(S) ListItem("\"")) </out>

  rule [P-ObjEmpty] :
       <k> Print (R:Ref,N) => . ... </k>
       <ObjStore> OS:Map(R |-> Obj(.Map, _)) </ObjStore>
       <out> ... (. => ListItem("{}")) </out>

  rule [P-ObjStart] :
       <k> Print (R:Ref,N) => Print(Val,N -Int 1) ~> Print(Obj(M,F), N -Int 1) ...</k>
       <ObjStore> OS:Map(R |-> Obj((Id |-> Val)M:Map, F)) </ObjStore>
       <out> ... (. => ListItem("{\"") ListItem(Id) ListItem("\":")) </out>
       when N =/=Int 0

  rule [P-ObjContinue] :
       <k> Print(Obj((Id |-> Val)M,F), N) => Print(Val,N) ~> Print(Obj(M,F),N) ...</k>
       <out> ... (. => ListItem(", \"") ListItem(Id) ListItem("\":")) </out>

  rule [P-ObjDone] :
       <k> Print(Obj(.Map,F), N) => . ...</k>
       <out> ... (. => ListItem("}")) </out>

  rule [P-ObjDeep] :
       <k> Print (R:Ref,N) => . ... </k>
       <out> ... (. => ListItem("...")) </out>
       when N ==Int 0

endmodule
