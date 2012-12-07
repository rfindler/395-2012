/*@

Functions, Variables, and With in JavaScript

by Lee Fan
Burke Allen Fetscher,
John Morgan Greene,
Dylan A Hirshkowitz, 
Daniel Joseph Lieberman,
Josiah William Matlack,
Ethan James Romba,
Maciej Swiech,
Matthew Pierce Wampler-Doty,
Kaicheng Zhang, and
Robby Findler



This is a model of functions and variables and \texttt{with} in
JavaScript.  It demonstrates how scope objects work (assuming that var
declarations have already been lifted to the nearest enclosing scope
or the top of the program), showing \texttt{with} in its full
glory. It includes a simplified version of objects (with normal fields
and getter/setter pairs but not prototypes or other features of
JavaScript's objects) --- just enough to be able to explain scope
objects.

Note that values in the language include only strings and objects:
numbers are not JavaScript values but references to objects in
the store. */

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

  syntax MaybeFuncObj ::= FuncObj
                        | "nofunobj"

  syntax FuncObj ::= FuncObj(Refs, Ids, Block)
  
  syntax AccessPair ::= MaybeFuncObj "**" MaybeFuncObj

  syntax K ::= Obj(Map, MaybeFuncObj)

  syntax KResult ::= Value | ValObjField | FuncObj 
  rule isKResult(return _:Value) => true

  syntax Expr ::= FuncObj

  configuration <C> <k color="green"> $PGM:Block </k>
                    <Store color="orange"> (0 |-> Obj(.Map, nofunobj)) </Store>
                    <Scope color="blue"> 0, .Refs </Scope>
                    <R color="red"> 1 </R>
                    <out stream="stdout" color="black"> .List </out>
                </C>

  rule [Val-Sequencing] :
      V:Value ; St:Stmt => St

/*@
\section{Object Creation}
*/

  rule [Object-Intro] :
       <k> ( { O:ValObjFields } => O ~> R ) ... </k>
       <Store> OS:Map(. => R |-> Obj(.Map, nofunobj)) </Store>
       <R> R:Ref => R +Int 1 </R> 

  rule [Property-Intro] :
       <k> ( Str:String : V:Value, O:ValObjFields => O) ~> R ... </k>
       <Store> OS:Map(R |-> Obj((M => M[V / Str]), _)) </Store>

  rule [Getter-Intro] :
       <k> (get X:Id () { B:Block }, O:ValObjFields => O) ~> R ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (. => Id2String(X) |-> (FuncObj(Rs,.Ids,B) ** nofunobj)), _)) </Store>
       <Scope> Rs:Refs </Scope>
       when notBool(Id2String(X) in keys(M))

  rule [Getter-Update] :
       <k> (get X:Id () { B:Block }, O:ValObjFields => O) ~> R ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (XX |-> (_ => FuncObj(Rs, .Ids, B)) ** _), _)) </Store>
       <Scope> Rs:Refs </Scope>
       when XX ==String Id2String(X)

  rule [Setter-Intro] :
       <k> (set S:Id ( I:Id ) { B:Block }, O:ValObjFields => O) ~> R ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (. => Id2String(S) |-> 
                                               (FuncObj(.Refs,.Ids,var .Ids; return undefined)
                                                ** 
                                                FuncObj(Rs,(I,.Ids),B))),_:MaybeFuncObj)) </Store>
       <Scope> Rs </Scope>
       when notBool(Id2String(S) in keys(M)) 

  rule [Setter-Update] :
       <k> (set S:Id ( Y:Id ) { B:Block }, O:ValObjFields => O) ~> R ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (SS |-> (_ ** (_ => FuncObj(Rs,(Y,.Ids), B)))), _)) </Store>
       <Scope> Rs </Scope>
       when SS ==String Id2String(S)


  rule [Reference-Object]:
       .ValObjFields ~> R => R

/*@
\section{Field Access and Mutation}
*/

  rule [E-GetField] :
       <k> ( R:Ref [ Str : String ] => V ) ... </k>
       <Store> OS:Map( R |-> Obj(O:Map(Str |-> V:Value), _)) </Store>
       
  rule [E-NoField] :
       <k> ( R:Ref [ Str : String ] => undefined) ... </k>
       <Store> OS:Map( R |-> Obj(O, _)) </Store>
       when notBool(Str in keys(O))

  rule [E-SetField-Update] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (Str |-> (V2:Value => V)),_)) </Store>

  rule [E-SetField-Create] :
       <k> ( R:Ref [Str:String] = V:Value => V ) ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (. => Str |-> V), _)) </Store>
       when notBool(Str in keys(M))

  rule [E-Getter] :
       <k> ( R:Ref [ Str : String ] => G(.Values)) ... </k>
       <Store> OS:Map(R |-> Obj(O:Map(Str |-> G ** F), _)) </Store>
  
  rule [E-Setter] :
       <k> R:Ref [Str:String] = V:Value => 
           (FuncObj(.Refs,(String2Id("x"),.Ids), var .Ids ; return V)(F(V))) ... </k>
       <Store> OS:Map(R |-> Obj(M:Map (Str |-> F1 ** F), _)) </Store>
  
/*@
\section{Variables}
*/

  syntax K ::= "PopScope"

  rule [Var-Declaration] :
       <k> var ( X:Id, Rest:Ids => Rest ) ; St:Stmt ... </k>
       <Store> OS:Map(R |-> Obj(M:Map => M[undefined / Id2String(X)], _)) </Store>
       <Scope> R:Ref, _ </Scope>
  
  rule [Empty-Var] :
       <k> var .Ids ; St:Stmt => St ... </k> [structural]

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


/*@
\section{Scope Object Chain}
*/
  syntax K ::= Sets(Refs, String, Value)
             | Gets(Refs, String)

  rule [Gets-Got] :
       <k> Gets((R:Ref, _), Str:String) => V ... </k>
       <Store> OS:Map(R |-> Obj(M:Map(Str |-> V:Value), _)) </Store>

  rule [Gets-Miss] :
       <k> Gets((R:Ref, Rs:Refs), Str:String) => Gets(Rs, Str) ...</k>
       <Store> OS:Map(R |-> Obj(M:Map, _)) </Store>
       when notBool(Str in keys(M))
    
  rule [Sets-Sot] :
       <k> Sets((R:Ref, R2:Ref, Rs:Refs), Str:String, V:Value) => V ...</k>
       <Store> OS:Map(R |-> Obj(M:Map (Str |-> (_ => V)), nofunobj)) </Store>

  rule [Sets-Miss] :
       <k> Sets((R:Ref, R2:Ref, Rs:Refs => R2, Rs), Str:String, V:Value) ...</k>
       <Store> OS:Map(R |-> Obj(M:Map, _)) </Store>
       when notBool(Str in keys(M))

  rule [Sets-Glo] :
       <k> Sets((R:Ref, .Refs), Str:String, V:Value) => R[Str] = V  ... </k>

/*@
\section{Functions}
*/

  syntax K ::= FEnd(Refs) 

  rule [Fun-Alloc] :
       <k> function (Xs:Ids) {B:Block} => R ...</k>
       <Store> OS:Map(. => R |-> Obj(.Map, FuncObj(Rs,Xs,B))) </Store>
       <R> R:Ref => R +Int 1 </R>
       <Scope> Rs:Refs </Scope>
       
  rule [Fun-Lookup] : 
       <k>( (F:Ref => FuncObj(Env,Xs,B)) ( Vs:Values ))  ...</k>
       <Store> OS:Map(F |-> Obj(M:Map,FuncObj(Env:Refs,Xs:Ids,B:Block)))</Store>

  rule [Fun-Call-Some-Args] :
       <k> FuncObj(Env:Refs,(X:Id, Xs:Ids),(var Ys:Ids; S:Stmt)) ( V:Value, Vs:Values ) => 
           FuncObj(Env:Refs,Xs,(var X,Ys; X=V; S)) ( Vs ) ... </k>

  rule [Fun-Call-No-Args] :
       <k> (FuncObj(E:Refs,.Ids,B:Block) ( .Exprs )) => B ~> FEnd(Rs) ... </k>
       <Store> OS:Map(. => R |-> Obj(.Map, nofunobj)) </Store>
       <Scope> Rs:Refs => R, E </Scope>
       <R> R:Ref => R +Int 1 </R>

  rule [Fun-End] :
       <k> ( return V:Value ~> FEnd( Rs ) => V ) ...</k>
       <Scope> R:Refs => Rs </Scope>
   
  rule [Short-Circuit-Return] :
       <k> return V:Value ; S:Stmt => return V ... </k>

  rule [Fun-Done-No-Return] :
       <k> (V:Value => return undefined) ~> FEnd( Rs ) ...</k>

/*@
\section{Printing}
*/

  syntax K ::= Print(K, Int) | PrintFields(K, Map, Int) | Dump(String)

  rule [Print] :
       <k> print(V:Value) => Print(V,5) ~> Dump("\n") ~> undefined ... </k>

  rule [P-Dump] : 
       <k> Dump(Str) => . ... </k>
       <out> ... (. => ListItem(Str)) </out>

  rule [P-undefined] :
       <k> Print(undefined, N) => Dump("undefined") ... </k>

  rule [P-null] : 
       <k> Print(null, N) => Dump("null") ... </k>

  rule [P-String] : 
       <k> Print(S:String, N) => Dump("\"") ~> Dump(S) ~> Dump("\"") ... </k>

  rule [P-ObjEmpty] :
       <k> Print (R:Ref,N) => Dump("{}") ... </k>
       <Store> OS:Map(R |-> Obj(.Map, _)) </Store>
       when N =/=Int 0

  rule [P-ObjStart] :
       <k> Print (R:Ref,N) => 
           Dump("{") ~> PrintFields(sortStrings(keys(M)),M,N -Int 1) ... </k>
       <Store> OS:Map(R |-> Obj(M:Map, F)) </Store>
       when N =/=Int 0

  rule [P-ObjContinue] :
       <k> PrintFields(S1 +_+ S2 +_+ Rs,M(S1 |-> Val),N) => 
           Dump("\"") ~> Dump(S1) ~> Dump("\":") ~> Print(Val,N)
           ~> Dump(", ") ~> PrintFields(S2 +_+ Rs,M,N) ... </k>

  rule [P-ObjDone] :
       <k> PrintFields(S1 +_+ .Strings,S1 |-> Val,N) => 
           Dump("\"") ~> Dump(S1) ~> Dump("\":") ~> Print(Val,N) ~> Dump("}")  ...</k>

  rule [P-ObjDeep] :
       <k> Print (R:Ref,N) => Dump("...") ... </k>
       when N ==Int 0

/*@
\section{Sorting}
*/
  syntax Strings ::= List{String,"+_+"} | sortStrings(Set) | insertString(String,Strings)
  rule sortStrings(SetItem(S1) R:Set) =>
       insertString(S1 , sortStrings(R))             [anywhere]
  rule sortStrings(.Set) => .Strings                 [anywhere]

  rule insertString(S1,.Strings) => S1 +_+ .Strings  [anywhere]
  rule insertString(S1,(S2 +_+ Rs)) => S1 +_+ (S2 +_+ Rs)
       when S1 <String S2                            [anywhere]
  rule insertString(S1,(S2 +_+ Rs)) => S2 +_+ insertString(S1,Rs)
       when S2 <=String S1                           [anywhere]

endmodule
