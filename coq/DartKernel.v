(** Abstract syntax Kernel language. *)

Variable Identifier : Type.
Variable Member : Type.
Variable Super : Type. 
Variable Location : Type.
Variable type : Type.


(** Expressions *)

Inductive expr : Type := 
 | VarrGet : Identifier -> expr
 | VarSet : Identifier -> expr -> expr
 | PropertyGet : expr -> Identifier -> expr
 | PropertySet : expr -> Identifier -> expr -> expr
 | DPropertyGet : expr -> Member -> expr
 | DPropertySet : expr -> Member -> expr -> expr
 | SuperGet : Super -> Identifier -> expr
 | SuperSet : Super -> Identifier -> expr -> expr
 | StaticGet : Member -> expr
 | StaticSet : Member -> expr -> expr
 | Not: expr -> expr
 | And: expr -> expr
 | Or: expr -> expr
 | Conditional: expr -> expr -> expr -> expr
 | Is: expr -> type -> expr
 | As: expr -> type -> expr
 | This: expr
 | Throw: expr -> expr
 | Rethrow: expr
 | Await: expr -> expr
 | String: expr
 | IntLit: expr
 | DoubleLit: expr
 | TrueLit: expr
 | FalseLit: expr
 | NullLit: expr
 | Let: Identifier -> expr -> expr -> expr
 | New: type -> list expr -> expr
 | StaticInvocation: Identifier -> list expr -> expr
 | InstanceMethodInvocation : expr -> Identifier -> list expr -> expr
 | DInstanceMethodInvocation : expr -> Member -> list expr -> expr.

(** Statements *)
Variable Label : Type.

Inductive stmt : Type := 
 | Block : list stmt -> stmt
 | EStmt : expr -> stmt
 | Empty : stmt
 | Labeled : Label -> stmt -> stmt
 | Break : Label -> stmt
 | While : expr -> stmt -> stmt
 | Do : stmt -> expr -> stmt
 | For: list Identifier -> option expr -> list expr -> stmt -> stmt
 | Continue: stmt -> stmt
 | If: expr -> stmt -> stmt -> stmt
 | ReturnE : expr -> stmt
 | Return : stmt
 | TryCatch : stmt -> list expr -> list stmt -> stmt
 | TryFinally : stmt -> stmt -> stmt
 | Yield : expr -> stmt
 | VarDecl : Identifier -> expr -> stmt
 | FunDecl : type -> Identifier -> list type -> list Identifier -> stmt -> stmt.

(** Values *)

Inductive Value : Set :=
 | IntVal : nat -> Value
 | DoubleVal
 | StringVal
 | ObjectVal 
 | ListVal 
 | TypeVal 
 | SymbolVal
 | NullVal.

(**  *)
