Require Import
        String
        Coq.FSets.FMapList
        Coq.Structures.OrderedTypeEx.


Module Import M := FMapList.Make(Nat_as_OT).

(** Abstract syntax Kernel *)
Definition identifier : Type :=  string.
Definition location   : Type := nat.

Inductive member : Type :=
 | Field  : member
 | Method : member
 | Getter : member
 | Setter : member.

Variable DartType : Type.



(** *)
Definition node_id : Type := nat.

(** Expressions *)
Inductive expr : Type :=
 | VarGet                    : var_declaration -> expr
 | VarSet                    : var_declaration -> expr       -> expr
 | PropertyGet               : expr            -> identifier -> expr
 | PropertySet               : expr            -> identifier -> expr      -> expr
 | DPropertyGet              : expr            -> member     -> expr
 | DPropertySet              : expr            -> member     -> expr      -> expr
 | SuperGet                  : identifier      -> expr
 | SuperSet                  : identifier      -> expr       -> expr
 | StaticGet                 : member          -> expr
 | StaticSet                 : member          -> expr       -> expr
 | Not                       : expr            -> expr
 | And                       : expr            -> expr
 | Or                        : expr            -> expr
 | Conditional               : expr            -> expr       -> expr      -> expr
 | Is                        : expr            -> DartType   -> expr
 | As                        : expr            -> DartType   -> expr
 | This                      : expr
 | Throw                     : expr            -> expr
 | Rethrow                   : expr
 | Await                     : expr            -> expr
 | String                    : expr
 | IntLit                    : expr
 | DoubleLit                 : expr
 | TrueLit                   : expr
 | FalseLit                  : expr
 | NullLit                   : expr
 | Let                       : identifier      -> expr       -> expr      -> expr
 | New                       : DartType        -> list expr  -> expr
 | StaticInvocation          : identifier      -> list expr  -> expr
 | InstanceMethodInvocation  : expr            -> identifier -> list expr -> expr
 | DInstanceMethodInvocation : expr            -> member     -> list expr -> expr
with
 var_declaration             : Type :=
 | VarDeclaration            : node_id         -> identifier -> expr      -> var_declaration.


(** Statements *)

Variable Label : Type.

Inductive stmt : Type :=
 | Block      : list stmt       -> stmt
 | EStmt      : expr            -> stmt
 | Empty      : stmt
 | Labeled    : Label           -> stmt        -> stmt
 | Break      : Label           -> stmt
 | While      : expr            -> stmt        -> stmt
 | Do         : stmt            -> expr        -> stmt
 | For        : list identifier -> option expr -> list expr     -> stmt            -> stmt
 | Continue   : stmt            -> stmt
 | If         : expr            -> stmt        -> stmt          -> stmt
 | ReturnE    : expr            -> stmt
 | Return     : stmt
 | TryCatch   : stmt            -> list expr   -> list stmt     -> stmt
 | TryFinally : stmt            -> stmt        -> stmt
 | Yield      : expr            -> stmt
 | VarDecl    : var_declaration -> stmt
 | FunDecl    : DartType        -> identifier  -> list DartType -> list identifier -> stmt -> stmt.


(** Values *)

Inductive value : Type :=
 | IntVal    : nat -> value
 | DoubleVal : value
 | StringVal
 | ObjectVal
 | ListVal
 | TypeVal
 | SymbolVal
 | NullVal.


Definition binding     : Type := var_declaration -> location.
Definition environment : Type := M.t binding.
Definition store       : Type := M.t value.

Record function : Type := mkfunction {
                             f_formals : list var_declaration;
                             f_env : environment;
                             f_body : stmt
                           }.
(** CESK machine *)
