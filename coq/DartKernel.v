Require Import String.

(** Abstract syntax Kernel *)
Definition identifier : Type :=  string.
Variable Member : Type.
Variable Location : Type.
Variable DartType : Type.

(** *)
Definition node_id : Type := nat.

(** Expressions *)
Inductive expr               : Type :=
 | VarGet                    : var_declaration -> expr
 | VarSet                    : var_declaration -> expr -> expr
 | PropertyGet               : expr            -> identifier -> expr
 | PropertySet               : expr            -> identifier -> expr -> expr
 | DPropertyGet              : expr            -> Member -> expr
 | DPropertySet              : expr            -> Member -> expr -> expr
 | SuperGet                  : identifier      -> expr
 | SuperSet                  : identifier      -> expr -> expr
 | StaticGet                 : Member          -> expr
 | StaticSet                 : Member          -> expr -> expr
 | Not                       : expr            -> expr
 | And                       : expr            -> expr
 | Or                        : expr            -> expr
 | Conditional               : expr            -> expr -> expr -> expr
 | Is                        : expr            -> DartType -> expr
 | As                        : expr            -> DartType -> expr
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
 | Let                       : identifier      -> expr -> expr -> expr
 | New                       : DartType        -> list expr -> expr
 | StaticInvocation          : identifier      -> list expr -> expr
 | InstanceMethodInvocation  : expr            -> identifier -> list expr -> expr
 | DInstanceMethodInvocation : expr            -> Member -> list expr -> expr
with
 var_declaration             : Type :=
  | VarDeclaration           : node_id         -> identifier -> expr -> var_declaration.


(** Statements *)

Variable Label : Type.

Inductive stmt : Type := 
 | Block       : list stmt       -> stmt
 | EStmt       : expr            -> stmt
 | Empty       : stmt
 | Labeled     : Label           -> stmt -> stmt
 | Break       : Label           -> stmt
 | While       : expr            -> stmt -> stmt
 | Do          : stmt            -> expr -> stmt
 | For         : list identifier -> option expr -> list expr -> stmt -> stmt
 | Continue    : stmt            -> stmt
 | If          : expr            -> stmt -> stmt -> stmt
 | ReturnE     : expr            -> stmt
 | Return      : stmt
 | TryCatch    : stmt            -> list expr -> list stmt -> stmt
 | TryFinally  : stmt            -> stmt -> stmt
 | Yield       : expr            -> stmt
 | VarDecl     : var_declaration -> stmt
 | FunDecl     : DartType        -> identifier -> list DartType -> list identifier -> stmt -> stmt.

(** Values *)

Inductive Value : Type :=
 | IntVal : nat -> Value
 | DoubleVal
 | StringVal
 | ObjectVal 
 | ListVal 
 | TypeVal 
 | SymbolVal
 | NullVal.


(** Functions *)

Variable Environment : Type.

Record function : Type := mkfunction {
                             f_formals : list var_declaration;
                             f_env : Environment;
                             f_body : stmt
                           }.

(** CESK machine *)
