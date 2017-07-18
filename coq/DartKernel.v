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

(** Expressions in Dart Kernel. *)

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
 var_declaration : Type :=
 | VarDeclaration : node_id -> identifier -> expr -> var_declaration.


(** Statements in Dart Kernel *)

Record fun_signature : Type := mkfun_signature {
                            async : string;
                            type_params : list DartType;
                            required_params : nat;
                            pos_params : list var_declaration;
                            named_params : list var_declaration;
                            return_type : DartType
                           }.

Inductive stmt : Type :=
 | Block      : list stmt       -> stmt
 | EStmt      : expr            -> stmt
 | Empty      : stmt
 | Labeled    : stmt            -> stmt
 | Break      : stmt            -> stmt
 | While      : expr            -> stmt        -> stmt
 | Do         : stmt            -> expr        -> stmt
 | For        : list identifier -> option expr -> list expr -> stmt -> stmt
 | Continue   : stmt            -> stmt
 | If         : expr            -> stmt        -> stmt      -> stmt
 | ReturnE    : expr            -> stmt
 | Return     : stmt
 | TryCatch   : stmt            -> list expr   -> list stmt -> stmt
 | TryFinally : stmt            -> stmt        -> stmt
 | Yield      : expr            -> stmt
 | VarDecl    : var_declaration -> stmt
 | FunDecl    : fun_declaration -> stmt
with
fun_declaration : Type :=
  | FunDeclaration : var_declaration -> fun_signature -> stmt -> fun_declaration.

(** Values in Dart Kernel can be objects or vector literals. *)
Inductive value : Type :=
 | Object
 | Vector.

(** Environments in operation semantics of Dart Kernel are represented
  as lists of bindings. A binding associates a variable declaration
  to a location.  *)
Definition binding     : Type := var_declaration -> location.
Definition env         : Type := list binding.

Definition store_entry : Type := location -> value.
Definition store       : Type := list store_entry.

(** Function literal with captured variable declarations. *)
Record function : Type := mkfunction {
                             f_formals : list var_declaration;
                             f_env : env;
                             f_body : stmt
                            }.

(** Components for exceptions. *)

Definition strace : Type := list expr.
Definition curr_strace := list expr.
Definition curr_error := expr.

Inductive handler : Type :=
| ThrowH
| CatchH.

(** Continuations. *)

Inductive econt : Type :=
| VarSetEK
| ExpressionsEK
| NotEK
| AndEK
| OrEK
| ConditionalEK
| LetEK
| IsEK
| AsEK
| StaticGetEK
| StaticSetEK
| PropertyGetEK
| PropertySetEK
| PropertySetValueEK
| DPropertyGetEK
| DPropertySetEK
| DPropertySetValueEK
| SuperPropertyEK
| InstanceMethodEK
| DInstanceMethodEK
| VarDeclarationEK
| IfCodnitionEK
| ForConditionEK
| ExpressionEK.

Inductive scont : Type :=
| ExitSK
| BlockSK
| WhileSK
| BodySK
| NewSK
| FinallySK
| RethrowSK
| ForSK
| WhileCondSK.

Inductive acont : Type :=
| ValueA
| StringConcatenationA
| ForInitA
| ForUpdatesA
| SuperMethodA
| DInstanceMethodA
| FieldsA
| ConstructorA
| SuperA
| RedirectingA.

Inductive bcont : Type :=
| BreakK
| FinallyBreakK.

Inductive switch_cont : Type :=
| SwitchContinueK
| FinallySwitchContinueK.

Inductive labels : Type :=
 | Labels : list label -> labels
with
 label : Type :=
  | Label : stmt -> scont -> env -> label.

Inductive switch_labels : Type :=
| SwitchLabels : list switch_label -> switch_labels
with
switch_label : Type :=
| SwitchLabel: switch_cont -> env -> switch_label.


(** CESK machine *)


(** Configurations *)

Inductive configuration : Type :=
| EvalConfiguration         : expr        -> env        -> strace -> curr_strace   -> curr_error -> handler     -> econt      -> configuration
| EvalListConfiguration     : list expr   -> env        -> strace -> curr_strace   -> curr_error -> handler     -> acont      -> configuration
| ExecConfiguration         : stmt        -> env        -> labels -> switch_labels -> strace     -> curr_strace -> curr_error -> handler -> econt -> scont -> configuration
| ValuePassingConfiguration : econt       -> value      -> configuration
| ApplicationConfiguration  : acont       -> list value -> configuration
| ForwardConfiguration      : scont       -> env        -> configuration
| ThrowConfiguration        : handler     -> value      -> strace -> configuration
| BreakConfiguration        : bcont       -> configuration
| SwitchConfiguration       : switch_cont -> configuration.
