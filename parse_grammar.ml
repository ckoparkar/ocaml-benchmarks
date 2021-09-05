open Sexplib
open Common

type sym = int

type toplvl
  = DefineValues of (list_sym * expr)
  | DefineSyntaxes of (list_sym * expr)
  | BeginTop of list_toplvl
  | Expression of expr

and expr
  = VARREF of sym
  | Lambda of (formals * list_expr)
  | CaseLambda of lambda_case
  | If of (expr * expr * expr)
  | Begin of list_expr
  | Begin0 of (expr * list_expr)
  | LetValues of (lvbind * list_expr)
  | LetrecValues of (lvbind * list_expr)
  | SetBang of (sym * expr)
  | Quote of datum
  | QuoteSyntax of datum
  | QuoteSyntaxLocal of datum
  | WithContinuationMark of (expr * expr * expr)
  | App of (expr * list_expr)
  | Top of sym
  | VariableReference of sym
  | VariableReferenceTop of sym
  | VariableReferenceNull

and lvbind
  = CONSLVBIND of (list_sym * expr * lvbind)
  | NULLLVBIND

and lambda_case
  = CONSLAMBDACASE of (formals * list_expr * lambda_case)
  | NULLLAMBDACASE

and datum = INTLIT of int

and formals
  = F1 of list_sym
  | F2 of (list_sym * sym)
  | F3 of sym

and list_toplvl
  = CONSTOPLVL of (toplvl * list_toplvl)
  | NULLTOPLVL

and list_expr
  = CONSEXPR of (expr * list_expr)
  | NULLEXPR

and list_sym
  = CONSSYM of (sym * list_sym)
  | NULLSYM

let rec parse_toplvl (v : Sexp.t) : toplvl =
  match v with
    List (Atom "DefineValues" :: ls_sym :: expr :: []) ->
     DefineValues (parse_list_sym ls_sym, parse_expr expr)
  | List (Atom "DefineSyntaxes" :: ls_sym :: expr :: []) ->
     DefineSyntaxes (parse_list_sym ls_sym, parse_expr expr)
  | List (Atom "BeginTop" :: ls_toplvl :: []) ->
     BeginTop (parse_list_toplvl ls_toplvl)
  | List (Atom "Expression" :: expr :: []) ->
     Expression (parse_expr expr)
  | _ ->
     raise (Failure ("cannot parse toplvl: " ^ Sexp.to_string v))

and parse_expr (v : Sexp.t) : expr =
  match  v with
    List (Atom "VARREF" :: s :: []) -> VARREF (parse_sym s)
  | List (Atom "Lambda" :: formals :: ls_expr :: []) ->
     Lambda (parse_formals formals, parse_list_expr ls_expr)
  | List (Atom "CaseLambda" :: lcase :: []) ->
     CaseLambda (parse_lambda_case lcase)
  | List (Atom "If" :: a :: b :: c :: []) ->
     If (parse_expr a, parse_expr b, parse_expr c)
  | List (Atom "Begin" :: ls_expr :: []) ->
     Begin (parse_list_expr ls_expr)
  | List (Atom "Begin0" :: expr :: ls_expr :: []) ->
     Begin0 (parse_expr expr, parse_list_expr ls_expr)
  | List (Atom "LetValues" :: lv :: ls_expr :: []) ->
     LetValues (parse_lvbind lv, parse_list_expr ls_expr)
  | List (Atom "LetrecValues" :: lv :: ls_expr :: []) ->
     LetrecValues (parse_lvbind lv, parse_list_expr ls_expr)
  | List (Atom "SetBang" :: s :: expr :: []) ->
     SetBang (parse_sym s, parse_expr expr)
  | List (Atom "Quote" :: d :: []) ->
     Quote (parse_datum d)
  | List (Atom "QuoteSyntax" :: d :: []) ->
     QuoteSyntax (parse_datum d)
  | List (Atom "QuoteSyntaxLocal" :: d :: []) ->
     QuoteSyntaxLocal (parse_datum d)
  | List (Atom "WithContinuationMark" :: a :: b :: c :: []) ->
     WithContinuationMark (parse_expr a, parse_expr b, parse_expr c)
  | List (Atom "App" :: expr :: ls_expr :: []) ->
     App (parse_expr expr, parse_list_expr ls_expr)
  | List (Atom "Top" :: s :: []) ->
     Top (parse_sym s)
  | List (Atom "VariableReference" :: s :: []) ->
     VariableReference (parse_sym s)
  | List (Atom "VariableReferenceTop" :: s :: []) ->
     VariableReferenceTop (parse_sym s)
  | List (Atom "VariableReferenceNull" :: []) -> VariableReferenceNull
  | _ -> raise (Failure ("cannot parse toplvl: " ^ Sexp.to_string v))

and parse_lvbind (v : Sexp.t) : lvbind =
  match v with
    List (Atom "CONSLVBIND" :: ls_sym :: expr :: lbind :: []) ->
     CONSLVBIND (parse_list_sym ls_sym, parse_expr expr, parse_lvbind lbind)
  | List (Atom "NULLLVBIND" :: []) ->
     NULLLVBIND
  | _ -> raise (Failure ("cannot parse lvbind: " ^ Sexp.to_string v))

and parse_lambda_case (v : Sexp.t) : lambda_case = NULLLAMBDACASE
(* match v with
 *   List (Atom "CONSLAMBDACASE" :: fmls :: ls_expr :: lcase :: []) ->
 *    CONSLAMBDACASE (parse_formals fmls, parse_list_expr ls_expr, parse_lambda_case lcase)
 * | List (Atom "NULLLAMBDACASE" :: []) ->
 *    NULLLAMBDACASE
 * | _ -> raise (Failure ("cannot parse lambda case: " ^ Sexp.to_string v)) *)

and parse_datum (v : Sexp.t) : datum =
  match v with
    List (Atom "INTLIT" :: Atom i :: []) ->
     INTLIT (int_of_string i)
  | _ -> raise (Failure ("cannot parse datum: " ^ Sexp.to_string v))

and parse_formals (v : Sexp.t) : formals =
  match v with
    List (Atom "F1" :: ls_sym :: []) ->
     F1 (parse_list_sym ls_sym)
  | List (Atom "F2" :: ls_sym :: s :: []) ->
     F2 (parse_list_sym ls_sym, parse_sym s)
  | List (Atom "F3" :: s :: []) ->
     F3 (parse_sym s)
  | _ -> raise (Failure ("cannot parse formals: " ^ Sexp.to_string v))

and parse_list_toplvl (v : Sexp.t) : list_toplvl =
  match v with
    List (Atom "CONSTOPLVL" :: tplvl :: ls_toplvl :: []) ->
     CONSTOPLVL (parse_toplvl tplvl, parse_list_toplvl ls_toplvl)
  | List (Atom "NULLTOPLVL" :: []) ->
     NULLTOPLVL
  | _ -> raise (Failure ("cannot parse list toplvl: " ^ Sexp.to_string v))

and parse_list_expr (v : Sexp.t) : list_expr =
  match v with
    List (Atom "CONSEXPR" :: expr :: ls_expr :: []) ->
     CONSEXPR (parse_expr expr, parse_list_expr ls_expr)
  | List (Atom "NULLEXPR" :: []) ->
     NULLEXPR
  | _ -> raise (Failure ("cannot parse list expr: " ^ Sexp.to_string v))

and parse_list_sym (v : Sexp.t) : list_sym =
  match v with
    List (Atom "CONSSYM" :: sym :: ls_sym :: []) ->
     CONSSYM (parse_sym sym, parse_list_sym ls_sym)
  | List (Atom "NULLSYM" :: []) ->
     NULLSYM
  | _ -> raise (Failure ("cannot parse list expr: " ^ Sexp.to_string v))

and parse_sym (v : Sexp.t) : sym =
  match v with
    Atom s -> 0
  | _ -> raise (Failure ("cannot parse sym: " ^ Sexp.to_string v))

let parse_file filename =
  let lines = read_file filename in
  let str = String.concat " " lines in
  let e = Sexp.of_string str in
  parse_toplvl e
