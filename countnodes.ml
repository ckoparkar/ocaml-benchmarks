open Parse_grammar
module T = Domainslib.Task

let cons_cost = 1
let scalar_cost = 0
let tag_cost = 1

let rec c_toplvl (tl : toplvl) : int =
  match tl with
    DefineValues (ls_sym, e) ->
     (c_list_sym ls_sym) + (c_expr e)
  | DefineSyntaxes (ls_sym, e) ->
     (c_list_sym ls_sym) + (c_expr e)
  | BeginTop (ls_toplvl) ->
     (c_list_toplvl ls_toplvl)
  | Expression e ->
     c_expr e

and c_expr (e : expr) : int =
  match e with
    VARREF s -> tag_cost + scalar_cost
  | Lambda (fmls, ls_expr) ->
     tag_cost + (c_formals fmls) + (c_list_expr ls_expr)
  | CaseLambda cs -> c_lambda_case cs
  | If (tst, thn, els) ->
     tag_cost + (c_expr tst) + (c_expr thn) + (c_expr els)
  | Begin ls ->
     tag_cost + c_list_expr ls
  | Begin0 (e, ls) ->
     tag_cost + c_expr e + c_list_expr ls
  | LetValues (lbind, ls_expr) ->
     tag_cost + (c_lvbind lbind) + (c_list_expr ls_expr)
  | LetrecValues (lbind, ls_expr) ->
     tag_cost + (c_lvbind lbind) + (c_list_expr ls_expr)
  | SetBang (s, e) ->
     tag_cost + scalar_cost + (c_expr e)
  | Quote d ->
     tag_cost + (c_datum d)
  | QuoteSyntax d ->
     tag_cost + (c_datum d)
  | QuoteSyntaxLocal d ->
     tag_cost + (c_datum d)
  | WithContinuationMark (a, b, c) ->
     tag_cost + (c_expr a) + (c_expr b) + (c_expr c)
  | App (e, ls_e) ->
     tag_cost + (c_expr e) + (c_list_expr ls_e)
  | Top s -> tag_cost + scalar_cost
  | VariableReference s -> tag_cost + scalar_cost
  | VariableReferenceTop s -> tag_cost + scalar_cost
  | VariableReferenceNull -> tag_cost

and c_lvbind (l : lvbind) =
  match l with
    CONSLVBIND (ls_s, e, rst) ->
     cons_cost + (c_list_sym ls_s) + (c_expr e) + (c_lvbind rst)
  | NULLLVBIND -> 1

and c_lambda_case (l : lambda_case) =
  match l with
    CONSLAMBDACASE (fmls, ls, rst) ->
     cons_cost + (c_formals fmls) + (c_list_expr ls) + (c_lambda_case rst)
  | NULLLAMBDACASE -> 1

and c_datum (d : datum) =
  match d with
    INTLIT i -> cons_cost + scalar_cost

and c_formals (f : formals) =
  match f with
    F1 ls -> tag_cost + (c_list_sym ls)
  | F2 (ls, s) -> tag_cost + scalar_cost + (c_list_sym ls)
  | F3 s -> tag_cost + scalar_cost

and c_list_toplvl (ls : list_toplvl) =
  match ls with
    CONSTOPLVL (t, ts) ->
     tag_cost + (c_toplvl t) + (c_list_toplvl ts)
  | NULLTOPLVL -> 1

and c_list_expr (ls : list_expr) =
  match ls with
    CONSEXPR (e, es) ->
     cons_cost + (c_expr e) + (c_list_expr es)
  | NULLEXPR -> tag_cost

and c_list_sym (ls : list_sym) =
  match ls with
    CONSSYM (s, ss) ->
     cons_cost + scalar_cost + (c_list_sym ss)
  | NULLSYM -> tag_cost

and countnodes tl = c_toplvl tl

(* -------------------------------------------------------------------------- *)

let rec c_toplvl_par pool (height : int) (tl : toplvl) : int =
  match tl with
    DefineValues (ls_sym, e) ->
     tag_cost + (c_list_sym ls_sym) + (c_expr e)
  | DefineSyntaxes (ls_sym, e) ->
     tag_cost + (c_list_sym ls_sym) + (c_expr e)
  | BeginTop (ls_toplvl) ->
     tag_cost + (c_list_toplvl_par pool height ls_toplvl)
  | Expression e ->
     tag_cost + c_expr e

and c_list_toplvl_par pool (height : int) (ls : list_toplvl) =
  if height >= 9 then c_list_toplvl ls else
    match ls with
      CONSTOPLVL (t, ts) ->
       let a_f = T.async pool (fun _ -> (c_toplvl_par pool (height+1) t)) in
       let b = c_list_toplvl_par pool height ts in
       let a = T.await pool a_f in
       tag_cost + a + b
    | NULLTOPLVL -> tag_cost

and par_countnodes pool tl = c_toplvl_par pool 0 tl
