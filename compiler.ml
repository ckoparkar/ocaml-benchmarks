(* Course compiler *)

module L = List
module T = Domainslib.Task

(* --------------------------------------------------------------------------------
 * -- Data types common to all languages *)

type sym = int
type ty = sym
type label = sym
type var = sym

let sym_to_string s = string_of_int s

type arg = IntArg of int | TrueArg | FalseArg | VarArg of var

let print_arg arg =
  match arg with
    IntArg (i) -> print_string ("(IntArg " ^ string_of_int i ^ ")")
  | TrueArg -> print_string "(TrueArg)"
  | FalseArg -> print_string "(FalseArg)"
  | VarArg (v) -> print_string ("(VarArg " ^ sym_to_string v ^ ")")

type prim = AddP | SubP | AndP | OrP

let print_prim p =
  match p with
    AddP -> print_string "AddP"
  | SubP -> print_string "SubP"
  | AndP -> print_string "AndP"
  | OrP -> print_string "OrP"

type cmp = EqP | LtP

let print_cmp p =
  match p with
    EqP -> print_string "EqP"
  | LtP -> print_string "LtP"

(* --------------------------------------------------------------------------------
 * -- Environments *)

type typeEnv = (sym * sym) list
type varEnv = (sym * sym) list
type aliasEnv = (sym * sym) list

let default_int = 0
let default_sym = 0

let lookup_with_default def k env =
  match L.assq_opt k env with
    Some v -> v
  | None -> def

let empty_env = []

let insert_env env k v = (k, v) :: env

let lookup_env env k = lookup_with_default default_sym k env

let contains_env env k =
  let v = lookup_with_default default_sym k env in
  if v == default_sym
  then false
  else true

type homesEnv = (sym * int) list

let empty_int_env = []

let insert_int_env env k v = (k, v) :: env

let lookup_int_env env k = lookup_with_default default_int k env

let contains_int_env env k =
  let v = lookup_with_default default_int k env in
  if v == default_int
  then false
  else true

(* -------------------------------------------------------------------------------- *)


let intTy = 0
let boolTy = 1
let errorTy = 2

(* --------------------------------------------------------------------------------
 * -- ANF'd Source *)

type simplExpA = ArgA of arg
               | ReadA
               | NegA of arg
               | NotA of arg
               | PrimA of (prim * arg * arg)
               | CmpA of (cmp * arg * arg)

type expA = SimplA of simplExpA
          | LetA of (var * simplExpA *expA)
          (* A let binding which does not have a conditional in its body. *)
          | LetA2 of (var * simplExpA * expA)
          | IfA of (simplExpA * expA * expA)

type lang_a = ProgramA of (ty * expA)
            | ErrorA of ty

let print_simpl_expa exp =
  match exp with
    ArgA (a) -> print_string ("(ArgA ") ; print_arg a ; print_string (")")
  | ReadA    -> print_string ("(ReadA)")
  | NegA (a) -> print_string ("(NegA ") ; print_arg a ; print_string (")")
  | NotA (a) -> print_string ("(NotA ") ; print_arg a ; print_string (")")
  | PrimA (p,a1,a2) -> print_string ("(PrimA " ) ;
                       print_prim p ;
                       print_string (" ") ;
                       print_arg a1 ;
                       print_string (" ") ;
                       print_arg a2 ;
                       print_string (")")
  | CmpA (p,a1,a2) -> print_string ("(CmpA " ) ;
                      print_cmp p ;
                      print_string (" ") ;
                      print_arg a1 ;
                      print_string (" ") ;
                      print_arg a2 ;
                      print_string (")")

let rec print_expa exp =
  match exp with
    SimplA (simpl) -> print_string ("(SimplA ") ; print_simpl_expa simpl ; print_string (")")
  | LetA (v, rhs, bod) -> print_string ("(LetA " ^ sym_to_string v ^ " ") ;
                          print_simpl_expa rhs ;
                          print_expa bod ;
                          print_string (")")
  | LetA2 (v, rhs, bod) -> print_string ("(LetA2 " ^ sym_to_string v ^ " ") ;
                           print_simpl_expa rhs ;
                           print_expa bod ;
                           print_string (")")
  | IfA (a, b, c) -> print_string ("(IfA ") ;
                     print_simpl_expa a ;
                     print_string (" ") ;
                     print_expa b ;
                     print_string (" ") ;
                     print_expa c ;
                     print_string (")")

let print_program_a p =
  match p with
    ProgramA (ty, exp) -> print_string ("(ProgramA " ^ sym_to_string ty ^ " ") ;
                          print_expa exp ;
                          print_string (")")

  | ErrorA (err) -> print_string ("(ErrorA " ^ sym_to_string err ^ ")")


(* --------------------------------------------------------------------------------
 * -- C *)

type expC = ArgC of arg
          | ReadC
          | NegC of arg
          | NotC of arg
          | PrimC of (prim * arg * arg)
          | CmpC of (cmp * arg * arg)

type stmC = AssignC of (var * expC)

type tailC = RetC of expC
           | SeqC of (stmC * tailC)
           | IfC of (label * label * expC)
           | GotoC of label

type blkC = BlockCons of (label * tailC * blkC)
          | BlockNil
          | BlockAppend of (blkC * blkC)

type tailAndBlk = MkTailAndBlk of (tailC * blkC)

type lang_c = ProgramC of (ty * (sym list) * blkC)
            | ErrorC of ty

let print_expc exp =
  match exp with
    ArgC (a) -> print_string ("(ArgC ") ; print_arg a ; print_string (")")
  | ReadC    -> print_string ("(ReadC)")
  | NegC (a) -> print_string ("(NegC ") ; print_arg a ; print_string (")")
  | NotC (a) -> print_string ("(NotC ") ; print_arg a ; print_string (")")
  | PrimC (p,a1,a2) -> print_string ("(PrimC " ) ;
                       print_prim p ;
                       print_string (" ") ;
                       print_arg a1 ;
                       print_string (" ") ;
                       print_arg a2 ;
                       print_string (")")
  | CmpC (p,a1,a2) -> print_string ("(CmpC " ) ;
                      print_cmp p ;
                      print_string (" ") ;
                      print_arg a1 ;
                      print_string (" ") ;
                      print_arg a2 ;
                      print_string (")")


let print_stm (AssignC (v, exp)) = print_string ("(AssignC " ^ sym_to_string v ^ " ") ;
                                   print_expc exp ;
                                   print_string (")")

let rec print_tail tail =
  match tail with
    RetC (exp) -> print_string ("(RetC ") ; print_expc exp ; print_string (")")
  | SeqC (stm, tail) -> print_string ("(SeqC ") ;
                        print_stm stm ;
                        print_string (" ") ;
                        print_tail tail ;
                        print_string (")")

  | IfC (thn, els, cmp)  -> print_string ("(IfC " ^ sym_to_string thn ^ " " ^ sym_to_string els) ;
                            print_expc cmp ;
                            print_string (")")

  | GotoC lbl -> print_string ("(GotoC " ^ sym_to_string lbl ^ ")")

let rec print_blk blk =
  match blk with
    BlockCons (lbl, tail, rst) -> print_string ("(BlockCons " ^ sym_to_string lbl ^ " ") ;
                                  print_tail tail ;
                                  print_string (" ") ;
                                  print_blk rst ;
                                  print_string (")")
  | BlockNil -> print_string ("(BlockNil)")
  | BlockAppend (b1, b2) -> print_string ("(BlockAppend " ) ;
                            print_blk b1 ;
                            print_string (" ") ;
                            print_blk b2 ;
                            print_string (")")

let rec print_locals ls =
  match ls with
    [] -> ()
  | (x :: xs) -> print_string (sym_to_string x ^ " ") ;
                 print_locals xs

let print_program_c p =
  match p with
    ProgramC (ty, locals, blk) -> print_string ("(ProgramC " ^ sym_to_string ty ^ " (") ;
                                  print_locals locals ;
                                  print_string (")") ;
                                  print_blk blk ;
                                  print_string (")")
  | ErrorC (err) -> print_string ("(ErrorC " ^ sym_to_string err ^ ")")

(* --------------------------------------------------------------------------------
 * -- X86 with variables *)

type reg = string

type argX86 = IntX86 of int
            | VarX86 of var
            | RegX86 of reg
            | DerefX86 of (reg * int)


type instrs = InstrCons of (instr * instrs)
            | InstrNil
            | InstrAppend of (instrs * instrs)
            | InstrAppend2 of (label * instrs * instrs)

and instr = AddQ of (argX86 * argX86)
          | SubQ of (argX86 * argX86)
          | NegQ of argX86
          | XorQ of (argX86 * argX86)
          | SetEQ of reg
          | CmpQ of (argX86 * argX86)
          | MovQ of (argX86 * argX86)
          | MovzbQ of (argX86 * argX86)
          | JumpQ of label
          | JumpEQ of label
          | PushQ of argX86
          | PopQ of argX86
          | RetQ

type pseudoX86 = ProgramX86 of (ty * (sym list) * instrs)
               | ErrorX86 of ty

let print_reg r = print_string ("% " ^ r)

let print_argx86 a =
  match a with
    IntX86 (i) -> print_string (string_of_int i)
  | VarX86 (v) -> print_string (sym_to_string v)
  | RegX86 (r) -> print_reg r
  | DerefX86 (r, o) -> print_string (string_of_int o) ;
                       print_string ("(") ;
                       print_reg r ;
                       print_string (")")

let rec print_instrs instrs =
  match instrs with
    InstrCons (i, rst) -> print_instr i ;
                          print_string ("\n") ;
                          print_instrs rst
  | InstrNil -> ()
  | InstrAppend (xs, ys) ->  print_instrs xs ;
                             print_string ("\n") ;
                             print_instrs ys
  | InstrAppend2 (lbl, xs, ys) -> print_string (sym_to_string lbl ^ ":\n") ;
                                  print_instrs xs ;
                                  print_string ("\n") ;
                                  print_instrs ys

and print_instr instr =
  match instr with
    AddQ (a1, a2) -> print_string ("addq ") ;
                     print_argx86 a1 ;
                     print_string ", " ;
                     print_argx86 a2
  | SubQ (a1, a2) -> print_string ("subq ") ;
                     print_argx86 a1 ;
                     print_string ", " ;
                     print_argx86 a2
  | NegQ (a1) -> print_string ("negq ") ; print_argx86 a1
  | XorQ (a1, a2) -> print_string ("xorq ") ;
                     print_argx86 a1 ;
                     print_string ", " ;
                     print_argx86 a2
  | SetEQ (r) -> print_string ("sete ") ; print_reg r
  | CmpQ (a1, a2) -> print_string ("cmpq ") ;
                     print_argx86 a1 ;
                     print_string ", " ;
                     print_argx86 a2
  | MovQ (a1, a2) -> print_string ("movq ") ;
                     print_argx86 a1 ;
                     print_string ", " ;
                     print_argx86 a2
  | MovzbQ (a1, a2) -> print_string ("movzbq ") ;
                       print_argx86 a1 ;
                       print_string ", " ;
                       print_argx86 a2
  | JumpQ (lbl) -> print_string ("jmp " ^ sym_to_string lbl)
  | JumpEQ (lbl) -> print_string ("je " ^ sym_to_string lbl)
  | PushQ (a1) -> print_string ("pushq ") ; print_argx86 a1
  | PopQ (a1) -> print_string ("popq ") ; print_argx86 a1
  | RetQ -> print_string "retq"


let print_pseudox86 p =
  match p with
    ProgramX86 (ty, locals, instrs) -> print_string ("(locals ") ;
                                       print_locals locals ;
                                       print_string (")\n") ;
                                       print_instrs instrs

  | ErrorX86 (err) -> print_string ("(ErrorX86 " ^ sym_to_string err ^ ")")


(* --------------------------------------------------------------------------------
 * -- Gensym *)

(* let global_gensym_counter = ref 0
 *
 * let gensym () =
 *   global_gensym_counter := !global_gensym_counter + 1 ;
 *   "gensym_" ^ string_of_int (!global_gensym_counter) *)

let conclusion_lbl = 0

let global_gensym_counter = Atomic.make 1

let gensym () =
  let idx = Atomic.fetch_and_add global_gensym_counter 1 in
  idx

(* --------------------------------------------------------------------------------
 * -- Uniqify *)

let uniqify_arg var_env arg =
  match arg with
    VarArg (v) -> VarArg (lookup_env var_env v)
  | _ -> arg

let uniqify_simpl_expa var_env exp =
  match exp with
    ArgA (a) -> ArgA (uniqify_arg var_env a)
  | ReadA -> ReadA
  | NegA (e) -> NegA (uniqify_arg var_env e)
  | NotA (e) -> NotA (uniqify_arg var_env e)
  | PrimA (p, e1, e2) -> PrimA (p, (uniqify_arg var_env e1), (uniqify_arg var_env e2))
  | CmpA (c, e1, e2)  -> CmpA (c, (uniqify_arg var_env e1), (uniqify_arg var_env e2))

let rec uniqify_expa var_env exp =
  match exp with
    SimplA (simpl) -> SimplA (uniqify_simpl_expa var_env simpl)
  | LetA (v, rhs, bod) ->
     (* if contains_env var_env v
      * then *)
     let rhs' = uniqify_simpl_expa var_env rhs in
     let v' = gensym() in
     let var_env' = insert_env var_env v v' in
     let bod' = uniqify_expa var_env' bod in
     LetA (v', rhs', bod')
  (* else
   *   let rhs' = uniqify_simpl_expa var_env rhs in
   *   let var_env' = insert_env var_env v v in
   *   let bod' = uniqify_expa var_env' bod in
   *   LetA (v, rhs', bod') *)
  | LetA2 (v, rhs, bod) ->
     (* if contains_env var_env v
      * then *)
     let rhs' = uniqify_simpl_expa var_env rhs in
     let v' = gensym() in
     let var_env' = insert_env var_env v v' in
     let bod' = uniqify_expa var_env' bod in
     LetA2 (v', rhs', bod')
  (* else
   *   let rhs' = uniqify_simpl_expa var_env rhs in
   *   let var_env' = insert_env var_env v v in
   *   let bod' = uniqify_expa var_env' bod in
   *   LetA2 (v, rhs', bod') *)
  | IfA (a,b,c) -> IfA (uniqify_simpl_expa var_env a, uniqify_expa var_env b, uniqify_expa var_env c)

let rec uniqify_expa_par pool var_env exp =
  match exp with
    SimplA (simpl) -> SimplA (uniqify_simpl_expa var_env simpl)
  | LetA (v, rhs, bod) ->
     (* if contains_env var_env v
      * then *)
     let rhs' = uniqify_simpl_expa var_env rhs in
     let v' = gensym() in
     let var_env' = insert_env var_env v v' in
     let bod' = uniqify_expa_par pool var_env' bod in
     LetA (v', rhs', bod')
  (* else
   *   let rhs' = uniqify_simpl_expa var_env rhs in
   *   let var_env' = insert_env var_env v v in
   *   let bod' = uniqify_expa_par pool var_env' bod in
   *   LetA (v, rhs', bod') *)
  | LetA2 (v, rhs, bod) ->
     (* if contains_env var_env v
      * then *)
     let rhs' = uniqify_simpl_expa var_env rhs in
     let v' = gensym() in
     let var_env' = insert_env var_env v v' in
     let bod' = uniqify_expa var_env' bod in
     LetA2 (v', rhs', bod')
  (* else
   *   let rhs' = uniqify_simpl_expa var_env rhs in
   *   let var_env' = insert_env var_env v v in
   *   let bod' = uniqify_expa var_env' bod in
   *   LetA2 (v, rhs', bod') *)
  | IfA (a,b,c) ->
     let b' = T.async pool (fun _ -> uniqify_expa_par pool var_env b) in
     let c' = uniqify_expa_par pool var_env c in
     IfA (uniqify_simpl_expa var_env a, T.await pool b', c')

let uniqify prg =
  match prg with
    ProgramA (ty, exp) -> ProgramA (ty, uniqify_expa empty_env exp)
  | ErrorA (err) -> ErrorA (err)

let uniqify_par pool prg =
  match prg with
    ProgramA (ty, exp) -> ProgramA (ty, uniqify_expa_par pool empty_env exp)
  | ErrorA (err) -> ErrorA (err)

(* --------------------------------------------------------------------------------
 * -- Typecheck *)

let typecheck_arg ty_env arg =
  match arg with
    IntArg(i) -> intTy
  | TrueArg -> boolTy
  | FalseArg -> boolTy
  | VarArg (v) ->
     if contains_env ty_env v
     then lookup_env ty_env v
     else errorTy

let typecheck_prim p t1 t2 =
  match p with
    AddP -> if t1 == intTy && t2 == intTy
            then intTy
            else errorTy
  | SubP -> if t1 == intTy && t2 == intTy
            then intTy
            else errorTy
  | AndP -> if t1 == boolTy && t2 == boolTy
            then boolTy
            else errorTy
  | OrP -> if t1 == boolTy && t2 == boolTy
           then boolTy
           else errorTy

let typecheck_cmp p t1 t2 =
  match p with
    LtP -> if t1 == intTy && t2 == intTy
           then boolTy
           else errorTy
  | EqP -> if t1 == intTy && t2 == intTy
           then boolTy
           else errorTy

let typecheck_simpl_expa ty_env exp =
  match exp with
    ArgA (arg) -> typecheck_arg ty_env arg
  | ReadA -> intTy
  | NegA (exp1) ->
     let texp1 = typecheck_arg ty_env exp1 in
     if texp1 == intTy
     then intTy
     else errorTy
  | NotA (exp1) ->
     let texp1 = typecheck_arg ty_env exp1 in
     if texp1 == boolTy
     then intTy
     else errorTy
  | PrimA (p, a1, a2) ->
     let t1 = typecheck_arg ty_env a1 in
     let t2 = typecheck_arg ty_env a2 in
     typecheck_prim p t1 t2
  | CmpA (p, a1, a2) ->
     let t1 = typecheck_arg ty_env a1 in
     let t2 = typecheck_arg ty_env a2 in
     typecheck_cmp p t1 t2

let rec typecheck_expa ty_env exp =
  match exp with
    SimplA (simpl) -> typecheck_simpl_expa ty_env simpl
  | LetA (v, rhs, bod) ->
     let ty = typecheck_simpl_expa ty_env rhs in
     let ty_env' = insert_env ty_env v ty in
     typecheck_expa ty_env' bod
  | LetA2 (v, rhs, bod) ->
     let ty = typecheck_simpl_expa ty_env rhs in
     let ty_env' = insert_env ty_env v ty in
     typecheck_expa ty_env' bod
  | IfA (a, b, c) ->
     let t1 = typecheck_simpl_expa ty_env a in
     let t2 = typecheck_expa ty_env b in
     let t3 = typecheck_expa ty_env c in
     if t1 == boolTy
     then if t2 == t3
          then t2
          else errorTy
     else errorTy

let rec typecheck_expa_par pool ty_env exp =
  match exp with
    SimplA (simpl) -> typecheck_simpl_expa ty_env simpl
  | LetA (v, rhs, bod) ->
     let ty = typecheck_simpl_expa ty_env rhs in
     let ty_env' = insert_env ty_env v ty in
     typecheck_expa_par pool ty_env' bod
  | LetA2 (v, rhs, bod) ->
     let ty = typecheck_simpl_expa ty_env rhs in
     let ty_env' = insert_env ty_env v ty in
     typecheck_expa ty_env' bod
  | IfA (a, b, c) ->
     let t1 = typecheck_simpl_expa ty_env a in
     let t2_f = T.async pool (fun _ -> typecheck_expa_par pool ty_env b) in
     let t3 = typecheck_expa_par pool ty_env c in
     let t2 = T.await pool t2_f in
     if t1 == boolTy
     then if t2 == t3
          then t2
          else errorTy
     else errorTy

let typecheck prg =
  match prg with
    ProgramA (expected, exp) ->
     let actual = typecheck_expa empty_env exp in
     if expected == actual
     then ProgramA (expected, exp)
     else ErrorA errorTy
  | ErrorA (err) -> ErrorA (err)

let typecheck_par pool prg =
  match prg with
    ProgramA (expected, exp) ->
     let actual = typecheck_expa_par pool empty_env exp in
     if expected == actual
     then ProgramA (expected, exp)
     else ErrorA errorTy
  | ErrorA (err) -> ErrorA (err)

(* --------------------------------------------------------------------------------
 * -- Explicate control *)

let to_expc exp =
  match exp with
    ArgA (a) -> ArgC (a)
  | ReadA -> ReadC
  | NegA (a) -> NegC (a)
  | NotA (a) -> NotC (a)
  | PrimA (p, a1, a2) -> PrimC (p, a1, a2)
  | CmpA (p, a1, a2) -> CmpC (p, a1, a2)

let rec explicate_tail2 exp =
  match exp with
    SimplA (simpl) -> ([], (RetC (to_expc simpl)))
  | LetA2 (v, rhs, bod) ->
     let rhs' = to_expc rhs in
     let stm = AssignC (v, rhs') in
     let (locals, tail) = explicate_tail2 bod in
     let locals' = v :: locals in
     let tail' = SeqC (stm, tail) in
     (locals', tail')
  | _ -> raise (Failure "explicate_tail2")

let rec explicate_tail exp =
  match exp with
    SimplA (simpl) ->
     let tb = MkTailAndBlk (RetC (to_expc simpl), BlockNil) in
     ([], tb)
  | LetA2 (v, rhs, bod) ->
     let (locals, tail) = explicate_tail2 exp in
     let tb = MkTailAndBlk (tail, BlockNil) in
     (locals, tb)
  | LetA (v, rhs, bod) ->
     let rhs' = to_expc rhs in
     let (locals, (MkTailAndBlk (tl, blk))) = explicate_tail bod in
     let stm = AssignC (v, rhs') in
     let tail = SeqC (stm, tl) in
     let locals' = v :: locals in
     (locals', MkTailAndBlk (tail, blk))
  | IfA (a,b,c) ->
     let a' = to_expc a in
     let (locals1, MkTailAndBlk(thn_tail, thn_blocks)) = explicate_tail b in
     let (locals2, MkTailAndBlk(els_tail, els_blocks)) = explicate_tail c in
     let locals3 = L.append locals1 locals2 in
     let thn_label = gensym() in
     let els_label = gensym() in
     let tail' = IfC (thn_label, els_label, a') in
     let blks0 = BlockCons (thn_label, thn_tail, thn_blocks) in
     let blks1 = BlockCons (els_label, els_tail, els_blocks) in
     let blks2 = BlockAppend (blks0, blks1) in
     let tb = MkTailAndBlk (tail', blks2) in
     (locals3, tb)

let rec explicate_tail_par pool exp =
  match exp with
    SimplA (simpl) ->
     let tb = MkTailAndBlk (RetC (to_expc simpl), BlockNil) in
     ([], tb)
  | LetA2 (v, rhs, bod) ->
     let (locals, tail) = explicate_tail2 exp in
     let tb = MkTailAndBlk (tail, BlockNil) in
     (locals, tb)
  | LetA (v, rhs, bod) ->
     let rhs' = to_expc rhs in
     let (locals, (MkTailAndBlk (tl, blk))) = explicate_tail_par pool bod in
     let stm = AssignC (v, rhs') in
     let tail = SeqC (stm, tl) in
     let locals' = v :: locals in
     (locals', MkTailAndBlk (tail, blk))
  | IfA (a,b,c) ->
     let a' = to_expc a in
     let b1_f = T.async pool (fun _ -> explicate_tail_par pool b) in
     let (locals2, MkTailAndBlk(els_tail, els_blocks)) = explicate_tail_par pool c in
     let (locals1, MkTailAndBlk(thn_tail, thn_blocks)) = T.await pool b1_f in
     let locals3 = L.append locals1 locals2 in
     let thn_label = gensym() in
     let els_label = gensym() in
     let tail' = IfC (thn_label, els_label, a') in
     let blks0 = BlockCons (thn_label, thn_tail, thn_blocks) in
     let blks1 = BlockCons (els_label, els_tail, els_blocks) in
     let blks2 = BlockAppend (blks0, blks1) in
     let tb = MkTailAndBlk (tail', blks2) in
     (locals3, tb)

let explicate_control prg =
  match prg with
    ProgramA (ty, exp) ->
     let (locals, (MkTailAndBlk (tail, blk0))) = explicate_tail exp in
     let start = gensym() in
     let blk2 = BlockCons (start, tail, blk0) in
     ProgramC (ty, locals, blk2)
  | ErrorA (err) -> ErrorC (err)

let explicate_control_par pool prg =
  match prg with
    ProgramA (ty, exp) ->
     let (locals, (MkTailAndBlk (tail, blk0))) = explicate_tail_par pool exp in
     let start = gensym() in
     let blk2 = BlockCons (start, tail, blk0) in
     ProgramC (ty, locals, blk2)
  | ErrorA (err) -> ErrorC (err)


(* --------------------------------------------------------------------------------
 * -- Select instructions *)

let select_instrs_arg arg =
  match arg with
    IntArg (i) -> IntX86 i
  | TrueArg -> IntX86 1
  | FalseArg -> IntX86 0
  | VarArg v -> VarX86 v

let select_instrs_exp target exp rst =
  match exp with
    ArgC arg ->
     let arg' = select_instrs_arg arg in
     InstrCons (MovQ (arg', target), rst)
  | ReadC -> raise (Failure "select_instrs_exp: ReadC")
  | NegC arg ->
     let arg' = select_instrs_arg arg in
     InstrCons (MovQ (arg', target), InstrCons (NegQ target, rst))
  | NotC arg ->
     let arg' = select_instrs_arg arg in
     InstrCons (MovQ (arg', target), InstrCons (XorQ (IntX86 1, target), rst))
  | PrimC (p, a1, a2) ->
     let a1' = select_instrs_arg a1 in
     let a2' = select_instrs_arg a2 in
     let f x = match p with AddP -> AddQ x
                          | SubP -> SubQ x
                          | AndP -> raise (Failure "select_instrs_exp: AndP")
                          | OrP -> raise (Failure "select_instrs_exp: OrP") in
     let instr1 = MovQ (a1', target) in
     let instr2 = f (a2', target) in
     InstrCons (instr1, InstrCons (instr2, rst))
  | CmpC (c, a1, a2) ->
     let a1' = select_instrs_arg a1 in
     let a2' = select_instrs_arg a2 in
     let instr1 = CmpQ (a1', a2') in
     let instr2 = match c with EqP -> SetEQ ("al")
                             | LtP -> SetEQ ("al") in
     let instr3 = MovzbQ (RegX86 "al", target) in
     InstrCons (instr1, InstrCons (instr2, InstrCons (instr3, rst)))

let rec select_instrs_tail tail =
  match tail with
    RetC exp ->
     let target = RegX86 "rax" in
     select_instrs_exp target exp (InstrCons (JumpQ conclusion_lbl, InstrNil))
  | SeqC (AssignC (v, rhs), rst) ->
     let target = VarX86 v in
     let instrs_rst = select_instrs_tail rst in
     select_instrs_exp target rhs instrs_rst
  | IfC (thn, els, cmp) ->
     let instrs_rst = InstrCons (JumpEQ thn, InstrCons (JumpQ els, InstrNil)) in
     let target = RegX86 "rbx" in
     select_instrs_exp target cmp instrs_rst
  | GotoC lbl -> raise (Failure "select_instrs_tail: GotoC")

let rec select_instrs_blk blk =
  match blk with
    BlockNil -> InstrNil
  | BlockCons (lbl, tail, rst) ->
     let instrs1 = select_instrs_tail tail in
     let instrs2 = select_instrs_blk rst in
     InstrAppend2 (lbl, instrs1, instrs2)
  | BlockAppend (blk1, blk2) ->
     let instrs1 = select_instrs_blk blk1 in
     let instrs2 = select_instrs_blk blk2 in
     InstrAppend (instrs1, instrs2)

let rec select_instrs_blk_par pool blk =
  match blk with
    BlockNil -> InstrNil
  | BlockCons (lbl, tail, rst) ->
     let instrs1_f = T.async pool (fun _ -> select_instrs_tail tail) in
     let instrs2 = select_instrs_blk_par pool rst in
     let instrs1 = T.await pool instrs1_f in
     InstrAppend2 (lbl, instrs1, instrs2)
  | BlockAppend (blk1, blk2) ->
     let instrs1_f = T.async pool (fun _ -> select_instrs_blk_par pool blk1) in
     let instrs2 = select_instrs_blk_par pool blk2 in
     let instrs1 = T.await pool instrs1_f in
     InstrAppend (instrs1, instrs2)

let select_instrs prg =
  match prg with
    ProgramC (ty, locals, blk) ->
     let conclusion = InstrAppend2 (conclusion_lbl, InstrCons (RetQ, InstrNil), InstrNil) in
     let instrs = InstrAppend (select_instrs_blk blk, conclusion) in
     ProgramX86 (ty, locals, instrs)
  | ErrorC (err) -> ErrorX86 (err)

let select_instrs_par pool prg =
  match prg with
    ProgramC (ty, locals, blk) ->
     let conclusion = InstrAppend2 (conclusion_lbl, InstrCons (RetQ, InstrNil), InstrNil) in
     let instrs = InstrAppend (select_instrs_blk_par pool blk, conclusion) in
     ProgramX86 (ty, locals, instrs)
  | ErrorC (err) -> ErrorX86 (err)

(* --------------------------------------------------------------------------------
 * -- Assign homes *)

let make_homes ls : homesEnv =
  let em = empty_int_env in
  let indices = L.mapi (fun i _ -> i) ls in
  L.fold_left2 (fun acc v i -> let stack_loc = 0 - (8 + (8 * i)) in
                               insert_int_env acc v stack_loc)
    em ls indices

let assign_homes_arg homes arg =
  match arg with
    VarX86 v ->
     let o = lookup_int_env homes v in
     (* let o = 1 in *)
     DerefX86 ("rbp", o)
  | _ -> arg

let assign_homes_instr homes instr =
  match instr with
    AddQ (a1, a2) -> AddQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | SubQ (a1, a2) -> SubQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | NegQ (a1) -> NegQ (assign_homes_arg homes a1)
  | XorQ (a1, a2) -> XorQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | SetEQ (r) -> SetEQ (r)
  | CmpQ (a1, a2) -> CmpQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | MovQ (a1, a2) -> MovQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | MovzbQ (a1, a2) -> MovzbQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | JumpQ (lbl) -> JumpQ (lbl)
  | JumpEQ (lbl) -> JumpEQ (lbl)
  | PushQ (a1) -> PushQ (assign_homes_arg homes a1)
  | PopQ (a1) -> PopQ (assign_homes_arg homes a1)
  | RetQ -> RetQ

let rec assign_homes_instrs homes instrs =
  match instrs with
    InstrCons (instr, rst) ->
     let instr' = assign_homes_instr homes instr in
     let rst' = assign_homes_instrs homes rst in
     InstrCons (instr', rst')
  | InstrNil -> InstrNil
  | InstrAppend (instrs1, instrs2) ->
     let instrs1' = assign_homes_instrs homes instrs1 in
     let instrs2' = assign_homes_instrs homes instrs2 in
     InstrAppend (instrs1', instrs2')
  | InstrAppend2 (lbl, instrs1, instrs2) ->
     let instrs1' = assign_homes_instrs homes instrs1 in
     let instrs2' = assign_homes_instrs homes instrs2 in
     InstrAppend2 (lbl, instrs1', instrs2')

let rec assign_homes_instrs_par pool homes instrs =
  match instrs with
    InstrCons (instr, rst) ->
     let instr' = assign_homes_instr homes instr in
     let rst' = assign_homes_instrs homes rst in
     InstrCons (instr', rst')
  | InstrNil -> InstrNil
  | InstrAppend (instrs1, instrs2) ->
     let instrs1_f = T.async pool (fun _ -> assign_homes_instrs_par pool homes instrs1) in
     let instrs2' = assign_homes_instrs_par pool homes instrs2 in
     let instrs1' = T.await pool instrs1_f in
     InstrAppend (instrs1', instrs2')
  | InstrAppend2 (lbl, instrs1, instrs2) ->
     let instrs1_f = T.async pool (fun _ -> assign_homes_instrs_par pool homes instrs1) in
     let instrs2' = assign_homes_instrs_par pool homes instrs2 in
     let instrs1' = T.await pool instrs1_f in
     InstrAppend2 (lbl, instrs1', instrs2')

let assign_homes prg =
  match prg with
    ProgramX86 (ty, locals, instrs) ->
     let homes = make_homes locals in
     ProgramX86 (ty, [], assign_homes_instrs homes instrs)
  | ErrorX86 err -> ErrorX86 err

let assign_homes_par pool prg =
  match prg with
    ProgramX86 (ty, locals, instrs) ->
     let homes = make_homes locals in
     ProgramX86 (ty, [], assign_homes_instrs_par pool homes instrs)
  | ErrorX86 err -> ErrorX86 err


(* -------------------------------------------------------------------------------- *)

let compile p0 =
  let p1 = typecheck p0 in
  let p2 = uniqify p1 in
  (* let _ = print_program_a p2 in *)
  let p3 = explicate_control p2 in
  let p4 = select_instrs p3 in
  let p5 = assign_homes p4 in
  p5

let compile_par pool p0 =
  let p1 = typecheck_par pool p0 in
  let p2 = uniqify_par pool p1 in
  (* let _ = print_program_a p2 in *)
  let p3 = explicate_control_par pool p2 in
  let p4 = select_instrs_par pool p3 in
  let p5 = assign_homes_par pool p4 in
  p5

let rec make_big_ex2 n =
  if n <= 0
  then SimplA (ArgA (IntArg 1))
  else
    (* let v2 = "v2" in *)
    let v2 = 1 in
    LetA2 (v2, (ArgA (IntArg (n-1))), make_big_ex2 (n-1))


let rec make_big_ex n d =
  if d > 1
  then make_big_ex2 n
  else
    (* let v1 = "v1" in *)
    let v1 = 0 in
    let branch = make_big_ex n (d+1) in
    IfA (CmpA (EqP, (IntArg n), (IntArg 0)), branch, branch)
