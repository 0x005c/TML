open List

type assign =
  | Set of (Type.typ * Type.typ) list
  | Compose of assign * assign
;;

let unification_error (s,t) =
  Printf.eprintf "couldn't match type \"%s\" with \"%s\""
      (Type.type_to_string s) (Type.type_to_string t);
  exit 1
;;

let unknown_variable_error s =
  Printf.eprintf "unknown variable \"%s\"" s;
  exit 1
;;

let x = ref 0 ;;

let fresh =
  fun () ->
    x := !x+1;
    Type.Var !x
;;

let rec assign s t =
  match t with
  | Type.Fun (t1,t2) -> Type.Fun (assign s t1,assign s t2)
  | Type.Tuple xs -> Type.Tuple (map (assign s) xs)
  | _ ->
  match s with
  | Set [] -> t
  | Set ((t',u)::s') -> if t==t' then u
                        else assign (Set s') t
  | Compose (s1,s2) -> assign s1 (assign s2 t)
;;

let assignC s c = List.map (fun (t,u) -> (assign s t,assign s u)) c ;;

let rec unify c =
  match c with
  | [] -> Set []
  | (s,t)::c' ->
      if s==t then unify c'
      else match (s,t) with
      (* 出現検査をしていない *)
      | (Type.Var _,_) -> Compose ((unify(assignC (Set [(s,t)]) c')),Set [(s,t)])
      | (_,Type.Var _) -> Compose ((unify(assignC (Set [(t,s)]) c')),Set [(t,s)])
      | (Type.Fun (s1,s2),Type.Fun (t1,t2)) -> unify((s1,t1)::(s2,t2)::c')
      | (_,_) -> unification_error (s,t)
;;

let rec lookup env s =
  match env with
  | [] -> unknown_variable_error s
  | (s',t)::env -> if String.equal s s' then t
                   else lookup env s
;;

let addT env (s,t) =
  let (tenv,eenv) = env in
  let tenv = (s,t)::tenv in
  (tenv,eenv)
;;

let addE env (s,e) =
  let (tenv,eenv) = env in
  let eenv = (s,e)::eenv in
  (tenv,eenv)
;;

let rec assign_let (s,e1) e2 =
  let alet = assign_let (s,e1) in
  match e2 with
  | Exp.Var s' -> if String.equal s s' then e1 else e2
  | Exp.Let (s',e3,e4) -> if String.equal s s' then e2 else Exp.Let (s',alet e3,alet e4)
  | Exp.LetRec (s',e3,e4) -> if String.equal s s' then e2 else Exp.LetRec (s',alet e3,alet e4)
  | Exp.Fun (s',e) -> if String.equal s s' then e2 else Exp.Fun (s',alet e)
  | Exp.Int _ | Exp.Float _ | Exp.Bool _ | Exp.String _ | Exp.Unit -> e2
  | Exp.Not e -> Exp.Not (alet e)
  | Exp.If (e3,e4,e5) -> Exp.If (alet e3,alet e4,alet e5)
  | Exp.And (e3,e4) -> Exp.And (alet e3,alet e4)
  | Exp.Or (e3,e4) -> Exp.Or (alet e3,alet e4)
  | Exp.IAdd (e3,e4) -> Exp.IAdd (alet e3,alet e4)
  | Exp.ISub (e3,e4) -> Exp.ISub (alet e3,alet e4)
  | Exp.IMul (e3,e4) -> Exp.IMul (alet e3,alet e4)
  | Exp.IDiv (e3,e4) -> Exp.IDiv (alet e3,alet e4)
  | Exp.FAdd (e3,e4) -> Exp.FAdd (alet e3,alet e4)
  | Exp.FSub (e3,e4) -> Exp.FSub (alet e3,alet e4)
  | Exp.FMul (e3,e4) -> Exp.FMul (alet e3,alet e4)
  | Exp.FDiv (e3,e4) -> Exp.FDiv (alet e3,alet e4)
  | Exp.Eq (e3,e4) -> Exp.Eq (alet e3,alet e4)
  | Exp.Ne (e3,e4) -> Exp.Ne (alet e3,alet e4)
  | Exp.Lt (e3,e4) -> Exp.Lt (alet e3,alet e4)
  | Exp.Gt (e3,e4) -> Exp.Gt (alet e3,alet e4)
  | Exp.Le (e3,e4) -> Exp.Le (alet e3,alet e4)
  | Exp.Ge (e3,e4) -> Exp.Ge (alet e3,alet e4)
  | Exp.Apply (e3,e4) -> Exp.Apply (alet e3,alet e4)
  | Exp.Annot (e,t) -> Exp.Annot(alet e,t)
  | Exp.Tuple xs -> Exp.Tuple (map alet xs)
;;

let rec inferC env e =
  match e with
  | Exp.Int _ -> (Type.Int,[])
  | Exp.Float _ -> (Type.Float,[])
  | Exp.Bool _ -> (Type.Bool,[])
  | Exp.String _ -> (Type.String,[])
  | Exp.Not e ->
      let (t,c) = inferC env e in
      let c' = (t,Type.Bool)::c in
      (Type.Bool,c')
  | Exp.If (e1,e2,e3) ->
      let (t1,c1) = inferC env e1 in
      let (t2,c2) = inferC env e2 in
      let (t3,c3) = inferC env e3 in
      let c = (t1,Type.Bool)::(t2,t3)::(c1 @ c2 @ c3) in
      (t2,c)
  | Exp.Let (s,e1,e2) ->
      let e2 = assign_let (s,e1) e2 in
      let _ = inferC env e1 in
      inferC env e2
  | Exp.LetRec (s,e1,e2) ->
      let x = fresh() in
      let (t1,c) = inferC ((s,x)::env) e1 in
      let t1 = assign (unify ((x,t1)::c)) t1 in
      inferC ((s,t1)::env) e2
  | Exp.And (e1,e2) | Exp.Or (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC env e1,inferC env e2) in
      let c1 = (t1,Type.Bool)::c1 in
      let c2 = (t2,Type.Bool)::c2 in
      (Type.Bool,c1@c2)
  | Exp.IAdd (e1,e2) | Exp.ISub (e1,e2) | Exp.IMul (e1,e2) | Exp.IDiv (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC env e1,inferC env e2) in
      let c1 = (t1,Type.Int)::c1 in
      let c2 = (t2,Type.Int)::c2 in
      (Type.Int,c1@c2)
  | Exp.FAdd (e1,e2) | Exp.FSub (e1,e2) | Exp.FMul (e1,e2) | Exp.FDiv (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC env e1,inferC env e2) in
      let c1 = (t1,Type.Float)::c1 in
      let c2 = (t2,Type.Float)::c2 in
      (Type.Float,c1@c2)
  | Exp.Eq (e1,e2) | Exp.Ne (e1,e2) | Exp.Lt (e1,e2) | Exp.Gt (e1,e2)
  | Exp.Le (e1,e2) | Exp.Ge (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC env e1,inferC env e2) in
      let c = (t1,t2)::(c1@c2)  in
      (Type.Bool,c)
  | Exp.Fun (s,e) ->
      let t1 = fresh() in
      let env = (s,t1)::env in
      let (t2,c) = inferC env e in
      (Type.Fun (t1,t2),c)
  | Exp.Var s ->
      (lookup env s,[])
  | Exp.Apply (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC env e1,inferC env e2) in
      let x = fresh() in
      let c' = (t1,Type.Fun (t2,x))::c1@c2 in
      (x,c')
  | Exp.Annot (e,t) ->
      let (t',c) = inferC env e in
      let c = (t,t')::c in
      (t,c)
  | Exp.Unit -> (Unit,[])
  | Exp.Tuple xs ->
      let tcs = map (fun e -> inferC env e) xs in
      let c = fold_left (fun x (_,y) -> x@y) [] tcs in
      let ts = map (fun (t,_) -> assign (unify c) t) tcs in
      (Type.Tuple ts,c)
;;

let infer e =
  let (t,c) = inferC Builtin.builtin_tenv e in
  assign (unify c) t
;;
