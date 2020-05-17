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

let occurence_error x t =
  Printf.eprintf "%s occurs in %s\n" (Type.type_to_string x) (Type.type_to_string t);
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

let rec occur x t =
  let i =
    match x with
    | Type.Var i -> i
    | _ -> Printf.eprintf "the first argument of occur should be type variable";
           exit 1
  in
  let f _ y = occur x y in
  match t with
  | Type.Var j -> if i==j then occurence_error x t
             else ()
  | Type.Int | Type.Float | Type.Bool | Type.String | Type.Unit -> ()
  | Type.Fun (t1,t2) -> occur x t1; occur x t2
  | Type.Tuple ts -> List.fold_left f () ts
;;

let rec unify c =
  match c with
  | [] -> Set []
  | (s,t)::c' ->
      if s==t then unify c'
      else match (s,t) with
      (* 出現検査をしていない *)
      | (Type.Var _,_) ->
          occur s t;
          Compose ((unify(assignC (Set [(s,t)]) c')),Set [(s,t)])
      | (_,Type.Var _) ->
          occur t s;
          Compose ((unify(assignC (Set [(t,s)]) c')),Set [(t,s)])
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

let rec inferC te exp =
  match exp with
  | Exp.Int _ -> (Type.Int,[])
  | Exp.Float _ -> (Type.Float,[])
  | Exp.Bool _ -> (Type.Bool,[])
  | Exp.String _ -> (Type.String,[])
  | Exp.Not e ->
      let (t,c) = inferC te e in
      let c' = (t,Type.Bool)::c in
      (Type.Bool,c')
  | Exp.If (e1,e2,e3) ->
      let (t1,c1) = inferC te e1 in
      let (t2,c2) = inferC te e2 in
      let (t3,c3) = inferC te e3 in
      let c = (t1,Type.Bool)::(t2,t3)::(c1 @ c2 @ c3) in
      (t2,c)
  | Exp.Let (s,e1,e2) ->
      let x = fresh() in
      let (t1,c1) = inferC ((s,x)::te) e1 in
      let (t2,_) = inferC ((s,t1)::te) e2 in
      (t2,c1)
  | Exp.LetRec (s,e1,e2) ->
      let x = fresh() in
      let (t1,c1) = inferC ((s,x)::te) e1 in
      let (t2,c2) = inferC ((s,x)::te) e2 in
      (t2,(x,t1)::c1@c2)
  | Exp.And (e1,e2) | Exp.Or (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC te e1,inferC te e2) in
      let c1 = (t1,Type.Bool)::c1 in
      let c2 = (t2,Type.Bool)::c2 in
      (Type.Bool,c1@c2)
  | Exp.IAdd (e1,e2) | Exp.ISub (e1,e2) | Exp.IMul (e1,e2) | Exp.IDiv (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC te e1,inferC te e2) in
      let c1 = (t1,Type.Int)::c1 in
      let c2 = (t2,Type.Int)::c2 in
      (Type.Int,c1@c2)
  | Exp.FAdd (e1,e2) | Exp.FSub (e1,e2) | Exp.FMul (e1,e2) | Exp.FDiv (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC te e1,inferC te e2) in
      let c1 = (t1,Type.Float)::c1 in
      let c2 = (t2,Type.Float)::c2 in
      (Type.Float,c1@c2)
  | Exp.Eq (e1,e2) | Exp.Ne (e1,e2) | Exp.Lt (e1,e2) | Exp.Gt (e1,e2)
  | Exp.Le (e1,e2) | Exp.Ge (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC te e1,inferC te e2) in
      let c = (t1,t2)::(c1@c2)  in
      (Type.Bool,c)
  | Exp.Fun (s,e) ->
      let t1 = fresh() in
      let te = (s,t1)::te in
      let (t2,c) = inferC te e in
      (Type.Fun (t1,t2),c)
  | Exp.Var s ->
      (lookup te s,[])
  | Exp.Apply (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC te e1,inferC te e2) in
      let x = fresh() in
      let c' = (t1,Type.Fun (t2,x))::c1@c2 in
      (x,c')
  | Exp.Annot (e,t) ->
      let (t',c) = inferC te e in
      let c = (t,t')::c in
      (t,c)
  | Exp.Unit -> (Unit,[])
  | Exp.Tuple xs ->
      let tcs = map (fun e -> inferC te e) xs in
      let c = fold_left (fun x (_,y) -> x@y) [] tcs in
      let ts = map (fun (t,_) -> assign (unify c) t) tcs in
      (Type.Tuple ts,c)
;;

let infer e =
  let (t,c) = inferC Builtin.builtin_tenv e in
  assign (unify c) t
;;
