open List

type assign =
  | Set of (Type.typ * Type.typ) list
  | Compose of assign * assign
;;

exception UnificationError of Type.typ * Type.typ ;;
exception InferenceError of Exp.exp ;;
exception UnknownVariableError of string ;;

let x = ref 0 ;;

let fresh =
  fun () ->
    x := !x+1;
    Type.Var !x
;;

let rec assign s t =
  match t with
  | Type.Fun (t1,t2) -> Type.Fun (assign s t1,assign s t2)
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
      | (_,_) -> raise (UnificationError (s,t))
;;

let rec lookupG gamma s =
  match gamma with
  | [] -> raise (UnknownVariableError s)
  | (s',t)::gamma' -> if String.equal s s' then t
                      else lookupG gamma' s
;;

let rec inferC gamma e =
  match e with
  | Exp.Int _ -> (Type.Int,[])
  | Exp.Float _ -> (Type.Float,[])
  | Exp.IAdd (e1,e2) | Exp.ISub (e1,e2) | Exp.IMul (e1,e2) | Exp.IDiv (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC gamma e1,inferC gamma e2) in
      let c1 = (t1,Type.Int)::c1 in
      let c2 = (t2,Type.Int)::c2 in
      (Type.Int,c1@c2)
  | Exp.FAdd (e1,e2) | Exp.FSub (e1,e2) | Exp.FMul (e1,e2) | Exp.FDiv (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC gamma e1,inferC gamma e2) in
      let c1 = (t1,Type.Float)::c1 in
      let c2 = (t2,Type.Float)::c2 in
      (Type.Float,c1@c2)
  | Exp.Fun (s,e) ->
      let t1 = fresh() in
      let gamma' = (s,t1)::gamma in
      let (t2,c) = inferC gamma' e in
      let t1 = assign (unify c) t1 in
      (Type.Fun (t1,t2),c)
  | Exp.Var s ->
      (lookupG gamma s,[])
  | Exp.Apply (e1,e2) ->
      let ((t1,c1),(t2,c2)) = (inferC gamma e1,inferC gamma e2) in
      let x = fresh() in
      let c' = (t1,Type.Fun (t2,x))::c1@c2 in
      (x,c')
;;

let infer e =
  let (t,c) = inferC [] e in
  assign (unify c) t
;;
