open Exp
open Type
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
  | Fun (t1,t2) -> Fun (assign s t1,assign s t2)
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
      | (Var _,_) -> Compose ((unify(assignC (Set [(s,t)]) c')),Set [(s,t)])
      | (_,Var _) -> Compose ((unify(assignC (Set [(t,s)]) c')),Set [(t,s)])
      | (Fun (s1,s2),Fun (t1,t2)) -> unify((s1,t1)::(s2,t2)::c')
      | (_,_) -> raise (UnificationError (s,t))
;;

let rec lookupG gamma s =
  match gamma with
  | [] -> raise (UnknownVariableError s)
  | (s',t)::gamma' -> if String.equal s s' then t
                      else lookupG gamma' s
;;

let rec inferC gamma c e =
  match e with
  | Exp.Int _ -> Type.Int
  | Exp.IAdd (e1,e2) | Exp.ISub (e1,e2) ->
      let (t1,t2) = (inferC gamma c e1,inferC gamma c e2) in
      let c' = (t1,Type.Int)::(t2,Type.Int)::c in
      let asgn = unify(c') in
      if assign asgn t1 == assign asgn t2 then Type.Int
      else raise (InferenceError e)
  | Exp.Lambda (s,e) ->
      let t1 = fresh() in
      let gamma' = (s,t1)::gamma in
      let t2 = inferC gamma' c e in
      Fun (t1,t2)
  | Exp.Var s ->
      lookupG gamma s
  | _ -> fresh()
;;

let infer e = inferC [] [] e ;;
